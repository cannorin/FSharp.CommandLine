module rec FSharp.CommandLine.Options
open FSharp.CommandLine.Scanf
open System.Runtime.CompilerServices

open CommandOption

/// specify how to treat options like ```-abcd```
type SingleHyphenStyle = 
  /// treat ```-abcd``` as ```--abcd```
  | SingleLong
  /// treat ```-abcd``` as ```-a bcd```
  | SingleShort
  /// treat ```-abcd``` as ```-a -b -c -d```
  | MergedShort

[<Struct>]
type CommandOptionNoArgProvided<'a> =
  | UseDefault of 'a
  | JustFail

[<Struct>]
type CommandOptionKind<'a> =
  | Flag of (bool -> 'a)
  | TakingValueWith of CommandOptionNoArgProvided<'a> * (string -> 'a option) list

type ICommandOption<'a> =
  abstract member Parse: string list -> ('a * string list)
  abstract member Config: CommandInfo -> CommandInfo
  abstract member Summary: CommandOptionSummary

[<Struct>]
type CommandOption<'a> = {
    baseSummary: CommandOptionSummary
    kind: CommandOptionKind<'a>
    style: SingleHyphenStyle
  }
  with
    member this.Summary =
      let self = this
      {
        this.baseSummary with
          isMatch =
            fun argv ->
              match (parse self argv) with
                | (Some _, rem) -> Some rem
                | (None, _) -> None
      }
    member this.Parse argv = parse this argv
    interface ICommandOption<'a option> with
      member this.Summary = this.Summary
      member this.Parse argv = parse this argv
      member this.Config cfg =
        {
          cfg with
            options = this.Summary :: cfg.options
        }

let inline private defaultCO () = 
  { 
    baseSummary = 
      {
        names = [];
        description = "";
        isFlag = false;
        paramNames = [];
        isMatch = fun _ -> None
        genSuggestions = (fun _ -> [])
      };
    kind = TakingValueWith (JustFail, []);
    style = MergedShort
  }

let inline private defaultCF () =
  { 
    baseSummary = 
      {
        names = [];
        description = "";
        isFlag = true;
        paramNames = [];
        isMatch = fun _ -> None
        genSuggestions = (fun _ -> []) 
      };
    kind = Flag id;
    style = MergedShort
  }


module ReservedCommandOptions =
  let helpOption = 
    { 
      baseSummary =
        {
          names = ["?"; "h"; "help"]
          description = "display this help usage."
          isFlag = false
          paramNames = []
          isMatch = fun _ -> None
          genSuggestions = (fun _ -> [])
        }
      kind = Flag id
      style = MergedShort
    }
  
  let suggestOption =
    {
      baseSummary =
        {
          names = ["generate-suggestions"; "generate-suggestions-incomplete"]
          description = ""
          isFlag = false
          paramNames = [["name"]]
          isMatch = fun _ -> None
          genSuggestions = (fun _ -> [])
        }
      kind = TakingValueWith (UseDefault "zsh", [Some])
      style = MergedShort
    }

[<Struct>]
type CommandOptionBuilder<'a>(dc: unit -> CommandOption<'a>) =
  member __.For (_, _) = failwith "Not supported"
  member __.Yield _ = dc ()
  [<CustomOperation("names")>]
  member __.Names (co, x) = { co with baseSummary = { co.baseSummary with names = x } }
  [<CustomOperation("description")>]
  member __.Description (co, x) = { co with baseSummary = { co.baseSummary with description = x } }
  [<CustomOperation("takes")>]
  member inline __.Takes (co: CommandOption<'a>, x) =
    let (f, ts) = construct x in
    { co with 
        kind =
          match co.kind with
            | TakingValueWith (d, xs) -> TakingValueWith (d, List.append xs [f])
            | _                       -> TakingValueWith (JustFail, [f]);
        baseSummary =
          { co.baseSummary with
              paramNames = ts :: co.baseSummary.paramNames
          }
    }
  [<CustomOperation("defaultValue")>]
  member __.DefaultValue (co: CommandOption<'a>, value: 'a) =
    { co with
        kind =
          match co.kind with
            | TakingValueWith (_, xs) -> TakingValueWith (UseDefault value, xs)
            | x -> x
    }
  [<CustomOperation("suggests")>]
  member __.Suggests (co, f) = { co with baseSummary = { co.baseSummary with genSuggestions=f } }
  [<CustomOperation("style")>]
  member __.Style (co, st) = { co with style = st }

let commandOption<'a> = CommandOptionBuilder<'a> defaultCO
let commandFlag = CommandOptionBuilder defaultCF

type Command =
  /// short-form definition of command option
  static member inline option (_names, _format, ?_descr, ?defVal, ?_style) =
    let mutable co =
      commandOption {
        names _names
        takes (format _format)
        description (_descr ?| "")
        style (_style ?| SingleHyphenStyle.MergedShort)
      }
    defVal |> Option.iter (fun x -> co <- commandOption<_>.DefaultValue(co, x))
    co
  
  /// short-form definition of command flag
  static member inline flag (_names, ?_descr, ?_style) =
    commandFlag {
      names _names
      description (_descr ?| "")
      style (_style ?| SingleHyphenStyle.MergedShort)
    }

type private RefinedToken =
  | RFlag of string
  | RFlagDisable of string
  | RFlagAndValue of string * string
  | RMaybeCombinedFlag of string
  | RMaybeCombinedFlagAndValue of string * string
  | RValue of string
  | RIgnoreAfter
  with
    override this.ToString() =
      match this with
        | RFlag s -> sprintf "--%s" s
        | RFlagDisable s -> sprintf "-%s-" s
        | RFlagAndValue (s, v) -> sprintf "--%s=%s" s v
        | RMaybeCombinedFlag s -> sprintf "-%s" s
        | RMaybeCombinedFlagAndValue (s, v) -> sprintf "-%s=%s" s v
        | RValue s -> s
        | RIgnoreAfter -> "--"

let private optionForms = [
    tryKscanf "--" (fun () -> RIgnoreAfter)
    tryKscanf "-%c" (RFlag << to_s);
    tryKscanf "-%c=%s" (Tuple.map2 to_s id >> RFlagAndValue);
    tryKscanf "--%s=%s" RFlagAndValue;
    tryKscanf "/%s=%s" RFlagAndValue;
    tryKscanf "--%s" RFlag;
    tryKscanf "-%c+" (RFlag << to_s);
    tryKscanf "-%c-" (RFlagDisable << to_s);
    tryKscanf "-%s=%s" RMaybeCombinedFlagAndValue;
    tryKscanf "-%s" RMaybeCombinedFlag;
    tryKscanf "/%s" RFlag;
    tryKscanf "%s" RValue
  ]

let rec private tokenize argv =
  seq {
    if List.isEmpty argv then
      yield! Seq.empty
    else
      let (h, t) = (List.head argv, List.tail argv) in
      let ro = optionForms |> List.map (fun f -> f h)
                           |> List.choose (function Ok x -> Some x | _ -> None)
                           |> List.tryHead in
      if Option.isSome ro then
        yield ro.Value;
        yield!
          match ro.Value with
            | RIgnoreAfter -> t |> Seq.map RValue
            | _ -> tokenize t
      else
        yield! Seq.empty
  }

module CommandOption =
  let getRemainingOptions argv =
    argv |> tokenize |> List.ofSeq
         |> List.choose (function RIgnoreAfter | RValue _ -> None | x -> Some x)
         |> List.map to_s

  let parse (opt: CommandOption<'a>) argv =
    let inline isSingle s = String.length s = 1 in
    let inline matches x = opt.baseSummary.names |> List.contains x in
    let shortNames = opt.baseSummary.names |> List.filter isSingle in
    let opf msg = OptionParseFailed(opt.baseSummary, msg)

    let tokens = tokenize argv |> List.ofSeq in
    let rec find ts =
      match opt.kind with
        | Flag f ->
          let inline f x = f x |> Some in
          match ts with
            | RFlag x :: rest when matches x -> (f true, rest)
            | RFlagDisable x :: rest when matches x -> (f false, rest)
            | RFlagAndValue (x, _) :: _ when matches x ->
              sprintf "'%s' is a flag and does not take an argument" x |> opf |> raise
            | RMaybeCombinedFlag xs :: rest & x :: _ ->
              match opt.style with
                | MergedShort when shortNames |> List.exists xs.Contains ->
                  let c = shortNames |> List.find xs.Contains in
                  (f true, RMaybeCombinedFlag (xs.Replace(c, "")) :: rest)
                | SingleLong  when matches xs -> (f true, rest)
                | SingleShort when shortNames |> List.exists xs.StartsWith ->
                  sprintf "'%c' is a flag and does not take an argument" (xs.[0]) |> opf |> raise
                | _ -> find rest |> Tuple.map2 id (fun rest' -> x :: rest')
            | RMaybeCombinedFlagAndValue (xs, v) :: rest & x :: _ ->
              match opt.style with
                | MergedShort when shortNames |> List.exists xs.Contains ->
                  let c = shortNames |> List.find xs.Contains in
                  if shortNames |> List.exists xs.EndsWith then
                    sprintf "'%s' is a flag and does not take an argument" c |> opf |> raise
                  else 
                    (f true, RMaybeCombinedFlagAndValue(xs.Replace(c, ""), v) :: rest)
                | SingleLong when matches xs ->
                    sprintf "'%s' is a flag and does not take an argument" xs |> opf |> raise
                | SingleShort -> sprintf "invalid option: '-%s=%s'" xs v |> opf |> raise
                | _ -> find rest |> Tuple.map2 id (fun rest' -> x :: rest')
            | x :: rest -> find rest |> Tuple.map2 id (fun rest' -> x :: rest')
            | [] -> (None, [])
        | TakingValueWith (na, fs) ->
          let inline tryReturn v name =
            match (fs |> List.map (fun f -> f v) |> List.choose id |> List.tryHead) with
              | Some x -> Some x
              | None -> 
                sprintf "the value '%s' is invalid for the option '%s'" v name |> opf |> raise
          in
          let inline tryDefault name =
            match na with
              | UseDefault x -> Some x
              | JustFail -> 
                sprintf "a value is missing for the option '%s'" name |> opf |> raise
          in
          match ts with
            | RFlag x :: RValue v :: rest
            | RFlagAndValue (x, v) :: rest when matches x -> (tryReturn v x, rest)
            | RFlag x :: rest when matches x -> (tryDefault x, rest)
            | RFlagDisable x :: _ when matches x ->
              sprintf "a value is missing for the option '%s'" x |> opf |> raise
            | RMaybeCombinedFlag xs :: RValue v :: rest & x :: _ :: _->
              match opt.style with
                | MergedShort when shortNames |> List.exists xs.EndsWith ->
                  let c = shortNames |> List.find xs.EndsWith in
                  (tryReturn v c, RMaybeCombinedFlag (xs.Replace(c, "")) :: rest)
                | MergedShort when shortNames |> List.exists xs.Contains ->
                  let c = shortNames |> List.find xs.Contains in
                  (tryDefault c, RMaybeCombinedFlag (xs.Replace(c, "")) :: RValue v :: rest)
                | SingleShort when shortNames |> List.exists xs.StartsWith ->
                  let c = shortNames |> List.find xs.StartsWith in
                  let v' = xs.Substring(1) in
                  (tryReturn v' c, RValue v :: rest)
                | SingleLong when matches xs ->
                  (tryReturn v xs, rest)
                | _ -> find rest |> Tuple.map2 id (fun rest' -> x :: RValue v :: rest')
            | RMaybeCombinedFlagAndValue (xs, v) :: rest & x :: _ ->
              match opt.style with
                | MergedShort when shortNames |> List.exists xs.EndsWith ->
                  let c = shortNames |> List.find xs.EndsWith in
                  (tryReturn v c, RMaybeCombinedFlag (xs.Replace(c, "")) :: rest)
                | MergedShort when shortNames |> List.exists xs.Contains ->
                  let c = shortNames |> List.find xs.Contains in
                  (tryDefault c, RMaybeCombinedFlagAndValue (xs.Replace(c, ""), v) :: rest)
                | SingleLong when matches xs ->
                  (tryReturn v xs, rest)
                | SingleShort -> sprintf "invalid option: '-%s=%s'" xs v |> opf |> raise
                | _ -> find rest |> Tuple.map2 id (fun rest' -> x :: rest')
            | x :: rest -> find rest |> Tuple.map2 id (fun rest' -> x :: rest')
            | [] -> (None, [])
    in
    find tokens |> Tuple.map2 id (List.map to_s)

  let parseMany (opt: #ICommandOption<_>) argv =
    let rec p xs =
      seq  {
        yield!
          match (opt.Parse xs) with
            | (Some x, rest) ->
              seq { yield (x, rest); yield! p rest }
            | (None, _) ->
              Seq.empty
      }
    in
    let x = p argv in
    (x |> Seq.map fst |> List.ofSeq, x |> Seq.map snd |> Seq.tryLast ?| argv)

let inline (|OptionParse|_|) opt argv =
  let (reso, argv') = parse opt argv
  reso |> Option.map (fun x -> (x, argv'))