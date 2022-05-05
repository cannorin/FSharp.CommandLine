namespace rec FSharp.CommandLine

open FSharp.Scanf
open OptionParser

/// specify how to treat options like ```-abcd```
type SingleHyphenStyle =
  /// treat ```-abcd``` as ```--abcd```
  | SingleLong
  /// treat ```-abcd``` as ```-a bcd```
  | SingleShort
  /// treat ```-abcd``` as ```-a -b -c -d```
  | MergedShort

type CommandOptionNoArgProvided<'a> =
  | UseDefault of 'a
  | JustFail

type CommandOptionKind<'a> =
  | Flag of (bool -> 'a)
  | TakingValueWith of CommandOptionNoArgProvided<'a> * (string -> 'a option) list

type ICommandOption<'a> =
  abstract member Parse: string list -> ('a * string list)
  abstract member Config: CommandInfo -> CommandInfo
  abstract member Summary: CommandOptionSummary

/// represents a command option parser that tries to parse the arguments
/// and returns an optional result value.
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
            match (parseImpl self argv) with
              | (Some _, rem) -> Some rem
              | (None, _) -> None
    }
  member this.Parse argv = parseImpl this argv

  static member internal defaultOption : CommandOption<'a> = {
    baseSummary = {
      names = []
      description = ""
      isFlag = false
      paramNames = []
      isMatch = (fun _ -> None)
      genSuggestions = (fun _ -> [])
    }
    kind = TakingValueWith (JustFail, [])
    style = MergedShort
  }

  static member internal defaultFlag : CommandOption<bool> = {
    baseSummary = {
      names = []
      description = ""
      isFlag = true
      paramNames = []
      isMatch = (fun _ -> None)
      genSuggestions = (fun _ -> [])
    }
    kind = Flag id
    style = MergedShort
  }

  interface ICommandOption<'a option> with
    member this.Summary = this.Summary
    member this.Parse argv = parseImpl this argv
    member this.Config cfg =
      {
        cfg with
          options = this.Summary :: cfg.options
      }

/// represents a command option of which behavior and/or functionality are augmented.
/// (e.g. can parse multiple occurrence of the option at once)
type AugmentedCommandOption<'a, 'b> = {
  orig: ICommandOption<'a>
  augmenter: ICommandOption<'a> -> Args -> ('b * Args)
}
with
  interface ICommandOption<'b> with
    member this.Summary = this.orig.Summary
    member this.Parse argv = this.augmenter this.orig argv
    member this.Config cfg = (this.orig :> ICommandOption<_>).Config cfg

module CommandOption =
  /// gets the arguments which look like (will potentially be recognized by the parser as)
  /// a command option.
  let getRemainingOptions argv =
    argv |> tokenize |> List.ofSeq
          |> List.choose (function RIgnoreAfter | RValue _ -> None | x -> Some x)
          |> List.map string

  /// given a command option and arguments, applies the parser to the arguments
  /// and returns the parsed result and the remaining arguments.
  let inline parse (opt: #ICommandOption<_>) argv = opt.Parse argv

  /// given a command option and arguments, applies the parser to the arguments until it fails
  /// and returns the results and the remaining arguments.
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
    let x = p argv
    (x |> Seq.map fst |> List.ofSeq, x |> Seq.map snd |> Seq.tryLast ?| argv)

  /// given a command option, augments its functionality by modifying
  /// its behavior. both the original command option and the arguments which
  /// will be passed at the execution time are available.
  let inline augment f c = { orig=c; augmenter=f }
  let inline map f c =
    let ac = augment (fun c args -> c.Parse args) c
    {
      orig=ac.orig;
      augmenter=(fun o x -> ac.augmenter o x |> Tuple.map2 f id)
    }

  /// given a command option, returns a new command option that parses
  /// zero or more occurrence of that command option.
  let inline zeroOrMore co =
    co |> augment parseMany

  /// given a command option, returns a new command option that fails
  /// if there are more than one occurrence of that command option.
  let inline zeroOrExactlyOne co =
    co
    |> zeroOrMore
    |> map (function
      | [] -> None
      | x :: [] -> Some x
      | _ ->
      let msg = sprintf "the option '%s' should be provided only once"
                        (co.Summary.NameRepresentations |> List.head)
      OptionParseFailed (co.Summary, msg) |> raise)

  /// given a command option, returns a new command option that returns
  /// the specified default value if there is no occurence.
  let inline whenMissingUse defaultValue co =
    co |> map (function Some x -> x | None -> defaultValue)

  /// an active pattern to parse the arguments using the command option
  /// and gets the result and the remaining arguments.
  let inline (|Parse|_|) opt argv =
    let (reso, argv') = parse opt argv
    reso |> Option.map (fun x -> (x, argv'))

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

module private OptionParser =
  type RefinedToken =
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

  let optionForms = [
      tryKsscanf "--" (fun () -> RIgnoreAfter)
      tryKsscanf "-%c" (RFlag << string);
      tryKsscanf "-%c=%s" (Tuple.map2 string id >> RFlagAndValue);
      tryKsscanf "--%s=%s" RFlagAndValue;
      tryKsscanf "/%s=%s" RFlagAndValue;
      tryKsscanf "--%s" RFlag;
      tryKsscanf "-%c+" (RFlag << string);
      tryKsscanf "-%c-" (RFlagDisable << string);
      tryKsscanf "-%s=%s" RMaybeCombinedFlagAndValue;
      tryKsscanf "-%s" RMaybeCombinedFlag;
      tryKsscanf "/%s" RFlag;
      tryKsscanf "%s" RValue
    ]

  let rec tokenize argv =
    seq {
      if List.isEmpty argv then
        yield! Seq.empty
      else
        let (h, t) = (List.head argv, List.tail argv)
        let ro = optionForms |> List.map (fun f -> f h)
                              |> List.choose (function Ok x -> Some x | _ -> None)
                              |> List.tryHead
        if Option.isSome ro then
          yield ro.Value;
          yield!
            match ro.Value with
              | RIgnoreAfter -> t |> Seq.map RValue
              | _ -> tokenize t
        else
          yield! Seq.empty
    }

  let parseImpl (opt: CommandOption<'a>) argv =
    let inline isSingle s = String.length s = 1
    let inline matches x = opt.baseSummary.names |> List.contains x
    let shortNames = opt.baseSummary.names |> List.filter isSingle
    let opf msg = OptionParseFailed(opt.baseSummary, msg)

    let tokens = tokenize argv |> List.ofSeq
    let rec find ts =
      match opt.kind with
        | Flag f ->
          let inline f x = f x |> Some
          match ts with
            | RFlag x :: rest when matches x -> (f true, rest)
            | RFlagDisable x :: rest when matches x -> (f false, rest)
            | RFlagAndValue (x, _) :: _ when matches x ->
              sprintf "'%s' is a flag and does not take an argument" x |> opf |> raise
            | RMaybeCombinedFlag xs :: rest & x :: _ ->
              match opt.style with
                | MergedShort when shortNames |> List.exists xs.Contains ->
                  let c = shortNames |> List.find xs.Contains
                  (f true, RMaybeCombinedFlag (xs.Replace(c, "")) :: rest)
                | SingleLong  when matches xs -> (f true, rest)
                | SingleShort when shortNames |> List.exists xs.StartsWith ->
                  sprintf "'%c' is a flag and does not take an argument" (xs.[0]) |> opf |> raise
                | _ -> find rest |> Tuple.map2 id (fun rest' -> x :: rest')
            | RMaybeCombinedFlagAndValue (xs, v) :: rest & x :: _ ->
              match opt.style with
                | MergedShort when shortNames |> List.exists xs.Contains ->
                  let c = shortNames |> List.find xs.Contains
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
          let inline tryDefault name =
            match na with
              | UseDefault x -> Some x
              | JustFail ->
                sprintf "a value is missing for the option '%s'" name |> opf |> raise
          match ts with
            | RFlag x :: RValue v :: rest
            | RFlagAndValue (x, v) :: rest when matches x -> (tryReturn v x, rest)
            | RFlag x :: rest when matches x -> (tryDefault x, rest)
            | RFlagDisable x :: _ when matches x ->
              sprintf "a value is missing for the option '%s'" x |> opf |> raise
            | RMaybeCombinedFlag xs :: RValue v :: rest & x :: _ :: _->
              match opt.style with
                | MergedShort when shortNames |> List.exists xs.EndsWith ->
                  let c = shortNames |> List.find xs.EndsWith
                  (tryReturn v c, RMaybeCombinedFlag (xs.Replace(c, "")) :: rest)
                | MergedShort when shortNames |> List.exists xs.Contains ->
                  let c = shortNames |> List.find xs.Contains
                  (tryDefault c, RMaybeCombinedFlag (xs.Replace(c, "")) :: RValue v :: rest)
                | SingleShort when shortNames |> List.exists xs.StartsWith ->
                  let c = shortNames |> List.find xs.StartsWith
                  let v' = xs.Substring(1)
                  (tryReturn v' c, RValue v :: rest)
                | SingleLong when matches xs ->
                  (tryReturn v xs, rest)
                | _ -> find rest |> Tuple.map2 id (fun rest' -> x :: RValue v :: rest')
            | RMaybeCombinedFlagAndValue (xs, v) :: rest & x :: _ ->
              match opt.style with
                | MergedShort when shortNames |> List.exists xs.EndsWith ->
                  let c = shortNames |> List.find xs.EndsWith
                  (tryReturn v c, RMaybeCombinedFlag (xs.Replace(c, "")) :: rest)
                | MergedShort when shortNames |> List.exists xs.Contains ->
                  let c = shortNames |> List.find xs.Contains
                  (tryDefault c, RMaybeCombinedFlagAndValue (xs.Replace(c, ""), v) :: rest)
                | SingleLong when matches xs ->
                  (tryReturn v xs, rest)
                | SingleShort -> sprintf "invalid option: '-%s=%s'" xs v |> opf |> raise
                | _ -> find rest |> Tuple.map2 id (fun rest' -> x :: rest')
            | x :: rest -> find rest |> Tuple.map2 id (fun rest' -> x :: rest')
            | [] -> (None, [])
    find tokens |> Tuple.map2 id (List.map string)
