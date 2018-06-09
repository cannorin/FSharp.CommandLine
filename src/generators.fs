module FSharp.CommandLine.Generators
open System

let private getCommandInfo (cmd: #ICommand<_>) =
  try
    cmd.Run [runBeforePreprocess] |> ignore
    (cmd.Summary(), cmd.Subcommands(), cmd.Options())
  with
    | RaiseInfo (x, y, z, _) -> (x, y, z)

[<Struct>]
type HelpBuilder =
  member inline __.For (_, _) = failwith "Not supported"
  member inline __.Yield _ : HelpElement seq = Seq.empty
  [<CustomOperation("defaultUsage")>]
  member inline __.Usage xs = xs |> Seq.snoc HelpUsage
  [<CustomOperation("customUsage")>]
  member inline __.UsageWithCustomArgs (xs, argNames) = xs |> Seq.snoc (HelpUsageCustomArgs argNames)
  [<CustomOperation("text")>]
  member inline __.RawText (xs, str) = xs |> Seq.snoc (HelpRawString str)
  [<CustomOperation("allSubcommands")>]
  member inline __.Subcommands xs = xs |> Seq.snoc HelpAllSubcommands
  [<CustomOperation("specificSubcommands")>]
  member inline __.SpecificSubcommands (xs, cmds) = xs |> Seq.snoc (HelpSpecificSubcommands cmds)
  [<CustomOperation("allOptions")>]
  member inline __.Options xs = xs |> Seq.snoc HelpAllOptions
  [<CustomOperation("specificOptions")>]
  member inline __.SpecificOptions (xs, opts) = xs |> Seq.snoc (HelpSpecificOptions opts)
  [<CustomOperation("section")>]
  member inline __.Section (xs, sectionName, section) = xs |> Seq.snoc (HelpSection(sectionName, section))
  [<CustomOperation("emptyLine")>]
  member inline __.EmptyLine xs = xs |> Seq.snoc HelpEmptyLine

let helpText = HelpBuilder ()

module Help =
  let private prettySprint a b =
    let indent = "                                 "
    let indentLen = String.length indent
    let aLen = String.length a
    if aLen > indentLen then
      seq {
        yield sprintf "%s" a
        yield sprintf "%s %s" indent b
      }
    else
      seq { yield sprintf "%s %s" a (String.replicate (indentLen - aLen) " " |> sprintf "%s%s" <| b) }

  let private genParamNames pns subs options = 
    match pns with
      | Some xs -> xs |> String.concat " "
      | None ->
        match (List.isEmpty subs, List.isEmpty options) with
          | (true, true) -> ""
          | (true, false) -> "[options]"
          | (false, true) -> "<command>"
          | (false, false) -> "[options] <command>"

  let interpret (generator: #ICommand<_> -> #seq<HelpElement>) (cmd: #ICommand<_>) =
    let (smry, scs, opts) = getCommandInfo cmd
    let dn = smry.displayName ?| smry.name
    let rec print elems =
      seq {
        for elem in elems do
          yield!
            match elem with
              | HelpUsage -> seq [sprintf "usage: %s %s" dn (genParamNames smry.paramNames scs opts)]
              | HelpUsageCustomArgs args -> seq [sprintf "usage: %s %s" dn (args |> String.concat " ")]
              | HelpRawString txt -> seq [txt]
              | HelpAllSubcommands ->
                seq {
                  if List.isEmpty scs |> not then
                    for sc in scs do
                      let (scsmry, scsubs, scopts) = getCommandInfo sc
                      yield! prettySprint (sprintf "%s %s" scsmry.name (genParamNames scsmry.paramNames scsubs scopts)) scsmry.description
                    yield ""
                }
              | HelpSpecificSubcommands names ->
                let scs' = scs |> List.filter (fun sc -> names |> List.contains (sc.Summary().name))
                seq {
                  if scs' |> List.isEmpty |> not then
                    for sc in scs' do
                      let (scsmry, scsubs, scopts) = getCommandInfo sc
                      yield! prettySprint (sprintf "%s %s" scsmry.name (genParamNames scsmry.paramNames scsubs scopts)) scsmry.description
                    yield ""
                }
              | HelpAllOptions ->
                seq {
                  if List.isEmpty opts |> not then
                    for opt in opts do
                      let (pr, desc) = opt.Print()
                      yield! prettySprint pr desc
                }
              | HelpSpecificOptions names ->
                let opts' = opts |> List.filter (fun opt -> names |> List.exists (fun name -> opt.names |> List.contains name))
                seq {
                  if List.isEmpty opts' |> not then
                    for opt in opts' do
                      let (pr, desc) = opt.Print()
                      yield! prettySprint pr desc
                }
              | HelpSection (name, bodies) ->
                seq {
                  yield sprintf "%s:" name
                  yield! print bodies |> Seq.map (fun line -> sprintf "  %s" line)
                }
              | HelpEmptyLine -> seq [""]

      }
    generator cmd |> print

  let defaultGenerator (cmd: #ICommand<_>) =
    let (smry, scs, opts) = getCommandInfo cmd
    helpText {
      defaultUsage
      emptyLine
      text smry.description
      emptyLine
      section "commands" (
        helpText { 
          allSubcommands
        }
      )
      emptyLine
      section "options" (
        helpText {
          allOptions
        }
      )
    }
  
  let generate cmd =
    let (smry, _, _) = getCommandInfo cmd
    match smry.help with
      | Some help -> interpret (fun _ -> help) cmd
      | None -> interpret defaultGenerator cmd

type ISuggestionBackend =
  abstract print: CommandSuggestion list -> string

module Suggestions =
  let print (backend: #ISuggestionBackend) css =
    backend.print css

  let generate args (cmd: #ICommand<_>) =
    List.ofSeq <|
      try
        (args @ [runBeforePreprocess]) |> cmd.Run |> printfn "%A"
        Seq.empty
      with
        | RaiseInfo (cmdsum, cmdsubs, cmdopts, remArgs) ->
          seq {
            for sub in cmdsubs do
              let (scsum, _, _) = getCommandInfo sub
              yield ValuesWithDescription [(scsum.name, scsum.description)]
            for opt in cmdopts do
              yield OptionSuggestion(opt.NameRepresentations, opt.description)
            yield!
              if (box cmdsum.genSuggestions <> null) then
                cmdsum.genSuggestions remArgs |> Seq.ofList
              else Seq.empty
          }
        | OptionParseFailed (opsum, _) ->
          seq {
            yield!
              if (box opsum.genSuggestions <> null) then
                args |> List.tryLast
                     |> Option.filter (String.startsWith "-" >> not)
                     |> Option.filter (String.startsWith "/" >> not)
                     |> Option.map (Some >> opsum.genSuggestions)
                     |> Option.defaultWith (fun () -> opsum.genSuggestions None)
                     |> Seq.ofList
              else Seq.empty
          }
        | CommandExecutionFailed msg -> seq { yield Message msg }

module SuggestionBackends =
  open FSharp.Collections

  let zsh =
    let quote str = sprintf "'%s'" str
    let escape str =
      str |> String.replace "'" "'\"'\"'"
          |> String.replace ":" "\\:"
          |> String.replace "\\" "\\\\"
    let conv = function
      | Values [] | ValuesWithDescription [] -> []
      | Values xs ->
          "_values" :: "-w" :: "'values'" :: (xs |> List.map (escape >> quote))
      | ValuesWithDescription xs ->
          "_values" :: "-w" :: "'values'" :: (xs |> List.map (fun (v, descr) -> sprintf "%s[%s]" v descr) 
                                                 |> List.map (escape >> quote))
      | Files (Some pattern) -> "_files" :: "-g" :: [pattern |> quote]
      | Files None -> ["_files"]
      | Directories (Some pattern) -> "_files" :: "-/" :: "-g" :: [pattern |> quote]
      | Directories None -> "_files" :: "-/" :: []
      | OptionSuggestion ([name], desc) ->
        "_arguments" :: (sprintf "'%s[%s]'" name (desc |> escape)) :: ["\"*: :->hoge\""]
      | OptionSuggestion (names, desc) when List.length names > 1 ->
        let names = names |> List.filter (String.startsWith "-")
        "_arguments" :: (sprintf "{%s}'[%s]'" (names |> String.concat ",") (desc |> escape)) :: ["\"*: :->hoge\""]
      | Message msg -> "_message" :: "-r" :: (msg |> escape |> quote) :: []
      | _ -> []
    {
      new ISuggestionBackend with
        member __.print css =
          css |> List.map conv
              |> List.filter (List.isEmpty >> not)
              |> List.groupBy List.head
              |> List.map (fun (gn, xss) ->
                    if gn = "_arguments" then
                      gn :: (xss |> List.map (fun xs -> xs.[1 .. List.length xs - 2]) |> List.concat) @ ["\"*: :->hoge\""]
                    else
                      gn :: (xss |> List.map (fun xs -> xs.[1 .. List.length xs - 1]) |> List.concat)
                  )
              |> List.map (String.concat " ")
              |> String.concat "; "
    }

  let mutable impls =
    Map.ofList [
      ("zsh", zsh)
    ]

  let findByName name =
    impls |> Map.tryFind name
          |> Option.defaultWith (fun () -> sprintf "suggestion backend not exist: %s" name |> CommandExecutionFailed |> raise)