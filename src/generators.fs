module FSharp.CommandLine.Generators
open System

let private getCommandInfo (cmd: #ICommand<_>) =
  try
    cmd.Run [runBeforePreprocess] |> ignore
    (cmd.Summary(), cmd.Subcommands(), cmd.Options())
  with
    | RaiseInfo (x, y, z, _) -> (x, y, z)


module Help =
  let generate (cmd: #ICommand<_>) =
    seq {
      let (smry, scs, opts) = getCommandInfo cmd

      let prettySprint a b =
        let indent = "                                 "
        let indentLen = String.length indent
        let aLen = String.length a
        if aLen > indentLen then
          seq {
            yield sprintf " %s" a
            yield sprintf " %s %s" indent b
          }
        else
          seq { yield sprintf " %s %s" a (String.replicate (indentLen - aLen) " " |> sprintf "%s%s" <| b) }
      
      let genParamNames pns subs options = 
        match pns with
          | Some xs -> xs |> String.concat " "
          | None ->
            match (List.isEmpty subs, List.isEmpty options) with
              | (true, true) -> ""
              | (true, false) -> "[options]"
              | (false, true) -> "<command>"
              | (false, false) -> "<command> [options]"

      yield sprintf "usage: %s %s" smry.name (genParamNames smry.paramNames scs opts)
      yield ""
      yield smry.description
      yield ""
      if List.isEmpty scs |> not then
        yield "commands:"
        for sc in scs do
          let (scsmry, scsubs, scopts) = getCommandInfo sc
          yield! prettySprint (sprintf "%s %s" scsmry.name (genParamNames scsmry.paramNames scsubs scopts)) scsmry.description
        yield ""
      if List.isEmpty opts |> not then
        yield "options:"
        for opt in opts do
          let (pr, desc) = opt.Print()
          yield! prettySprint pr desc
    }

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
