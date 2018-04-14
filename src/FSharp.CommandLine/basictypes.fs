[<AutoOpen>]
module FSharp.CommandLine.BasicTypes

type Args = string list

[<Struct>]
type HelpText = {
    usage: string * string list
    description: string
    subcommands: (string * string) list
    options: (string * string) list
    additionalLines: string list
  }

let inline internal emptyHelp () = { usage=("", []); description=""; subcommands=[]; options=[]; additionalLines=[] }

type CommandSuggestion =
  | Values of string list
  | ValuesWithDescription of (string * string) list
  | Files of pattern: string option
  | Directories of pattern: string option
  | OptionSuggestion of (string list) * string
  | Message of string

type CommandOptionSummary = { 
    names: string list;
    description: string;
    isFlag: bool;
    paramNames: (string list) list
    genSuggestions: string option -> CommandSuggestion list 
  }
  with
    member this.Param =
      let rec print eq xss =
        let heads = xss |> List.map List.tryHead
        if heads |> List.exists Option.isSome then
          let isOptional = heads |> List.exists Option.isNone
          let groups = xss |> List.filter (List.isEmpty >> not)
                           |> List.groupBy List.head
                           |> List.map (fun (g, xs) -> sprintf "%s%s" g (xs |> List.map List.tail |> print ""))
          let s =
            if List.length groups > 1 then
              let s = groups |> String.concat "|"
              if isOptional then s else sprintf "{%s}" s
            else
              groups |> String.concat ""
          if isOptional then sprintf "[%s%s]" eq s else sprintf "%s%s"eq s
        else
          ""
      in print "=" (this.paramNames |> List.rev)

    member this.NameRepresentations =
      this.names
        |> List.map (function
              | x when x.Length = 1 -> 
                [ "-"; "/" ] |> List.map (fun prefix -> sprintf "%s%s" prefix x)
              | x -> 
                [ "--"; "/" ] |> List.map (fun prefix -> sprintf "%s%s" prefix x)
            )
        |> List.concat

    member this.Print () =
      let o x = if this.isFlag then sprintf "%s[+|-]" x else sprintf "%s%s" x this.Param
      ((this.NameRepresentations |> List.filter (String.startsWith "-") |> String.concat ", " |> o), this.description)

[<Struct>]
type CommandSummary = {
    name: string;
    description: string;
    paramNames: (string list) option
    genSuggestions: Args -> CommandSuggestion list
  }

[<Literal>]
let internal runBeforePreprocess = "----run-before-preprocess-then-fail"

type ICommand<'a> =
  abstract member Run: Args -> ('a * Args)
  abstract member Summary: unit -> CommandSummary
  abstract member Subcommands: unit -> ICommand<int> list
  abstract member Options: unit -> CommandOptionSummary list
