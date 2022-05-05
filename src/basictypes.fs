namespace FSharp.CommandLine

open FSharp.CommandLine.Internals
open System.Runtime.CompilerServices

type Args = string list

type HelpElement =
  /// prints `usage: $(command name) $(args)`.
  | HelpUsage
  /// prints `usage: $(command name) $(args)` of which `args` can be customized.
  | HelpUsageCustomArgs of args: string list
  /// prints a string.
  | HelpRawString of text: string
  /// prints the infomation of all the subcommands.
  | HelpAllSubcommands
  /// prints the infomation of the specified subcommands.
  | HelpSpecificSubcommands of names: string list
  /// prints the infomation of all the command options.
  | HelpAllOptions
  /// prints the infomation of the specified command options.
  | HelpSpecificOptions of namesWithoutHyphen: string list
  /// prints elements as a section. the content will be indented.
  /// nested sections make the indentation deeper.
  | HelpSection of sectionName: string * sectionBody: seq<HelpElement>
  /// prints an empty line.
  | HelpEmptyLine

type HelpText = HelpElement seq

type CommandSuggestion =
  /// suggests a set of string.
  | Values of string list
  /// suggests a set of string with description.
  | ValuesWithDescription of (string * string) list
  /// suggests files optionally with pattern.
  | Files of pattern: string option
  /// suggests directories.
  | Directories of pattern: string option
  /// suggests a command option.
  | OptionSuggestion of (string list) * string
  /// prints a message.
  | Message of string

type CommandOptionSummary = {
  names: string list;
  description: string;
  isFlag: bool;
  paramNames: (string list) list
  isMatch: string list -> string list option
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
    this.names |> List.collect (function
      | x when x.Length = 1 ->
        [ "-"; "/" ] |> List.map (fun prefix -> sprintf "%s%s" prefix x)
      | x ->
        [ "--"; "/" ] |> List.map (fun prefix -> sprintf "%s%s" prefix x))

  member this.Print () =
    let o x = if this.isFlag then sprintf "%s[+|-]" x else sprintf "%s%s" x this.Param
    ((this.NameRepresentations |> List.filter (String.startsWith "-") |> String.concat ", " |> o), this.description)

type CommandSummary = {
  name: string;
  displayName: string option;
  description: string;
  paramNames: (string list) option
  help: HelpText option
  genSuggestions: Args -> CommandSuggestion list
}

type CommandInfo = {
  summary: CommandSummary
  options: CommandOptionSummary list
  subcommands: Command<int> list
}
and Command<'a> = StateConfig<CommandInfo, Args, 'a>

type CommandInfo with
  static member empty =
    {
      summary = Unchecked.defaultof<CommandSummary>
      options = []
      subcommands = []
    }

[<Extension>]
type ICommandExt() =
  [<Extension>]
  static member inline Summary(x: Command<_>) =
    (x.config CommandInfo.empty).summary
