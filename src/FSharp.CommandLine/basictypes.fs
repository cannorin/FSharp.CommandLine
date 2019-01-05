namespace FSharp.CommandLine

[<AutoOpen>]
module BasicTypes =
  open FSharp.CommandLine.Internals.Abstraction
  open System.Runtime.CompilerServices

  type Args = string list

  type HelpElement =
    | HelpUsage
    | HelpUsageCustomArgs of args: string list
    | HelpRawString of text: string
    | HelpAllSubcommands
    | HelpSpecificSubcommands of names: string list
    | HelpAllOptions
    | HelpSpecificOptions of namesWithoutHyphen: string list
    | HelpSection of sectionName: string * sectionBody: seq<HelpElement>
    | HelpEmptyLine

  module internal Seq =
    let inline snoc x xs = seq { yield! xs; yield x }

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
    [<CustomOperation("conditionalSection")>]
    member inline __.ConditionalSection (xs, sectionName, cond, section) =
      if cond() then
        xs |> Seq.snoc (HelpSection(sectionName, section))
      else 
        xs
    [<CustomOperation("emptyLine")>]
    member inline __.EmptyLine xs = xs |> Seq.snoc HelpEmptyLine

  let helpText = HelpBuilder ()

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

  [<Struct>]
  type CommandSummary = {
      name: string;
      displayName: string option;
      description: string;
      paramNames: (string list) option
      help: HelpElement seq option
      genSuggestions: Args -> CommandSuggestion list
    }

  [<Struct>]
  type CommandInfo = {
      summary: CommandSummary
      options: CommandOptionSummary list
      subcommands: ICommand<int> list
    }
  and ICommand<'a> = IStateConfig<CommandInfo, Args, 'a>

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
    static member inline Summary(x: #ICommand<_>) =
      (x.Config CommandInfo.empty).summary
