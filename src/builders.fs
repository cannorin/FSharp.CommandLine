namespace FSharp.CommandLine.Internals

open FSharp.CommandLine
open Microsoft.FSharp.Quotations

module internal Helpers =
  let inline snoc x xs = seq { yield! xs; yield x }

  let inline mapSummary f co =
    co |> Command.mapInfo (fun cfg -> { cfg with summary = f cfg.summary })

open Helpers

type HelpBuilder() =
  member inline __.For (_, _) = failwith "Not supported"
  member inline __.Yield _ : HelpElement seq = Seq.empty

  /// prints `usage: $(command name) $(args)`.
  [<CustomOperation("defaultUsage")>]
  member inline __.Usage xs = xs |> snoc HelpUsage

  /// prints `usage: $(command name) $(args)` of which `args` can be customized.
  [<CustomOperation("customUsage")>]
  member inline __.UsageWithCustomArgs (xs, argNames) = xs |> snoc (HelpUsageCustomArgs argNames)

  /// prints a string.
  [<CustomOperation("text")>]
  member inline __.RawText (xs, str) = xs |> snoc (HelpRawString str)

  /// prints the infomation of all the subcommands.
  [<CustomOperation("allSubcommands")>]
  member inline __.Subcommands xs = xs |> snoc HelpAllSubcommands

  /// prints the infomation of the specified subcommands.
  [<CustomOperation("specificSubcommands")>]
  member inline __.SpecificSubcommands (xs, cmds) = xs |> snoc (HelpSpecificSubcommands cmds)

  /// prints the infomation of all the command options.
  [<CustomOperation("allOptions")>]
  member inline __.Options xs = xs |> snoc HelpAllOptions

  /// prints the infomation of the specified command options.
  [<CustomOperation("specificOptions")>]
  member inline __.SpecificOptions (xs, opts) = xs |> snoc (HelpSpecificOptions opts)

  /// prints elements as a section. the content will be indented.
  /// nested sections make the indentation deeper.
  [<CustomOperation("section")>]
  member inline __.Section (xs, sectionName, section) = xs |> snoc (HelpSection(sectionName, section))

  /// prints elements as a section when the condition holds. the content will be indented.
  /// nested sections make the indentation deeper.
  [<CustomOperation("conditionalSection")>]
  member inline __.ConditionalSection (xs, sectionName, cond, section) =
    if cond() then
      xs |> snoc (HelpSection(sectionName, section))
    else
      xs

  /// prints an empty line.
  [<CustomOperation("emptyLine")>]
  member inline __.EmptyLine xs = xs |> snoc HelpEmptyLine

type CommandOptionBuilder<'a>(dc: unit -> CommandOption<'a>) =
  member __.For (_, _) = failwith "Not supported"
  member __.Yield _ = dc ()

  /// required.
  /// specifies the option's names. hyphens should not be included,
  /// as they will automatically be handled depending on
  /// the length of the name and optionally the `style` command.
  [<CustomOperation("names")>]
  member __.Names (co, x) = { co with baseSummary = { co.baseSummary with names = x } }

  [<CustomOperation("description")>]
  member __.Description (co, x) = { co with baseSummary = { co.baseSummary with description = x } }

  /// required for command option.
  /// specifies the format of the argument. for example:
  /// `takes (format("%s").map(fun str -> someFunc str))`
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

  /// `takesFormat fmt (fun .. -> ..)` is a shorthand for
  /// `takes (format(fmt).map(fun .. -> ..))`.
  [<CustomOperation("takesFormat")>]
  member inline __.TakesFormat (co: CommandOption<_>, fmt: PrintfFormat<_,_,_,_,_>, [<ReflectedDefinition>]mapper: Expr<_ -> _>) =
    let mf x = (FuncHelper.compileFunc mapper) x in
    let pns = FuncHelper.getFirstArgumentName mapper in
    let x =
      { format = fmt; handler = mf; paramNames = pns }
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

  /// optional.
  /// makes the option's argument optional, and specifies the default value
  /// that will be used when the argument is not provided.
  [<CustomOperation("defaultValue")>]
  member __.DefaultValue (co: CommandOption<'a>, value: 'a) =
    { co with
        kind =
          match co.kind with
            | TakingValueWith (_, xs) -> TakingValueWith (UseDefault value, xs)
            | x -> x
    }

  /// optional.
  /// specifies the command suggestions this option will generate.
  [<CustomOperation("suggests")>]
  member __.Suggests (co, f) = { co with baseSummary = { co.baseSummary with genSuggestions=f } }

  /// optional.
  /// specify how to treat options like ```-abcd```.
  /// the default value is `MergedShort`.
  [<CustomOperation("style")>]
  member __.Style (co, st) = { co with style = st }

type CommandBuilder() =
  member inline __.Bind (c, f) = Command.bind f c
  member inline __.Return x = Command.returnValue x
  member inline __.For (c, f) = Command.bind f c
  member inline __.Yield x = Command.returnValue x
  member inline __.ReturnFrom (x: Command<_>) = x
  member inline __.Combine (a, b) = Command.combine a b
  member inline __.Combine (f, b) = Command.combine (f()) b
  member inline __.Combine (a, f) = Command.combine a (f())
  member inline __.Zero () = Command.returnValue ()
  member inline __.Delay (f: unit -> Command<_>) = f
  member inline __.Undelay x : Command<_> = x()
  member inline __.Run f : Command<_> = f()
  member inline __.TryWith (f, h) = try f() with exn -> h exn
  member inline __.TryFinally (f, h) = try f() finally h()
  member inline this.Using (disp: #System.IDisposable, m) = this.TryFinally(this.Delay(fun () -> m disp), fun () -> dispose disp)
  member inline this.While (cond, m: unit -> Command<_>) =
    let rec loop cond m : Command<_> =
      if cond () then this.Combine(this.Undelay m, loop cond m)
      else this.Zero ()
    loop cond m
  member inline this.For (xs: #seq<_>, exec) =
    this.Using(
      (xs :> seq<_>).GetEnumerator(),
      fun en ->
        this.While(
          en.MoveNext,
          this.Delay(fun () -> exec en.Current))
    )

  /// uses a command option.
  [<CustomOperation("opt", MaintainsVariableSpaceUsingBind = true, IsLikeZip=true)>]
  member inline __.UseOption (co: Command<'a>, opt: #ICommandOption<'b>, f: 'a -> 'b -> 'c) =
    {
      config = co.config >> opt.Config
      func =
        fun args ->
          let (a, args) = co.func args
          let (b, args) = opt.Parse args
          (f a b, args)
    }

  /// imports a command to this command. will inherit options and other metadatas.
  [<CustomOperation("import", MaintainsVariableSpaceUsingBind = true, IsLikeZip=true)>]
  member inline __.ImportCommand (c1, c2, f) : Command<_> = Command.zip c1 c2 f

  /// required. sets the name of the command. will also be used when this command is
  /// a subcommand of the other one.
  [<CustomOperation("name", MaintainsVariableSpaceUsingBind = true)>]
  member inline __.Name (co, x) =
    co |> mapSummary (fun s -> { s with name = x })

  /// optional. sets the name of the command that will be displayed in the help text.
  /// the one speficied by `name` will be used if not specified.
  [<CustomOperation("displayName", MaintainsVariableSpaceUsingBind = true)>]
  member inline __.DisplayName (co, n) =
    co |> mapSummary (fun s -> { s with displayName = Some n })

  /// required. sets the description of the command.
  [<CustomOperation("description", MaintainsVariableSpaceUsingBind = true)>]
  member inline __.Description (co, x) =
    co |> mapSummary (fun s -> { s with description = x })

  /// optional. speficies the suggestions it will generate.
  [<CustomOperation("suggests", MaintainsVariableSpaceUsingBind = true)>]
  member inline __.Suggests (co: Command<_>, f) =
    co |> mapSummary (fun s -> { s with genSuggestions = f })

  /// optional. customizes the help text.
  [<CustomOperation("help", MaintainsVariableSpaceUsingBind = true)>]
  member inline __.Help (co: Command<_>, xs: HelpElement seq) =
    co |> mapSummary (fun s -> { s with help = Some xs })

  /// optional. specifies the subcommands.
  [<CustomOperation("subcommands", MaintainsVariableSpaceUsingBind = true)>]
  member inline __.Subcommands (co, xs) = {
    config =
      fun cfg ->
        let cfg = co.config cfg
        { cfg with subcommands = cfg.subcommands @ xs }
    func =
      fun args ->
        let sc =
          match args with
            | h :: _ ->
              List.tryFind (fun (x:Command<_>) -> x.Summary().name = h) xs
            | _ -> None
        if sc.IsSome then
          let (code, _) = sc.Value.func (List.tail args)
          RequestExit code |> raise
        else
          co.func args
  }


namespace FSharp.CommandLine

open FSharp.CommandLine.Internals

[<AutoOpen>]
module Builders =
  let helpText = new HelpBuilder()
  let commandOption<'a> =
    new CommandOptionBuilder<'a>(fun () -> CommandOption<'a>.defaultOption)
  let commandFlag =
    new CommandOptionBuilder<_>(fun () -> CommandOption<bool>.defaultFlag)
  let command = CommandBuilder ()