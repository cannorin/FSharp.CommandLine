module FSharp.CommandLine.Commands
open System
open FSharp.CommandLine.Scanf
open FSharp.CommandLine.Options
open FSharp.CommandLine.Generators

type Args = string list

[<Struct>]
type Command<'a> = {
    summary: CommandSummary ref;
    func: Args -> ('a * Args);

    subcommands: (ICommand<int> list) ref;
    options: (CommandOptionSummary list) ref
  }
  with
    interface ICommand<'a> with
      member this.Run argv = this.func argv
      member this.Summary() = !(this.summary)
      member this.Subcommands() = !(this.subcommands)
      member this.Options() = !(this.options)

module Command =
  #if DEBUG
  let runAsEntryPointDebug args (cmd: #ICommand<int>) =
    match args |> List.ofArray |> parse ReservedCommandOptions.suggestOption with
      | (Some sug, rest) ->
        Suggestions.generate rest cmd |> Suggestions.print (SuggestionBackends.findByName sug) |> printfn "%A"
        (0, [])
      | (None, xs) -> cmd.Run xs
  #endif
  
  let runAsEntryPoint args (cmd: #ICommand<int>) =
    try
      match args |> List.ofArray |> parse ReservedCommandOptions.suggestOption with
        | (Some sug, rest) ->
          Suggestions.generate rest cmd |> Suggestions.print (SuggestionBackends.findByName sug) |> printfn "%A"
          0
        | (None, xs) -> cmd.Run xs |> fst
    with
      | RequestExit code -> code
      | OptionParseFailed (_, msg)
      | CommandExecutionFailed msg -> cprintfn ConsoleColor.Red "error: %s\n" msg; -1
      | e -> reraise' e
    
  let inline bind (f: 'a -> Command<'b>) (c: #ICommand<'a>) : Command<'b> =
    let smry = ref (c.Summary())
    let opts = ref (c.Options())
    let subs = ref (c.Subcommands())
    {
      summary = smry;
      options = opts;
      subcommands = subs;
      func =
        (fun args ->
          let (a, args') = c.Run args in
          let cb = f a in
          let res = cb.func args' in
          smry := c.Summary()
          subs := List.append (c.Subcommands()) (!cb.subcommands);
          opts := List.append (c.Options()) (!cb.options);
          res
        )
    }

  let inline map (f: 'a -> 'b) (c: #ICommand<'a>) : Command<'b> =
    {
      summary = ref (c.Summary());
      options = ref (c.Options());
      subcommands = ref (c.Subcommands());
      func = (fun args -> let (a, args') = c.Run args in (f a, args'))
    }

  let returnValue x =
    {
      summary = ref Unchecked.defaultof<CommandSummary>;
      options = ref [];
      subcommands = ref [];
      func = (fun args -> (x, args))
    }
  
  let inline combine a b =
    a |> bind (fun () -> b)

  let inline delay (f: unit -> #ICommand<'a>) : Command<'a> =
    let smry = ref Unchecked.defaultof<CommandSummary>
    let opts = ref []
    let subs = ref []
    {
      summary = smry;
      options = opts;
      subcommands = subs;
      func = 
        (fun args ->
          let cmd = f ()
          let res = cmd.Run args
          smry := cmd.Summary()
          opts := cmd.Options()
          subs := cmd.Subcommands()
          res
        )
    }

  let args =
    {
      summary = ref Unchecked.defaultof<CommandSummary>;
      options = ref [];
      subcommands = ref [];
      func = (fun args -> (args, args))
    }
  
[<Struct>]
type AugmentedCommandOption<'a, 'b> = {
    orig: Options.CommandOption<'a>;
    augmenter: Options.CommandOption<'a> -> Args -> ('b * Args)
  }
  with
    member private this.origi = this.orig :> ICommand<_>
    interface ICommand<'b> with
      member this.Summary() = this.origi.Summary()
      member this.Options() = this.origi.Options()
      member this.Subcommands() = this.origi.Subcommands()
      member this.Run args = this.augmenter this.orig args

module CommandOptionUtils =
  let inline augment f c = { orig=c; augmenter=f }
  let inline map f ac =
    { 
      orig=ac.orig;
      augmenter=(fun o x -> ac.augmenter o x |> Tuple.map2 f id)
    }

  let inline zeroOrMore co =
    co |> augment Options.parseMany

  let inline zeroOrExactlyOne co =
    co |> zeroOrMore 
       |> map (function 
           | [] -> None 
           | x :: [] -> Some x 
           | _ -> sprintf "the option '%s' should be provided only once" ((co :> ICommand<_>).Summary().name) |> CommandExecutionFailed |> raise
          )

  let inline whenMissingUse v co =
    co |> map (function Some x -> x | None -> v)

let inline (>>=) c f = Command.bind f c 

let inline private preprocess (cmd: #ICommand<_>) args =
  let inline checkSubCommand lastName (subcmd: #ICommand<_>) =
    subcmd.Summary().name = lastName
  match args with
    | [] -> []
    | help :: _ when ReservedCommandOptions.helpOption.summary.NameRepresentations |> List.contains help ->
      for line in Help.generate cmd do
        printfn "%s" line
      done
      RequestExit 0 |> raise
    | h :: rest when h = runBeforePreprocess ->
      RaiseInfo (cmd.Summary(), cmd.Subcommands(), cmd.Options(), rest) |> raise
    | subc :: args when cmd.Subcommands() |> List.exists (checkSubCommand subc) ->
      let subcommand = cmd.Subcommands() |> List.find (checkSubCommand subc)
      let (retCode, _) = subcommand.Run args
      RequestExit retCode |> raise
    | xs when xs |> List.contains runBeforePreprocess ->
      let rest = xs |> List.filter ((<>) runBeforePreprocess)
      RaiseInfo (cmd.Summary(), cmd.Subcommands(), cmd.Options(), rest) |> raise
    | xs -> xs

[<Struct>]
type CommandBuilder =
  member inline __.Bind (c, f) = Command.bind f c
  member inline __.Return x = Command.returnValue x
  member inline __.ReturnFrom x = x
  member inline __.Combine (a, b) = Command.combine a b
  //member inline __.Delay f = Command.delay f
  member inline __.Zero () = Command.returnValue ()
  [<CustomOperation("name", MaintainsVariableSpaceUsingBind = true)>]
  member inline __.Name (co: Command<_>, x) = { co with summary = ref { !co.summary with name = x } }
  [<CustomOperation("displayName", MaintainsVariableSpaceUsingBind = true)>]
  member inline __.DisplayName (co: Command<_>, n) = { co with summary = ref { !co.summary with displayName = Some n } }
  [<CustomOperation("description", MaintainsVariableSpaceUsingBind = true)>]
  member inline __.Description (co: Command<_>, x) = { co with summary = ref { !co.summary with description = x } }
  [<CustomOperation("subcommands", MaintainsVariableSpaceUsingBind = true)>]
  member inline __.Subcommands (co: Command<_>, xs) = { co with subcommands = ref xs }
  [<CustomOperation("suggests", MaintainsVariableSpaceUsingBind = true)>]
  member inline __.Suggests (co: Command<_>, f) = { co with summary = ref { !co.summary with genSuggestions = f } }
  [<CustomOperation("help", MaintainsVariableSpaceUsingBind = true)>]
  member inline __.Help (co: Command<_>, xs: HelpElement seq) =
    { co with summary = ref { !co.summary with help = Some xs } }
  [<CustomOperation("preprocess", MaintainsVariableSpaceUsingBind = true)>]
  member inline __.Preprocess (co: Command<_>) = 
    { co with func = co.func >> (fun (bv, args) -> let args' = preprocess co args in (bv, args')) }

let command = CommandBuilder ()

module CommandUtils =
  let failOnUnknownOptions =
    command {
      let! argv = Command.args in
      let uks = argv |> Options.getRemainingOptions in
      if uks |> List.isEmpty then
        return ()
      else
        sprintf "unknown option: '%s'" (List.head uks) |> CommandExecutionFailed |> raise
    }


