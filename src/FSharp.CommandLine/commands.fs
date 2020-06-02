namespace FSharp.CommandLine

[<AutoOpen>]
module Commands =
  open System
  open FSharp.CommandLine.Options
  open FSharp.CommandLine.Generators
  open FSharp.CommandLine.Internals.Abstraction

  let inline private mapSummary f co =
    co |> StateConfig.mapConfig (fun cfg -> { cfg with summary = f cfg.summary })

  type CommandBuilder() =
    member inline __.Bind (c, f) : Command<_> = StateConfig.bind f c
    member inline __.Return x = StateConfig.returnValue x
    member inline __.For (c, f) : Command<_> = StateConfig.bind f c
    member inline __.Yield x = StateConfig.returnValue x
    member inline __.ReturnFrom (x: Command<_>) = x
    member inline __.Combine (a, b) : Command<_> = StateConfig.combine a b
    member inline __.Combine (f: unit -> _, b: Command<_>) : Command<_> = StateConfig.combine (f()) b
    member inline __.Combine (a: Command<_>, f: unit -> _) : Command<_> = StateConfig.combine a (f())
    member inline __.Zero () = StateConfig.returnValue ()
    member inline __.Delay (f: unit -> Command<_>) = f
    member inline __.Undelay x : Command<_> = x()
    member inline __.Run f : Command<_> = f()
    member inline __.TryWith (f, h) = try f() with exn -> h exn
    member inline __.TryFinally (f, h) = try f() finally h()
    member inline this.Using (disp: #System.IDisposable, m) =
      this.TryFinally(
        this.Delay(fun () -> m disp),
        fun () -> dispose disp
      )
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
    member inline __.ImportCommand (c1, c2, f) : Command<_> = StateConfig.zip c1 c2 f
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
    [<CustomOperation("preprocess", MaintainsVariableSpaceUsingBind = true)>]
    [<Obsolete("the 'preprocess' operator is no longer needed and does nothing.")>]
    member inline __.Preprocess xs = xs
    /// optional. specifies the subcommands. 
    [<CustomOperation("subcommands", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Subcommands (co, xs) = 
      {
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

  let command = CommandBuilder ()

  module Command =
    let args : Command<Args> = StateConfig.args
    let inline bind f m : Command<_> = StateConfig.bind f m
    let inline returnValue x : Command<_> = StateConfig.returnValue x
    let inline returnWith f : Command<_> = StateConfig.returnWith f
    let inline map f m : Command<_> = StateConfig.map f m
    let inline mapInfo f m : Command<_> = StateConfig.mapConfig f m
    let inline zip a b f : Command<_> = StateConfig.zip a b f
    let inline combine a b : Command<_> = StateConfig.combine a b

    let private runMain args (cmd: Command<int>) =
      match args |> List.ofArray with
        | OptionParse ReservedCommandOptions.suggestOption (sug, rest) ->
          Suggestions.generate rest cmd
          |> Suggestions.print (SuggestionBackends.findByName sug)
          |> printfn "%A"
          (0, [])
        | OptionParse ReservedCommandOptions.helpOption (true, rest) ->
          for line in Help.generate rest cmd do
            printfn "%s" line
          (0, [])
        | args ->
          cmd.func args

    #if DEBUG
    let runAsEntryPointDebug args (cmd: Command<int>) = runMain args cmd 
    #endif

    /// executes the command as an entry point:
    /// 
    /// ```
    /// [<EntryPoint>]
    /// let main argv =
    ///   Command.runAsEntryPoint argv command
    /// ```
    let runAsEntryPoint args (cmd: Command<int>) =
      try
        runMain args cmd |> fst
      with
        | RequestExit code -> code
        | RequestShowHelp msg ->
          cprintfn ConsoleColor.Red "error: %s\n" msg
          for line in Help.generate (args |> List.ofArray) cmd do
            printfn "%s" line
          -1
        | OptionParseFailed (_, msg)
        | CommandExecutionFailed msg -> cprintfn ConsoleColor.Red "error: %s\n" msg; -1
        | e -> reraise' e

    /// stops the execution immediately and then exits with the specified code.
    let inline exit code = 
      RequestExit code |> raise

    /// stops the execution immediately and then exits with the specified error message and code `-1`.
    let inline fail msg =
      CommandExecutionFailed msg |> raise

    /// stops the execution immediately and then exits with the specified error message and code `-1`.
    let inline failf fmt =
      Printf.kprintf (fun msg -> CommandExecutionFailed msg |> raise ) fmt

    /// if there are remaining unknown options, then stops the execution
    /// immediately and then exits with code `-1`.
    let failOnUnknownOptions () =
      command {
        let! argv = StateConfig.args in
        let uks = argv |> CommandOption.getRemainingOptions in
        if uks |> List.isEmpty then
          return ()
        else
          sprintf "unknown option: '%s'" (List.head uks) |> CommandExecutionFailed |> raise
      }
    
    /// shows the error message and the help of the current (sub)command, then exits with code `-1`.
    let inline failShowingHelp message =
      RequestShowHelp message |> raise

