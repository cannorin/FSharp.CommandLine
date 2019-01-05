namespace FSharp.CommandLine

[<AutoOpen>]
module rec Commands =
  open System
  open FSharp.CommandLine.Options
  open FSharp.CommandLine.Generators
  open FSharp.CommandLine.Internals.Abstraction

  type Args = string list

  type Command<'a> = StateConfig<CommandInfo, Args, 'a>

  let inline private mapSummary f co =
    co |> StateConfig.mapConfig (fun cfg -> { cfg with summary = f cfg.summary })

  type CommandBuilder() =
    member inline __.Bind (c, f) : Command<_> = StateConfig.bind f c
    member inline __.Return x = StateConfig.returnValue x
    member inline __.For (c, f) : Command<_> = StateConfig.bind f c
    member inline __.Yield x = StateConfig.returnValue x
    member inline __.ReturnFrom x = x
    member inline __.Combine (a, b) : Command<_> = StateConfig.combine a b
    member inline __.Zero () = StateConfig.returnValue ()
    member inline this.Delay f = f
    member inline this.Undelay f = f()
    member inline this.Run f = f()
    member inline this.TryWith (f, h) = try f() with exn -> h exn
    member inline this.TryFinally (f, h) = try f() finally h()
    member inline this.Using (disp: #System.IDisposable, m) =
      this.TryFinally(
        this.Delay(fun () -> m disp),
        fun () -> dispose disp
      )
    member inline this.While (cond, m) =
      let rec loop cond m =
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
    [<Obsolete("binding command options no longer works properly. use 'opt .. in ..' operation instead.", true)>]
    member inline __.Bind (c: #ICommandOption<'a>, f: 'a -> #ICommand<'b>) : Command<'b> =
      failwith "obsolete"
    [<CustomOperation("import", MaintainsVariableSpaceUsingBind = true, IsLikeZip=true)>]
    member inline __.ImportCommand (c1, c2, f) : Command<_> = StateConfig.zip c1 c2 f
    [<CustomOperation("name", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Name (co, x) = 
      co |> mapSummary (fun s -> { s with name = x })
    [<CustomOperation("displayName", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.DisplayName (co, n) =
      co |> mapSummary (fun s -> { s with displayName = Some n })
    [<CustomOperation("description", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Description (co, x) =
      co |> mapSummary (fun s -> { s with description = x })
    [<CustomOperation("suggests", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Suggests (co: Command<_>, f) = 
      co |> mapSummary (fun s -> { s with genSuggestions = f })
    [<CustomOperation("help", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.Help (co: Command<_>, xs: HelpElement seq) =
      co |> mapSummary (fun s -> { s with help = Some xs })
    [<CustomOperation("preprocess", MaintainsVariableSpaceUsingBind = true)>]
    [<Obsolete("the 'preprocess' operator is no longer needed and does nothing.")>]
    member inline __.Preprocess xs = xs
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
                  List.tryFind (fun (x:ICommand<_>) -> x.Summary().name = h) xs
                | _ -> None
            if sc.IsSome then
              let (code, _) = sc.Value.Func (List.tail args)
              RequestExit code |> raise
            else
              co.func args
      } 
    [<CustomOperation("showHelp", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.ShowHelp (co: #ICommand<_>, condition) =
      if condition then
        for line in Help.generate [] (co |> StateConfig.map (fun _ -> 0)) do
          printfn "%s" line
      co

  let command = CommandBuilder ()

  module Command =
    let args = StateConfig.args
    let inline bind f m : Command<_> = StateConfig.bind f m
    let inline returnValue x : Command<_> = StateConfig.returnValue x
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
        | args -> cmd.func args

    #if DEBUG
    let runAsEntryPointDebug args (cmd: Command<int>) = runMain args cmd 
    #endif
    
    let runAsEntryPoint args (cmd: Command<int>) =
      try
        runMain args cmd |> fst
      with
        | RequestExit code -> code
        | OptionParseFailed (_, msg)
        | CommandExecutionFailed msg -> cprintfn ConsoleColor.Red "error: %s\n" msg; -1
        | e -> reraise' e
    
    let failOnUnknownOptions () =
      command {
        let! argv = StateConfig.args in
        let uks = argv |> CommandOption.getRemainingOptions in
        if uks |> List.isEmpty then
          return ()
        else
          sprintf "unknown option: '%s'" (List.head uks) |> CommandExecutionFailed |> raise
      }

