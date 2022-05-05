namespace FSharp.CommandLine

open System
open FSharp.CommandLine.Generators
open FSharp.CommandLine.Internals

module Command =
  let args : Command<Args> = StateConfig.args
  let bind (f: 'a -> Command<'b>) (m: Command<'a>) : Command<'b> = StateConfig.bind f m
  let returnValue (x: 'a) : Command<'a> = StateConfig.returnValue x
  let returnWith (f: unit -> 'a) : Command<'a> = StateConfig.returnWith f
  let map (f: 'a -> 'b) (m: Command<'a>) : Command<'b> = StateConfig.map f m
  let mapInfo (f: CommandInfo -> CommandInfo) (m: Command<'a>) : Command<'a> = StateConfig.mapConfig f m
  let zip (a: Command<'a>) (b: Command<'b>) (f: 'a -> 'b -> 'c) : Command<'c> = StateConfig.zip a b f
  let combine (a: Command<'a>) (b: Command<'b>) : Command<'b> = StateConfig.combine a b

  let private runMain args (cmd: Command<int>) =
    match args |> List.ofArray with
      | CommandOption.Parse ReservedCommandOptions.suggestOption (sug, rest) ->
        Suggestions.generate rest cmd
        |> Suggestions.print (SuggestionBackends.findByName sug)
        |> printfn "%A"
        (0, [])
      | CommandOption.Parse ReservedCommandOptions.helpOption (true, rest) ->
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
  let exit code = RequestExit code |> raise

  /// stops the execution immediately and then exits with the specified error message and code `-1`.
  let fail msg = CommandExecutionFailed msg |> raise

  /// stops the execution immediately and then exits with the specified error message and code `-1`.
  let failf fmt = Printf.kprintf (fun msg -> CommandExecutionFailed msg |> raise ) fmt

  /// if there are remaining unknown options, then stops the execution
  /// immediately and then exits with code `-1`.
  let failOnUnknownOptions () =
    args |> bind (fun argv ->
      let unknownOptions = CommandOption.getRemainingOptions argv
      if List.isEmpty unknownOptions then returnValue ()
      else
        unknownOptions
        |> List.head
        |> sprintf "unknown option: '%s'"
        |> CommandExecutionFailed
        |> raise
    )

  /// shows the error message and the help of the current (sub)command, then exits with code `-1`.
  let failShowingHelp message = RequestShowHelp message |> raise