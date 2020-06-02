#r "src/FSharp.CommandLine/bin/Debug/net46/FSharp.Scanf.dll"
#r "src/FSharp.CommandLine/bin/Debug/net46/FSharp.CommandLine.dll"

#load "src/common/prelude.fs"
open System
open FSharp.CommandLine

let fileOption =
  commandOption {
    names ["f"; "file"]
    description "Name of a file to use (Default index: 0)"
    takes (format("%s:%i").withNames ["filename"; "index"])
    takes (format("%s").map (fun filename -> (filename, 0)))
    suggests (fun _ -> [CommandSuggestion.Files None])
  }

type Verbosity = Quiet | Normal | Full | Custom of int

let verbosityOption =
  commandOption {
    names ["v"; "verbosity"]
    description "Display this amount of information in the log."
    takes (regex @"q(uiet)?$" |> asConst Quiet)
    takes (regex @"n(ormal)?$" |> asConst Normal)
    takes (regex @"f(ull)?$" |> asConst Full)
    takes (format("custom:%i").map (fun level -> Custom level))
    takes (format("c:%i").map (fun level -> Custom level))
  }

let inline nearest r g b =
  let brb = if r>128 || g>128 || b>128 then 8 else 0
  let rb = if r>64 then 4 else 0
  let gb = if g>64 then 2 else 0
  let bb = if b>64 then 1 else 0
  (brb + rb + gb + bb) |> enum<ConsoleColor>

let colorOption = 
  commandOption {
    names ["color"; "c"]; description "Colorize the output."
    takes (format "red"   |> asConst ConsoleColor.Red)
    takes (format "green" |> asConst ConsoleColor.Green)
    takes (format "blue"  |> asConst ConsoleColor.Blue)
    takes (format("%i,%i,%i").map (fun (r,g,b) -> nearest r g b))
    suggests (fun _ -> [CommandSuggestion.Values["red"; "green"; "blue"]])
  } 

let echoCommand =
  command {
    name "echo"
    displayName "main echo"
    description "Echo the input."
    opt color in colorOption |> CommandOption.zeroOrExactlyOne
    do! Command.failOnUnknownOptions()
    let! args = Command.args
    if args |> List.contains "help" then
      do! Command.failShowingHelp "showing help."
    do 
      let s = args |> String.concat " " 
      match color with
        | Some c -> cprintfn c "%s" s
        | None ->   printfn "%s" s
    return 0
  }

let plus1 =
  command {
    opt number in
      commandOption {
        names ["n"; "number"]; description "integer number."
        takes (format("%i").withNames["num"])
      } |> CommandOption.whenMissingUse 0
    do printfn "num: %i" number
    return number
  }

let x =
  command {
    do! Command.failOnUnknownOptions()
    do! Command.failOnUnknownOptions()
  }

let mainCommand =
  command {
    name "main"
    description "The main command."
    opt files in fileOption |> CommandOption.zeroOrMore
    opt verbosity in verbosityOption |> CommandOption.zeroOrExactlyOne 
                                     |> CommandOption.whenMissingUse Normal
    import num in plus1
    subcommands [echoCommand]
    do! Command.failOnUnknownOptions()
    let! args = Command.args
    if args |> List.contains "help" then
      do! Command.failShowingHelp "showing help."
    do printfn "%A, %A" files verbosity
    return 0
  }

while true do
  printf "test> "
  let inputs = Console.ReadLine() |> String.split ' ' |> String.removeEmptyEntries
  let mc = mainCommand
  try
    mc |> Command.runAsEntryPointDebug inputs ||> (fun code args -> printfn "(exited with %i, unused args:%A)\n" code args)
  with
    | RequestExit code -> printfn "(exited with %i)\n" code
    | RequestShowHelp msg ->
      cprintfn ConsoleColor.Red "error: %s\n" msg
      for line in Help.generate (inputs |> List.ofArray) mc do
        printfn "%s" line
    | OptionParseFailed (_, msg)
    | CommandExecutionFailed msg -> cprintfn ConsoleColor.Red "error: %s\n" msg
    | e -> reraise' e

