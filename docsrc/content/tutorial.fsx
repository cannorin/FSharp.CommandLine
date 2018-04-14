(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.CommandLine/"

(**
Introducing your project
========================

Say more

*)
#r "FSharp.CommandLine.dll"
open System
open FSharp.CommandLine
open FSharp.CommandLine.Options
open FSharp.CommandLine.Commands

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
    takes (regex @"n(ormal)?$" |> asConst Quiet)
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
  } 

let echoCommand () =
  command {
    name "echo"
    description "Echo the input."
    let! color = colorOption |> CommandOptionUtils.zeroOrExactlyOne
    preprocess
    let! args = Command.args
    do 
      let s = args |> String.concat " " 
      match color with
        | Some c -> cprintfn c "%s" s
        | None ->   printfn "%s" s
    return 0
  }

let mainCommand () =
  command {
    name "main"
    description "The main command."
    let! files = fileOption |> CommandOptionUtils.zeroOrMore
    let! verbosity = verbosityOption |> CommandOptionUtils.zeroOrExactlyOne 
                                     |> CommandOptionUtils.whenMissingUse Normal
    subcommands [echoCommand()]
    preprocess
    do printfn "%A, %A" files verbosity
    return 0
  }

[<EntryPoint>]
let main argv =
  mainCommand() |> Command.runAsEntryPoint argv

(**
Some more info
*)
