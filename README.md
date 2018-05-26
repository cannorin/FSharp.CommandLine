FSharp.CommandLine
======================

The FSharp.CommandLine library can be [installed from NuGet](https://nuget.org/packages/FSharp.CommandLine)

```
  PM> Install-Package FSharp.CommandLine
```

FSharp.CommandLine is a monadic commandline application framework that automatically generates both help texts and shell suggestions.

This library also contains type-safe scanf and type-safe commandline option parser.

Example
-------

```fsharp
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

let mainCommand () =
  command {
    name "main"
    description "The main command."
    let! files = fileOption |> CommandOptionUtils.zeroOrMore
    let! verbosity = verbosityOption |> CommandOptionUtils.zeroOrExactlyOne 
                                     |> CommandOptionUtils.whenMissingUse Normal
    preprocess
    do printfn "%A, %A" files verbosity
    return 0
  }

[<EntryPoint>]
let main argv =
  mainCommand() |> Command.runAsEntryPoint argv
```

## License

Apache 2. See LICENSE.txt for details.
