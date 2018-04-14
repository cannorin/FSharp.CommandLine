FSharp.CommandLine
======================

The FSharp.CommandLine library can be [installed from NuGet](ttps://nuget.org/packages/FSharp.CommandLine)

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

Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/cannorin/FSharp.CommandLine/tree/master/docs/content
  [gh]: https://github.com/cannorin/FSharp.CommandLine
  [issues]: https://github.com/cannorin/FSharp.CommandLine/issues
  [readme]: https://github.com/cannorin/FSharp.CommandLine/blob/master/README.md
  [license]: https://github.com/cannorin/FSharp.CommandLine/blob/master/LICENSE.txt
