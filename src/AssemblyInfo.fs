namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharp.CommandLine")>]
[<assembly: AssemblyProductAttribute("FSharp.CommandLine")>]
[<assembly: AssemblyDescriptionAttribute("A framework for building command line application in F#")>]
[<assembly: AssemblyConfigurationAttribute("Release")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] AssemblyTitle = "FSharp.CommandLine"
    let [<Literal>] AssemblyProduct = "FSharp.CommandLine"
    let [<Literal>] AssemblyDescription = "A framework for building command line application in F#"
    let [<Literal>] AssemblyConfiguration = "Release"
