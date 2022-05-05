namespace FSharp.CommandLine

open System
open System.Text.RegularExpressions
open FSharp.Scanf
open FSharp.Scanf.Internals.Parser
open FSharp.CommandLine.Internals
open Microsoft.FSharp.Quotations

type ValueFormat<'p,'st,'rd,'rl,'t,'a> = {
  format: PrintfFormat<'p,'st,'rd,'rl,'t>
  paramNames: (string list) option
  handler: 't -> 'a
}
with
  static member inline construct (this: ValueFormat<_,_,_,_,_,_>) =
    let parser s =
      s |> tryKsscanf this.format this.handler
        |> function Ok x -> Some x | _ -> None
    in
    let formatTokens =
      let defaultNames =
        this.format.GetFormatterNames()
          |> List.map (String.replace ' ' '_' >> String.toUpperInvariant)
          |> List.map (sprintf "%s_VALUE")
      in
      let names = (this.paramNames ?| defaultNames) |> List.map (sprintf "<%s>") in
      this.format.PrettyTokenize names
    in
    (parser, formatTokens)

  member this.map ([<ReflectedDefinition>]mapper: Expr<'a -> 'b>) =
    let mf x = (FuncHelper.compileFunc mapper) x in
    let pns = FuncHelper.getFirstArgumentName mapper in
    { format = this.format; handler = this.handler >> mf; paramNames = pns }

  member this.withNames names =
    { this with paramNames = Some names }

  member this.asConst value =
    { format = this.format; handler = (fun _ -> value); paramNames = this.paramNames }

type ValueRegex<'a> = {
  regex: Regex
  handler: string list -> 'a
}
with
  static member construct (this: ValueRegex<_>) =
    let parser (str: string) =
      let m: Match = this.regex.Match(str)
      if m.Success then
        m.Groups
          |> Seq.cast<Group>
          |> List.ofSeq
          |> List.zip (this.regex.GetGroupNames() |> List.ofArray)
          |> List.filter (fun (name, _) -> name |> String.forall Char.IsDigit |> not)
          |> List.map (fun (_, x) -> x.Value)
          |> this.handler |> Some
      else None
    let tokens = [string this.regex]
    (parser, tokens)

  member this.map mapper =
    { regex = this.regex; handler = this.handler >> mapper }

  member this.asConst value =
    { regex = this.regex; handler = fun _ -> value }

type ValueTypedRegex<'a, '_Regex, '_Match > = {
  typedRegex: '_Regex
  handler: '_Match -> 'a
}
with
  static member inline construct (this: ValueTypedRegex<_, ^Regex, ^Match>) : _
    when ^Match :> Match =
    let parser str =
      let m = (^Regex: (member TypedMatch: string -> ^Match) this.typedRegex,str)
      if m.Success then
        this.handler m |> Some
      else None
    let tokens = [string this.typedRegex]
    (parser, tokens)

  member inline this.map mapper =
    { typedRegex = this.typedRegex; handler = this.handler >> mapper }

  member inline this.asConst value =
    { typedRegex = this.typedRegex; handler = fun _ -> value }

[<AutoOpen>]
module OptionValues =
  let inline format (fmt: PrintfFormat<_,_,_,_,'t>) : ValueFormat<_,_,_,_,'t,'t> =
    { format = fmt; handler = id; paramNames = None }

  let inline regex r =
    { regex = Regex(r); handler = id }

  let inline typedRegex< ^Regex, ^Match when ^Regex: (new: unit -> ^Regex) and ^Regex: (member TypedMatch: string -> ^Match) > : ValueTypedRegex< ^Match, ^Regex, ^Match > =
    { typedRegex = new ^Regex(); handler = id }

  let inline asConst value (optionValue: ^X) =
    (^X: (member asConst: _ -> _) optionValue,value)

  let inline internal construct (optionValue: ^X) =
    (^X: (static member construct: _ -> _) optionValue)

