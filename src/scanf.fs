(*
The X11 License
scanf.fsx - type safe scanf
Copyright(c) 2018 cannorin
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

module FSharp.CommandLine.Scanf

open System
open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection

let inline private to_s x = x.ToString()

/// Verify that f x, and then return x, otherwise fail witha 'format failure' message
let private check f x = if f x then x else failwithf "format failure \"%s\"" x

let private parseDecimal x = Decimal.Parse(x, System.Globalization.CultureInfo.InvariantCulture)

/// The supported characters for the formatter
let private parsers =
  dict [
    'b', Boolean.Parse >> box
    'd', int64 >> box
    'i', int64 >> box
    's', box
    'u', uint32 >> int64 >> box
    'x', check (String.forall Char.IsLower) >> ((+) "0x") >> int64 >> box
    'X', check (String.forall Char.IsUpper) >> ((+) "0x") >> int64 >> box
    'o', ((+) "0o") >> int64 >> box
    'e', float >> box // no check for correct format for floats
    'E', float >> box
    'f', float >> box
    'F', float >> box
    'g', float >> box
    'G', float >> box
    'M', parseDecimal >> box
    'c', char >> box
  ]

// array of all possible formatters, i.e. [|"%b"; "%d"; ...|]
let private separators =
  parsers.Keys
  |> Seq.map (fun c -> "%" + c.ToString())
  |> Seq.toArray

// Creates a list of formatter characters from a format string,
// for example "(%s,%d)" -> ['s', 'd']
let rec private getFormatters xs =
  match xs with
  | '%' :: '%' :: xr -> getFormatters xr
  | '%' :: x :: xr   ->
    if parsers.ContainsKey x then x :: getFormatters xr
    else failwithf "Unsupported formatter '%%%c'" x
  | _ :: xr          -> getFormatters xr
  | []               -> []

type PrintfFormat<'a,'b,'c,'d,'e> with
  member this.GetFormatterNames () =
    let fs = this.Value.ToCharArray()
             |> Array.toList |> getFormatters in
    let print = function
      | 's' -> "string"
      | 'c' -> "char"
      | 'b' -> "bool"
      | 'i' | 'd' -> "int"
      | 'u' -> "uint"
      | 'x' -> "lowercase hex"
      | 'X' -> "uppercase hex"
      | 'o' -> "octal"
      | 'f' | 'e' | 'E' | 'g' | 'G' -> "double"
      | 'M' -> "decimal"
      | x -> failwithf "Unsupported formatter '%%%c'" x
    in
    fs |> List.map print

  member this.PrettyTokenize names =
    let fcs = this.Value.ToCharArray() |> Array.toList in
    if (List.length names) < (fcs |> getFormatters |> List.length) then
      failwith "Parameter count does not match to the format"
    else
      let rec replace = function
        | [], _ -> []
        | cs, [] ->
          cs |> List.map to_s
        | '%' :: '%' :: cs, ns ->
          replace (cs, ns)
        | '%' :: c :: cs, n :: ns when parsers.ContainsKey c ->
          n :: replace (cs, ns)
        | c :: cs, ns ->
          to_s c :: replace (cs, ns)
      in
      replace (fcs, names)

  member this.PrettyPrint names = this.PrettyTokenize names |> String.concat ""
 
// Coerce integer types from int64
let private coerce o = function
  | v when v = typeof<int32> ->
    int32(unbox<int64> o) |> box
  | v when v = typeof<uint32> ->
    uint32(unbox<int64> o) |> box
  | _ -> o

let kscanf (pf: PrintfFormat<_,_,_,_,'t>) (cont: 't -> 'u) s : 'u =
  let formatStr  = pf.Value
  let constants  = formatStr.Split([|"%%"|], StringSplitOptions.None) 
                   |> Array.map (fun x -> x.Split(separators, StringSplitOptions.None))
  let regexStr   = constants 
                   |> Array.map (fun c -> c |> Array.map Regex.Escape |> String.concat "(.*?)")
                   |> String.concat "%"
  let regex      = Regex("^" + regexStr + "$")
  let formatters = formatStr.ToCharArray() // need original string here (possibly with "%%"s)
                   |> Array.toList |> getFormatters
  let groups =
    regex.Match(s).Groups
    |> Seq.cast<Group>
    |> Seq.skip 1

  let matches =
    (groups, formatters)
    ||> Seq.map2 (fun g f -> g.Value |> parsers.[f])
    |> Seq.toArray

  let value =
    if typeof<'t> = typeof<unit> then
      if s = formatStr then
        box () :?> 't
      else
        failwith "Match failed"
    else if matches.Length = 1 then
      coerce matches.[0] typeof<'t> :?> 't
    else
      let tupleTypes = FSharpType.GetTupleElements(typeof<'t>)
      let matches =
        (matches,tupleTypes)
        ||> Array.map2 ( fun a b -> coerce a b)
      FSharpValue.MakeTuple(matches, typeof<'t>) :?> 't
  
  cont value

let tryKscanf pf cont s =
  try
    kscanf pf cont s |> Ok
  with
    | ex -> Error ex

let sscanf pf s  =
  kscanf pf id s

let trySscanf pf s =
  tryKscanf pf id s

let scanf pf =
  Console.ReadLine() |> sscanf pf

let tryScanf pf =
  Console.ReadLine() |> trySscanf pf

// active pattern
let (|Sscanf|_|) (format:PrintfFormat<_,_,_,_,'t>) input =
  trySscanf format input |> Result.toOption
