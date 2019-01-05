(*
The MIT License
Scanf.fs - type safe scanf
Copyright(c) 2018-2019 cannorin
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

// original: http://www.fssnip.net/4I/title/sscanf-parsing-with-format-strings

/// Scanf functions. If the result type is 7-tuple or less,
/// consider using `FSharp.Scanf.Optimized` module instead.
module FSharp.Scanf

open System
open System.IO
open Microsoft.FSharp.Reflection

let inline internal to_s x = x.ToString()

let inline internal check f x = if f x then x else failwithf "format failure \"%s\"" x

let inline internal parseDecimal x = Decimal.Parse(x, System.Globalization.CultureInfo.InvariantCulture)

module Internal =
  [<Literal>]
  let parserChars = "bdisuxXoeEfFgGMc"
  
  let inline internal formatIntegerStr str fmt =
    match fmt with
      | 'i' | 'd' | 'u' -> str
      | 'x' -> str |> check (String.forall Char.IsLower) |> ((+) "0x")
      | 'X' -> str |> check (String.forall Char.IsUpper) |> ((+) "0x")
      | 'o' -> "0o" + str
      | _ -> str

  let inline internal convertUnsafe fmt targetType str =
    let str = formatIntegerStr str fmt
    if targetType = typeof<string> then box str
    else if targetType = typeof<int32> then int32 str |> box
    else if targetType = typeof<float> then float str |> box
    else if targetType = typeof<char> then char str |> box
    else if targetType = typeof<bool> then Boolean.Parse(str) |> box
    else if targetType = typeof<int8> then int8 str |> box
    else if targetType = typeof<uint8> then uint8 str |> box
    else if targetType = typeof<int16> then int16 str |> box
    else if targetType = typeof<uint16> then uint16 str |> box
    else if targetType = typeof<uint32> then uint32 str |> box
    else if targetType = typeof<int64> then int64 str |> box
    else if targetType = typeof<uint64> then uint64 str |> box
    else if targetType = typeof<float32> then float32 str |> box
    else if targetType = typeof<decimal> then parseDecimal str |> box
    else if targetType = typeof<unit> then box ()
    else if targetType = typeof<bigint> then bigint.Parse str |> box
    else failwithf "Unsupported type '%s'" targetType.Name

  // type wrapper
  [<Struct>]
  type ty<'t> = Ty
  let inline internal ty<'t> : ty<'t> = Ty

  // Compile-time resolved string-to-value parsers
  type OptimizedConverter =
    static member inline Convert (_: ty<unit>, _, _) = ()
    static member inline Convert (_: ty<bool>, s: string list, _) = Boolean.Parse(s.Head)
    static member inline Convert (_: ty<string>, s: string list, _) = s.Head
    static member inline Convert (_: ty<char>, s:string list, _) = char s.Head
    static member inline Convert (_: ty<int8>, s: string list, formatters: char list) =
      formatIntegerStr s.Head formatters.Head |> int8
    static member inline Convert (_: ty<uint8>, s: string list, formatters: char list) =
      formatIntegerStr s.Head formatters.Head |> uint8
    static member inline Convert (_: ty<int16>, s: string list, formatters: char list) =
      formatIntegerStr s.Head formatters.Head |> int16
    static member inline Convert (_: ty<uint16>, s: string list, formatters: char list) =
      formatIntegerStr s.Head formatters.Head |> uint16
    static member inline Convert (_: ty<int32>, s: string list, formatters: char list) =
      formatIntegerStr s.Head formatters.Head |> int32
    static member inline Convert (_: ty<uint32>, s: string list, formatters: char list) =
      formatIntegerStr s.Head formatters.Head |> uint32
    static member inline Convert (_: ty<int64>, s: string list, formatters: char list) =
      formatIntegerStr s.Head formatters.Head |> int64
    static member inline Convert (_: ty<uint64>, s: string list, formatters: char list) =
      formatIntegerStr s.Head formatters.Head |> uint64
    static member inline Convert (_: ty<bigint>, s: string list, formatters: char list) =
      formatIntegerStr s.Head formatters.Head |> bigint.Parse
    static member inline Convert (_: ty<float>, s:string list, _) = float s.Head
    static member inline Convert (_: ty<float32>, s:string list, _) = float32 s.Head
    static member inline Convert (_: ty<decimal>, s:string list, _) = parseDecimal s.Head

  let inline internal convertFast (typ: ty< ^t >) (s: string list) (formatter:char list) =
    let inline call_2 (_: ty< ^Converter >, _: ty< ^x >) =
      ((^Converter or ^x): (static member Convert: _*_*_ -> ^t) typ,s,formatter)
    let inline call   (a: ty<'a>, b: ty<'b>) = call_2 (a, b)
    call (ty<OptimizedConverter>, typ)

  // 8-tuples or more are reprensented as `System.Tuple'7<_,_,_,_,_,_,_,System.Tuple<...>>`
  // but it is impossible to handle them generically
  type OptimizedConverter with
    static member inline Convert (_: ty<'t1*'t2>, s: string list, fs: char list) =
      convertFast ty<'t1> s fs,
      convertFast ty<'t2> s.Tail fs.Tail
    static member inline Convert (_: ty<'t1*'t2*'t3>, s: string list, fs: char list) =
      convertFast ty<'t1> s fs,
      convertFast ty<'t2> s.Tail fs.Tail,
      convertFast ty<'t3> s.Tail.Tail fs.Tail.Tail
    static member inline Convert (_: ty<'t1*'t2*'t3*'t4>, s: string list, fs: char list) =
      convertFast ty<'t1> s fs,
      convertFast ty<'t2> s.Tail fs.Tail,
      convertFast ty<'t3> s.Tail.Tail fs.Tail.Tail,
      convertFast ty<'t4> s.Tail.Tail.Tail fs.Tail.Tail.Tail
    static member inline Convert (_: ty<'t1*'t2*'t3*'t4*'t5>, s: string list, fs: char list) =
      convertFast ty<'t1> s fs,
      convertFast ty<'t2> s.Tail fs.Tail,
      convertFast ty<'t3> s.Tail.Tail fs.Tail.Tail,
      convertFast ty<'t4> s.Tail.Tail.Tail fs.Tail.Tail.Tail,
      convertFast ty<'t5> s.Tail.Tail.Tail.Tail fs.Tail.Tail.Tail.Tail
    static member inline Convert (_: ty<'t1*'t2*'t3*'t4*'t5*'t6>, s: string list, fs: char list) =
      convertFast ty<'t1> s fs,
      convertFast ty<'t2> s.Tail fs.Tail,
      convertFast ty<'t3> s.Tail.Tail fs.Tail.Tail,
      convertFast ty<'t4> s.Tail.Tail.Tail fs.Tail.Tail.Tail,
      convertFast ty<'t5> s.Tail.Tail.Tail.Tail fs.Tail.Tail.Tail.Tail,
      convertFast ty<'t6> s.Tail.Tail.Tail.Tail.Tail fs.Tail.Tail.Tail.Tail.Tail
    static member inline Convert (_: ty<'t1*'t2*'t3*'t4*'t5*'t6*'t7>, s: string list, fs: char list) =
      convertFast ty<'t1> s fs,
      convertFast ty<'t2> s.Tail fs.Tail,
      convertFast ty<'t3> s.Tail.Tail fs.Tail.Tail,
      convertFast ty<'t4> s.Tail.Tail.Tail fs.Tail.Tail.Tail,
      convertFast ty<'t5> s.Tail.Tail.Tail.Tail fs.Tail.Tail.Tail.Tail,
      convertFast ty<'t6> s.Tail.Tail.Tail.Tail.Tail fs.Tail.Tail.Tail.Tail.Tail,
      convertFast ty<'t7> s.Tail.Tail.Tail.Tail.Tail.Tail fs.Tail.Tail.Tail.Tail.Tail.Tail

  // Creates a list of formatter characters from a format string,
  // for example "(%s,%d)" -> ['s', 'd']
  let rec internal getFormatters xs =
    match xs with
    | '%' :: '%' :: xr -> getFormatters xr
    | '%' :: x :: xr   ->
      if parserChars |> Seq.contains x then x :: getFormatters xr
      else failwithf "Unsupported formatter '%%%c'" x
    | _ :: xr          -> getFormatters xr
    | []               -> []

  [<Struct; RequireQualifiedAccess>]
  type FormatStringPart =
    | Placeholder of typ:char
    | Literal of string
    | Space
    static member getFormatters(fmt: FormatStringPart list) =
      let rec get = function
        | Placeholder c :: rest -> c :: get rest
        | _ :: rest -> get rest
        | [] -> []
      get fmt

  let inline internal (<+>) h t =  
    match h, t with
      | FormatStringPart.Literal s, FormatStringPart.Literal t :: rest ->
        FormatStringPart.Literal (s+t) :: rest
      | _ -> h :: t

  let rec internal parsePlaceholderImpl currentPos (str: string) =
    let c = str.[currentPos]
    let nextPos = currentPos + 1
    if Char.IsLetter c then
      FormatStringPart.Placeholder c :: parseFormatImpl nextPos nextPos str
    else if c = '%' then
      FormatStringPart.Literal "%" <+> parseFormatImpl nextPos nextPos str
    else failwithf "Unsupported formatter '%%%c'" c

  and internal parseFormatImpl startPos currentPos (str: string) =
    if currentPos >= str.Length then
      if currentPos = startPos then []
      else
        let s = str.Substring(startPos, currentPos - startPos)
        FormatStringPart.Literal s :: []
    else
      let c = str.[currentPos]
      if c = '%' then
        let nextPos = currentPos + 1
        if currentPos = startPos then
          parsePlaceholderImpl nextPos str
        else
          let s = str.Substring(startPos, currentPos - startPos)
          FormatStringPart.Literal s <+> parsePlaceholderImpl nextPos str
      else if c = ' ' || c = '\n' || c = '\r' || c = '\t' then
        let mutable i = 1
        while currentPos + i < str.Length
           && let c = str.[currentPos + i] in
           c = ' ' || c = '\n' || c = '\r' || c = '\t' do i <- i+1
        let nextPos = currentPos + i 
        if currentPos = startPos then
          FormatStringPart.Space :: parseFormatImpl nextPos nextPos str
        else
          let s = str.Substring(startPos, currentPos - startPos)
          FormatStringPart.Literal s :: FormatStringPart.Space :: parseFormatImpl nextPos nextPos str    
      else
        parseFormatImpl startPos (currentPos + 1) str

  let inline internal parseFormat (str: string) =
    parseFormatImpl 0 0 str

  open FParsec
  let inline (<++>) p1 p2 = p1 .>>. p2 |>> List.Cons
  let inline strOf p = withSkippedString (fun s _ -> s) p

  let rec internal buildParser = function
    | [] -> eof >>% []
    | FormatStringPart.Space :: rest ->
      spaces >>. buildParser rest
    | FormatStringPart.Literal lit :: rest ->
      skipString lit >>. buildParser rest
    | FormatStringPart.Placeholder c :: rest->
      let cont = buildParser rest
      match c with
        | 'b' -> (pstring "true" <|> pstring "false") <++> cont
        | 'd' | 'i' -> many1Satisfy isDigit <++> cont
        | 's' ->
          manyCharsTill anyChar (followedBy cont) .>>.? cont |>> List.Cons
        | 'u' -> strOf puint64 <++> cont
        | 'x' | 'X' -> manySatisfy isHex <++> cont
        | 'o' -> manySatisfy isOctal <++> cont
        | 'e' | 'E' | 'f' | 'F' | 'g' | 'G' ->
          (skipStringCI "nan" >>% "NaN")
          <|> (skipStringCI "infinity" <|> skipStringCI "inf" >>% "Infinity")
          <|> (strOf pfloat)
          <++> cont
        | 'M' ->
          many1Satisfy isDigit .>>.? opt (skipChar '.' >>? many1Satisfy isDigit)
          |>> (fun (i, j) -> i + Option.defaultValue "" j)
          <++> cont
        | 'c' -> anyChar |>> string <++> cont
        | c -> failwithf "Unsupported formatter '%%%c'" c

  let inline internal matchFormat fmt fmtStr str =
    match run (buildParser fmt) str with
      | Success (xs, _, _) -> xs
      | Failure (msg, _, _) ->
        failwithf "the input does not match the format '%s': %s" fmtStr msg
  
  // Extracts string matches and the format from a format string and a given string.
  let getMatchesAndFormat (pf: PrintfFormat<_, _, _, _, _>) s =
    let formatStr  = pf.Value
    let fmt = parseFormat formatStr
    let groups = matchFormat fmt formatStr s
    let formatters = FormatStringPart.getFormatters fmt
    groups, formatStr, formatters

open Internal

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
        | '%' :: c :: cs, n :: ns when parserChars |> Seq.contains c ->
          n :: replace (cs, ns)
        | c :: cs, ns ->
          to_s c :: replace (cs, ns)
      in
      replace (fcs, names)

  member this.PrettyPrint names = this.PrettyTokenize names |> String.concat ""

let ksscanf (pf: PrintfFormat<_,_,_,_,'t>) (cont: 't -> 'u) s : 'u =
  let matches, formatStr, formatters = getMatchesAndFormat pf s
  let value =
    if typeof<'t> = typeof<unit> then
      if s = formatStr then
        box () :?> 't
      else
        failwith "Match failed"
    else
      let targetTypes =
        if matches.Length = 1 then Array.singleton typeof<'t>
        else FSharpType.GetTupleElements(typeof<'t>)
      let values =
        (formatters, targetTypes, matches)
        |||> Seq.map3 convertUnsafe
        |> Seq.toArray
      FSharpValue.MakeTuple(values, typeof<'t>) :?> 't
  cont value

let inline tryKsscanf pf cont s =
  try
    ksscanf pf cont s |> Ok
  with
    | ex -> Error ex
    
let inline sscanf pf s  =
  ksscanf pf id s

let inline trySscanf pf s =
  tryKsscanf pf id s

let inline scanfn pf =
  Console.ReadLine() |> sscanf pf

let inline tryScanfn pf =
  Console.ReadLine() |> trySscanf pf
  
let inline kscanfn pf cont =
  ksscanf pf cont <| Console.ReadLine()

let inline tryKscanfn pf cont =
  tryKsscanf pf cont <| Console.ReadLine()

let inline fscanfn pf (tr: TextReader) =
  tr.ReadLine() |> sscanf pf

let inline tryFscanfn pf (tr: TextReader) =
  tr.ReadLine() |> trySscanf pf

let inline kfscanfn pf cont (tr: TextReader) =
  ksscanf pf cont <| tr.ReadLine()

let inline tryKfscanfn pf cont (tr: TextReader) =
  tryKsscanf pf cont <| tr.ReadLine()

// active pattern
let (|Sscanf|_|) (format:PrintfFormat<_,_,_,_,'t>) input =
  trySscanf format input |> function | Ok x -> Some x | Error _ -> None

/// Scanf functions, no reflection/boxing.
/// 
/// About 6x-7x faster than unoptimized ones.
/// 
/// Can only be used with up to 7 captures (i.e. the result type must be up to 7-tuples).
module Optimized =
  let inline ksscanf (pf: PrintfFormat<_,_,_,_,^t>) (cont: ^t -> 'u) s : 'u =
    let matches, _, formatters = getMatchesAndFormat pf s
    let strings = matches |> Seq.toList
    convertFast ty< ^t > strings formatters |> cont
  let inline tryKsscanf pf cont s =
    try
      ksscanf pf cont s |> Ok
    with
      | ex -> Error ex
  let inline sscanf pf s  =
    ksscanf pf id s
  let inline trySscanf pf s =
    tryKsscanf pf id s
  let inline scanfn pf =
    Console.ReadLine() |> sscanf pf
  let inline tryScanfn pf =
    Console.ReadLine() |> trySscanf pf
  let inline kscanfn pf cont =
    ksscanf pf cont <| Console.ReadLine()
  let inline tryKscanfn pf cont =
    tryKsscanf pf cont <| Console.ReadLine()
  let inline fscanfn pf (tr: TextReader) =
    tr.ReadLine() |> sscanf pf
  let inline tryFscanfn pf (tr: TextReader) =
    tr.ReadLine() |> trySscanf pf
  let inline kfscanfn pf cont (tr: TextReader) =
    ksscanf pf cont <| tr.ReadLine()
  let inline tryKfscanfn pf cont (tr: TextReader) =
    tryKsscanf pf cont <| tr.ReadLine()
