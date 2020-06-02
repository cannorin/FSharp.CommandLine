namespace FSharp.CommandLine

[<AutoOpen>]
module InternalExtensions =
  open Microsoft.FSharp.Quotations
  open Microsoft.FSharp.Quotations.Patterns
  open Microsoft.FSharp.Linq.RuntimeHelpers

  type FuncHelper = private | FuncHelper with
    static member compileFunc (x: Expr<'a -> 'b>) =
      LeafExpressionConverter.EvaluateQuotation x :?> ('a -> 'b)
    
    static member getFirstArgumentName (x: Expr<'a -> 'b>) =
      let rec gn tupleArgName = function
        | Let (v, TupleGet(Var tn, index), body) when (tupleArgName = tn.Name) ->
          (index, v.Name) :: gn tupleArgName body
        | _ -> []
      in
      match x with
        | Lambda (v, e) ->
          match (gn v.Name e) with
            | [] -> Some [v.Name]
            | xs -> xs |> List.sortBy fst |> List.map snd |> Some
        | _ -> None

