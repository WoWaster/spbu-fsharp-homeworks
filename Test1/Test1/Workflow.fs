module Test1.Workflow

open FSharp.Collections

type OptionBuilder() =
    member _.Bind(v, f) = Option.bind f v
    member _.Return v = Some v
    member _.Zero() = None

let opt = OptionBuilder()

type binOp =
    | Add
    | Sub
    | Mul
    | Div

type expr =
    | Var of string
    | Const of int
    | BinOp of binOp * expr * expr


let rec eval (values: Map<string, int>) expression =
    match expression with
    | Var name ->
        opt {
            let! value = Map.tryFind name values
            return value
        }
    | Const value -> opt { return value }
    | BinOp(binOp, expr, expr1) ->
        opt {
            let! lBranch = eval values expr
            let! rBranch = eval values expr1

            match binOp with
            | Add -> return (lBranch + rBranch)
            | Sub -> return (lBranch - rBranch)
            | Mul -> return (lBranch * rBranch)
            | Div ->
                if rBranch <> 0 then
                    return (lBranch / rBranch)
        }
