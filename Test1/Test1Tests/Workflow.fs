module Test1Tests.Workflow

open NUnit.Framework
open FsUnit
open Test1.Workflow

[<Test>]
let ``1/1 = 1``() =
    eval (Map.ofList [("x", 1)]) (BinOp(Div, Const 1, Var "x")) |> should equal (Some 1)
    
[<Test>]
let ``division by zero`` () =
    eval (Map.ofList [("x", 0)]) (BinOp(Div, Const 1, Var "x")) |> should equal None
    
[<Test>]
let ``no value in Map`` () =
    eval (Map.ofList []) (BinOp(Div, Const 1, Var "x")) |> should equal None
    