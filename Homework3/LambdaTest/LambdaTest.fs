module LambdaTest

open NUnit.Framework
open FsUnit
open LambdaInterpreter

let k name1 name2 = Abs(name1, Abs(name2, Var(name1)))
let kStar name1 name2 = Abs(name1, Abs(name2, Var(name2)))

let s name1 name2 name3 =
    Abs(name1, Abs(name2, Abs(name3, App(App(Var(name1), Var(name3)), App(Var(name2), Var(name3))))))

let i name = Abs(name, Var(name))

let omega name = Abs(name, App(Var(name), Var(name)))

let bigOmega name = App(omega name, omega name)


[<Test>]
let ``S K K = I`` () =
    Lambda.eval (App(App(s "x" "y" "z", k "x" "y"), k "x" "y"))
    |> should equal (i "z")

[<Test>]
let ``Ω doesn't have normal form`` () =
    Lambda.eval (bigOmega "x") |> should equal (bigOmega "x")

let toRename = App(Abs("x", Abs("y", App(Var("x"), Var("y")))), Var "y")

[<Test>]
let ``Variable rename does occur`` () =
    Lambda.eval toRename |> should equal (Abs("'y", App(Var "y", Var "'y")))

[<Test>]
let ``K I = K_*`` () =
    Lambda.eval (App(k "x" "y", i "x")) |> should equal (kStar "y" "x")

[<Test>]
let ``ω I = I`` () =
    Lambda.eval (App(omega "s", i "x")) |> should equal (i "x")
