module LambdaTest

open NUnit.Framework
open FsUnit
open LambdaInterpreter

let K name1 name2 = Abs(name1, Abs(name2, Var(name1)))
let K_star name1 name2 = Abs(name1, Abs(name2, Var(name2)))

let S name1 name2 name3=
    Abs(name1, Abs(name2, Abs(name3, App(App(Var(name1), Var(name3)), App(Var(name2), Var(name3))))))

let I name = Abs(name, Var(name))

let omega name = Abs(name, App(Var(name), Var(name)))

let Omega name = App(omega name, omega name )


[<Test>]
let ``S K K = I`` () = Lambda.eval (App(App(S "x" "y" "z", K "x" "y" ), K "x" "y" )) |> should equal (I "z")

[<Test>]
let ``Ω doesn't have normal form`` () = Lambda.eval (Omega "x") |> should equal (Omega "x")

let toRename = App(Abs("x", Abs("y", App(Var("x"), Var("y")))), Var "y")

[<Test>]
let ``Variable rename does occur`` () = Lambda.eval toRename |> should equal (Abs ("'y", App (Var "y", Var "'y")))

[<Test>]
let ``K I = K_*`` () = Lambda.eval (App(K "x" "y", I "x")) |> should equal (K_star "y" "x")

[<Test>]
let ``ω I = I``() = Lambda.eval (App(omega "s", I "x")) |> should equal (I "x")