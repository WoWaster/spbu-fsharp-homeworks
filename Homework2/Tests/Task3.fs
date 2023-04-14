module Tests.Task3

open FsCheck
open NUnit.Framework
open FsUnit
open Tasks.Task3

let expr1 = Negate(Multiply(Add(Term 3, Subtract(Term 7, Term -2)), Term 5))

[<Test>]
let ``Expression with all operations`` () = evaluate expr1 |> should equal -60

let expr2 = Negate(Negate(Term 1))

[<Test>]
let ``Double negate should be id`` () = evaluate expr2 |> should equal 1

let testIfZero num =
    evaluate (Subtract(Term num, Term num)) = 0

[<Test>]
let ``FsCheck if 'x - x' is always zero`` () = Check.QuickThrowOnFailure testIfZero
