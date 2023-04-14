module Tests.Task1

open NUnit.Framework
open FsCheck
open Tasks.Task1

let countOnlyEvenNumbers list =
    (countEvenNumbers1 list = countEvenNumbers2 list)
    && (countEvenNumbers2 list = countEvenNumbers3 list)

[<Test>]
let ``FsCheck three variant of countOnlyEvenNumbers`` () =
    Check.QuickThrowOnFailure countOnlyEvenNumbers
