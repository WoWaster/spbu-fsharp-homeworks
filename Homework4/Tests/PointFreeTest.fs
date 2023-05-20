module Tests.PointFreeTest

open NUnit.Framework
open FsUnit
open PointFree
open FsCheck

let funcList = [ func; func'1; func'2; func'3; func'4 ]

let checkIfEqualFunctions x l =
    (List.distinct (List.map (fun f -> f x l) funcList)).Length = 1

[<Test>]
let ``FsCheck equality of map functions`` () =
    Check.QuickThrowOnFailure checkIfEqualFunctions
