module Tests.Task4

open NUnit.Framework
open FsUnit
open Tasks.Task4

[<Test>]
let ``Generate 10 first prime number`` () =
    primeGenerator
    |> Seq.take 10
    |> should equal [ 2I; 3I; 5I; 7I; 11I; 13I; 17I; 19I; 23I; 29I ]

[<Test>]
let ``2 is a prime`` () =
    primeGenerator |> Seq.take 1 |> should equal [ 2I ]

let primesBelow100 =
    [ 2I
      3I
      5I
      7I
      11I
      13I
      17I
      19I
      23I
      29I
      31I
      37I
      41I
      43I
      47I
      53I
      59I
      61I
      67I
      71I
      73I
      79I
      83I
      89I
      97I ]

[<Test>]
let ``Generate all primes below 100`` () =
    primeGenerator
    |> Seq.takeWhile (fun x -> x < 100I)
    |> should equal primesBelow100
