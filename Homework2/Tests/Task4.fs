module Tests.Task4

open NUnit.Framework
open FsUnit
open Tasks.Task4

[<Test>]
let ``Generate 10 first prime number`` () =
    primeGenerator
    |> Seq.take 10
    |> should equal [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29 ]

[<Test>]
let ``2 is a prime`` () =
    primeGenerator |> Seq.take 1 |> should equal [ 2 ]

let primesBelow100 =
    [ 2
      3
      5
      7
      11
      13
      17
      19
      23
      29
      31
      37
      41
      43
      47
      53
      59
      61
      67
      71
      73
      79
      83
      89
      97 ]

[<Test>]
let ``Generate all primes below 100`` () =
    primeGenerator
    |> Seq.takeWhile (fun x -> x < 100)
    |> should equal primesBelow100
