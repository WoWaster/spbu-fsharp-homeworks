module ComputationExpressionsTest

open NUnit.Framework
open ComputationExpressions.Rounding
open FsUnit

[<Test>]
let ``2.0 / 12.0 / 3.5 is approximately 0.048`` () =
    rounding 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return! a / b
    }
    |> should (equalWithin 0.001) 0.048

[<Test>]
let ``1/3 * 2 is approximately 0.66666`` () =
    rounding 5 {
        let! a = 1.0 / 3.0
        return! a * 2.0
    }
    |> should (equalWithin 0.00001) 0.66666

[<Test>]
let ``1.0*(0.5-0.4-0.1) = 0`` () =
    rounding 3 { return! 1.0 * (0.5 - 0.4 - 0.1) } |> should (equalWithin 0.001) 0
