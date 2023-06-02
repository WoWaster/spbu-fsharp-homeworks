module ComputationExpressions.Rounding

open System

type RoundingBuilder(nOfSignificantDigits: int) =
    member this.nOfSignificantDigits = nOfSignificantDigits

    member this.Bind(x: float, f: _ -> float) = f (Math.Round(x, nOfSignificantDigits))
    member this.Return(x) = x
    member this.ReturnFrom(x: float) = Math.Round(x, nOfSignificantDigits)

let rounding nOfSignificantDigits = RoundingBuilder(nOfSignificantDigits)
