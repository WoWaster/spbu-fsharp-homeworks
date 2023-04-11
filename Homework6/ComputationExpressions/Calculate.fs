module ComputationExpressions.Calculate

open System

let toInt32 (str: string) =
    match Int32.TryParse str with
    | true, number -> Some number
    | _ -> None

type CalculateBuilder() =
    member this.Bind(x, f) =
        match toInt32 x with
        | None -> None
        | Some num -> f num

    member this.Return(x) = Some x

let calculate = CalculateBuilder()
