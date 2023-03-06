module Tasks.Task1

let takeOnlyEven number =
    if number % 2 = 0 then Some(1) else None

let countEvenNumbers1 list =
    List.sum (List.choose takeOnlyEven list)

let countEvenNumbers2 list =
    list |> List.choose takeOnlyEven |> List.sum

let countEvenNumbers3 = List.choose takeOnlyEven >> List.sum
