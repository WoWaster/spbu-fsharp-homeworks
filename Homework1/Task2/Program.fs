let fibonacci n =
    let rec fibonacci n a b =
        if n = 0 then a else fibonacci (n - 1) b (a + b)

    fibonacci n 0L 1L

[ 0..100 ] |> List.iter (fun x -> printfn $"fibonacci({x}): {fibonacci x}")
