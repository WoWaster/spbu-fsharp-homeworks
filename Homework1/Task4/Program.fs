let arrayOfPowersOfTwo n m =
    [ for power in n .. (n + m) -> 1UL <<< power ]

arrayOfPowersOfTwo 48 15 |> printfn "%A"
