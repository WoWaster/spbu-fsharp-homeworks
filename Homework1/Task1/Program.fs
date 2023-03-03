let factorial n =
    let rec factorial n acc =
        if n = 0UL then acc else factorial (n - 1UL) (n * acc)

    factorial n 1UL

printfn $"{factorial 21UL}"
