module Tasks.Task4

let genNextPrime state =
    let rec findNext n =
        if (state |> Seq.exists (fun x -> n % x = 0)) then
            findNext (n + 2) // All primes except 2 are odd, so we can skip even numbers
        else
            n :: state

    if state.Head = 2 then
        3 :: state // We need to add 3 manually, otherwise skip of even numbers breaks
    else
        findNext (state.Head + 2)

let primeGenerator =
    [ 2 ] |> Seq.unfold (fun state -> Some(state.Head, genNextPrime state))
