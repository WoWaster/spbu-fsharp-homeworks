module Tasks.Task4

let genNextPrime state =
    let rec findNext n =
        if (state |> Seq.exists (fun x -> n % x = 0I)) then
            findNext (n + 2I) // All primes except 2 are odd, so we can skip even numbers
        else
            n :: state

    if state.Head = 2I then
        3I :: state // We need to add 3 manually, otherwise skip of even numbers breaks
    else
        findNext (state.Head + 2I)

let primeGenerator =
    [  2I ] |> Seq.unfold (fun state -> Some(state.Head, genNextPrime state))
