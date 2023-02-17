let reverseList list =
    let rec reverseList list acc =
        match list with
        | [] -> acc
        | head :: tail -> reverseList tail (head :: acc)

    reverseList list []

let l = [ 1; 2; 3; 4; 5 ]
printfn $"%A{l}"
let lr = reverseList l
printfn $"%A{lr}"
