let findFirstInList value list =
    let rec findFirstInList list index =
        match list with
        | [] -> None
        | head :: _ when head = value -> Some index
        | _ :: tail -> findFirstInList tail (index + 1)

    findFirstInList list 0

let l = [ "asjdfnajfd"; "rules"; "bubbles"; "tea" ]
printfn $"""%A{findFirstInList "tea" l} %A{findFirstInList "a" l}"""
