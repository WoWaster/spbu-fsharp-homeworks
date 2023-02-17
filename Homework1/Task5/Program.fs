let findFirstInList value list =
    let rec findFirstInList list index =
        match list with
        | [] -> None
        | head :: tail ->
            if head = value then
                Some(index)
            else
                findFirstInList tail (index + 1)

    findFirstInList list 0

let l = [ "asjdfnajfd"; "rules"; "bubbles"; "tea" ]
printfn $"""{findFirstInList "tea" l} {findFirstInList "a" l}"""
