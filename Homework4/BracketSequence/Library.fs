module BracketChecker

let bracketTypes = Map [ '}', '{'; ')', '('; ']', '[' ]

let checkIfValid str =
    let rec helper str (acc: char list) =
        match str with
        | [] -> acc.IsEmpty
        | head :: tail when Seq.contains head bracketTypes.Values -> helper tail (head :: acc)
        | head :: tail when Seq.contains head bracketTypes.Keys ->
            match acc with
            | [] -> false
            | accHead :: accTail when accHead = bracketTypes[head] -> helper tail accTail
            | _ -> false
        | _ :: tail -> helper tail acc

    helper (Seq.toList str) []
