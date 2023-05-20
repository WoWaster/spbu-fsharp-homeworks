module Test1.Supermap

/// Applies every function from the list of mapping to the given element
let map' element mappings =
    let rec helper acc mappings =
        match mappings with
        | [] -> acc
        | head :: tail -> helper ((head element) :: acc) tail

    helper [] mappings

/// Applies every function from the list of mappings to all elements from the given list
let supermap mappings list =
    let rec helper acc list =
        match list with
        | [] -> acc
        | head :: tail -> helper ((map' head mappings) @ acc) tail

    helper [] list |> List.rev
