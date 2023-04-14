module Tasks.Task2

type BinaryTree<'a> =
    | Empty
    | Node of 'a * BinaryTree<'a> * BinaryTree<'a>

let treeMap mapping tree =
    let rec innerMap tree continuation =
        match tree with
        | Empty -> continuation Empty
        | Node(value, leftTree, rightTree) ->
            innerMap leftTree (fun leftTree ->
                innerMap rightTree (fun rightTree -> continuation (Node(mapping value, leftTree, rightTree))))

    innerMap tree id
