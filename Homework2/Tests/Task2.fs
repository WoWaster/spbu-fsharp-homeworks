module Tests.Task2

open NUnit.Framework
open FsUnit
open Tasks.Task2

let unsquaredTree =
    Node(
        7,
        Node(1, Node(4, Empty, Empty), Node(3, Empty, Empty)),
        Node(5, Node(9, Node(6, Empty, Empty), Node(8, Empty, Empty)), Node(2, Empty, Empty))
    )

let squaredTree =
    Node(
        49,
        Node(1, Node(16, Empty, Empty), Node(9, Empty, Empty)),
        Node(25, Node(81, Node(36, Empty, Empty), Node(64, Empty, Empty)), Node(4, Empty, Empty))
    )

[<Test>]
let ``Square every element of tree`` () =
    unsquaredTree |> treeMap (fun x -> x * x) |> should equal squaredTree

let stringTree =
    Node("knit", Node("plan", Node("bag", Empty, Empty), Node("assembly", Empty, Empty)), Node("wife", Empty, Empty))

let stringLengthsTree =
    Node(4, Node(4, Node(3, Empty, Empty), Node(8, Empty, Empty)), Node(4, Empty, Empty))

[<Test>]
let ``Map tree from string to int`` () =
    stringTree |> treeMap (fun str -> str.Length) |> should equal stringLengthsTree

[<Test>]
let ``Test on one node tree`` () =
    Node("AbCd", Empty, Empty)
    |> treeMap (fun str -> str.ToLower())
    |> should equal (Node("abcd", Empty, Empty))

[<Test>]
let ``Map Empty tree`` () =
    Empty |> treeMap (fun x -> x * 2) |> should equal BinaryTree<int>.Empty
