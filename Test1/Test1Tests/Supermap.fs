module Test1Tests.Supermap

open NUnit.Framework
open FsUnit
open Test1.Supermap


[<Test>]
let ``sin and cos`` () =
    supermap [ sin; cos ] [ 1.0; 2.0; 3.0 ]
    |> should equal [ sin 1.0; cos 1.0; sin 2.0; cos 2.0; sin 3.0; cos 3.0 ]


let id x = x
let xSquared x = x * x
let polyn x = x * x + 5 * x + 19

[<Test>]
let ``simple algebraic transforms`` () =
    supermap [ id; xSquared; polyn ] [ 0; 1; 4 ]
    |> should
        equal
        [ id 0
          xSquared 0
          polyn 0
          id 1
          xSquared 1
          polyn 1
          id 4
          xSquared 4
          polyn 4 ]


[<Test>]
let ``transform strings`` () =
    supermap [ Seq.rev ] [ "hello"; "asdf"; "woof" ]
    |> should equal [ Seq.rev "hello"; Seq.rev "asdf"; Seq.rev "woof" ]

[<Test>]
let ``empty mappings`` () =
    supermap [] [ 1; 2; 3 ] |> should equal []

[<Test>]
let ``empty list`` () = supermap [ id ] [] |> should equal []
