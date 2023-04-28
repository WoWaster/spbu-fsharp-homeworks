module Test1Tests.RhombusGenerator

open NUnit.Framework
open FsUnit
open Test1.RhombusGenerator

[<Test>]
let ``Rhombus 4`` () =
    generateRhombus 4
    |> should equal [ "   *   "; "  * *  "; " *   * "; "*     *"; " *   * "; "  * *  "; "   *   " ]

[<Test>]
let ``Rhombus 5`` () =
    generateRhombus 5
    |> should
        equal
        [ "    *    "
          "   * *   "
          "  *   *  "
          " *     * "
          "*       *"
          " *     * "
          "  *   *  "
          "   * *   "
          "    *    " ]
