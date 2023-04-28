module Test1.RhombusGenerator

/// Generates row of rhombus
let generateRow sideLength row =
    String.init (2 * sideLength - 1) (fun col ->
        if ((abs row) + (abs (col - sideLength + 1)) = (sideLength - 1)) then
            "*"
        else
            " ")


/// Generates rhombus with given side length
let generateRhombus sideLength =
    let rec helper row acc =
        if row = sideLength then
            acc
        else
            helper (row + 1) ((generateRow sideLength row) :: acc)

    helper (-sideLength + 1) []
