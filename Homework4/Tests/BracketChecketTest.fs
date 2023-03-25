module Tests.BracketChecketTest

open NUnit.Framework
open FsUnit
open BracketChecker

[<Test>]
let ``{}[]()`` () = checkIfValid "{}[]()" |> should be True

[<Test>]
let ``}{`` () = checkIfValid "}{}" |> should be False

let longLaTeXLine =
    """f(z) =
\frac{1}{2 \pi i} \int_{\overrightarrow{\gamma_r}}
\frac{f(\zeta)}{\zeta - a} d\zeta +
\frac{1}{2 \pi i} \int_{\overrightarrow{\gamma_r}}
f(\zeta) \left(\frac{1}{\zeta - a}
\sum_{n = 1 }^{\infty} \left(\frac{z - a}{\zeta -a}\right)^n\right)d\zeta"""

[<Test>]
let ``Long LaTeX Line`` () =
    checkIfValid longLaTeXLine |> should be True

[<Test>]
let ``[{(})]`` () =
    checkIfValid "[{(})]" |> should be False
