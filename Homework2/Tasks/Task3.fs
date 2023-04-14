module Tasks.Task3

type ArithmeticExpression =
    | Term of int
    | Add of ArithmeticExpression * ArithmeticExpression
    | Subtract of ArithmeticExpression * ArithmeticExpression
    | Multiply of ArithmeticExpression * ArithmeticExpression
    | Negate of ArithmeticExpression

let evaluate arithmeticExpression =
    let rec evaluate arithmeticExpression continuation =
        match arithmeticExpression with
        | Term value -> continuation value
        | Add(leftTerm, rightTerm) ->
            evaluate leftTerm (fun leftTerm ->
                evaluate rightTerm (fun rightTerm -> continuation (leftTerm + rightTerm)))
        | Subtract(leftTerm, rightTerm) ->
            evaluate leftTerm (fun leftTerm ->
                evaluate rightTerm (fun rightTerm -> continuation (leftTerm - rightTerm)))
        | Multiply(leftTerm, rightTerm) ->
            evaluate leftTerm (fun leftTerm ->
                evaluate rightTerm (fun rightTerm -> continuation (leftTerm * rightTerm)))
        | Negate term -> evaluate term (fun term -> continuation -term)

    evaluate arithmeticExpression id
