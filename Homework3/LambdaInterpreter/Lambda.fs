namespace LambdaInterpreter

type Term =
    | Var of string
    | Abs of string * Term
    | App of Term * Term

module Lambda =
    let getFreeVars term =
        let rec helper term =
            match term with
            | Var name -> Set.empty.Add(name)
            | Abs (name, innerTerm) -> (helper innerTerm).Remove(name)
            | App (leftTerm, rightTerm) -> Set.union (helper leftTerm) (helper rightTerm)

        helper term

    let getFreshVar varName vars =
        let rec helper varName =
            match Set.contains varName vars with
            | true -> helper ("'" + varName)
            | false -> varName

        helper varName

    let substituteTerm term varName subTerm =
        let rec helper term =
            match term with
            | Var name when name = varName -> subTerm
            | Var name -> Var name
            | Abs (name, innerTerm) when name = varName -> Abs(name, innerTerm)
            | Abs (name, innerTerm) -> Abs(name, helper innerTerm)
            | App (leftTerm, rightTerm) -> App(helper leftTerm, helper rightTerm)

        helper term

    let renameVars term vars =
        let rec helper term =
            match term with
            | Abs (name, innerTerm) when Set.contains name vars ->
                let newVarName = getFreshVar name vars
                let subTerm = substituteTerm innerTerm name (Var newVarName)
                Abs(newVarName, helper subTerm)
            | Abs (name, innerTerm) -> Abs(name, helper innerTerm)
            | App (leftTerm, rightTerm) -> App(helper leftTerm, helper rightTerm)
            | term -> term

        helper term

    let eval term =
        let rec helper term seenTerms =
            match Set.contains term seenTerms with
            | true -> term
            | false ->
                match term with
                | Var name -> Var name
                | Abs (name, innerTerm) -> Abs(name, helper innerTerm (seenTerms.Add(term)))
                | App (Abs (name, innerTerm), rightTerm) ->
                    let newTerm =
                        substituteTerm (renameVars innerTerm (getFreeVars rightTerm)) name rightTerm

                    helper newTerm (seenTerms.Add(term))
                | App (App _ as leftTerm, rightTerm) ->
                    helper (App(helper leftTerm (seenTerms.Add(term)), rightTerm)) (seenTerms.Add(term))
                | App (leftTerm, rightTerm) -> App(leftTerm, helper rightTerm (seenTerms.Add(term)))

        helper term Set.empty
