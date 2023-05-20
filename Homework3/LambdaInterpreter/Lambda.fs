namespace LambdaInterpreter

type Term =
    | Var of string
    | Abs of string * Term
    | App of Term * Term

module Lambda =
    let rec getFreeVars term =
        match term with
        | Var name -> Set.singleton name
        | Abs(name, innerTerm) -> (getFreeVars innerTerm).Remove(name)
        | App(leftTerm, rightTerm) -> Set.union (getFreeVars leftTerm) (getFreeVars rightTerm)

    let rec getFreshVar varName vars =
        match Set.contains varName vars with
        | true -> getFreshVar ("'" + varName) vars
        | false -> varName

    let substituteTerm term varName subTerm =
        let rec helper term =
            match term with
            | Var name when name = varName -> subTerm
            | Var _ -> term
            | Abs(name, _) when name = varName -> term
            | Abs(name, innerTerm) -> Abs(name, helper innerTerm)
            | App(leftTerm, rightTerm) -> App(helper leftTerm, helper rightTerm)

        helper term

    let renameVars term vars =
        let rec helper term =
            match term with
            | Abs(name, innerTerm) when Set.contains name vars ->
                let newVarName = getFreshVar name vars
                let subTerm = substituteTerm innerTerm name (Var newVarName)
                Abs(newVarName, helper subTerm)
            | Abs(name, innerTerm) -> Abs(name, helper innerTerm)
            | App(leftTerm, rightTerm) -> App(helper leftTerm, helper rightTerm)
            | _ -> term

        helper term

    /// Return pair of type Term * bool, where Term is the result of the reduction
    /// and bool indicates whether result is in normal form or not
    let eval term =
        let rec helper term seenTerms =
            match Set.contains term seenTerms with
            | true -> (term, false)
            | false ->
                match term with
                | Var _ -> (term, true)
                | Abs(name, innerTerm) -> (Abs(name, fst (helper innerTerm (seenTerms.Add(term)))), true)
                | App(Abs(name, innerTerm), rightTerm) ->
                    let newTerm =
                        substituteTerm (renameVars innerTerm (getFreeVars rightTerm)) name rightTerm

                    helper newTerm (seenTerms.Add(term))
                | App(App _ as leftTerm, rightTerm) ->
                    helper (App(fst (helper leftTerm (seenTerms.Add(term))), rightTerm)) (seenTerms.Add(term))
                | App(leftTerm, rightTerm) -> (App(leftTerm, fst (helper rightTerm (seenTerms.Add(term)))), true)

        helper term Set.empty
