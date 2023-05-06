
type Symbol = String

data SExp = Sym Symbol | Cons SExp SExp

symbolOccursInSexp :: Symbol -> SExp -> Bool
symbolOccursInSexp x sexp = case sexp of
    (Sym sym) -> x == sym

    (Cons car cdr) ->
        symbolOccursInSexp x car ||
        symbolOccursInSexp x cdr

replaceSymbolInSExp :: Symbol -> SExp -> SExp -> SExp
replaceSymbolInSExp replaceMe replaceWith sexp = case sexp of
    (Sym sym) -> if sym == replaceMe
        then replaceWith
        else Sym sym
    (Cons car cdr) -> Cons
        (replaceSymbolInSExp replaceMe replaceWith car)
        (replaceSymbolInSExp replaceMe replaceWith cdr)

data Clause
    = Relation Symbol [SExp]
    | Conde [[Clause]]
    | Fresh [Symbol] [Clause]

replaceSymbolInClause :: Symbol -> SExp -> Clause -> Clause
replaceSymbolInClause replaceMe replaceWith clause = case clause of
    Relation name args ->
        Relation name (map (replaceSymbolInSExp replaceMe replaceWith) args)

    Conde conjunctions ->
        Conde (map (map (replaceSymbolInClause replaceMe replaceWith)) conjunctions)

    Fresh vars clauses ->
        if replaceMe `elem` vars
        then Fresh vars clauses
        else Fresh vars (map (replaceSymbolInClause replaceMe replaceWith) clauses)

data Defrel = Defrel Symbol [Symbol] [Clause]
