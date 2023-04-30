
data SExp = Sym Symbol | Cons SExp SExp

symbolOccursInSexp :: Symbol -> SExp -> Bool
symbolOccursInSexp sym sexp = case sexp of
    (Cons car cdr) ->  (
        symbolOccursInSexp sym car ||
        symbolOccursInSexp sym cdr
    )

    (Sym s) -> sym == s

data Clause
    = Relation
        Symbol
        [SExp]
    
    
    | Conde
        [[Clause]]
    
    
    | Fresh
        [Symbol]
        [Clause]

data Defrel = Defrel
    Symbol
    [Symbol]
    [Clause]
