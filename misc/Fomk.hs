import Data.Set (Set, singleton, union, empty)

data Disj
    = CC Conj Conj
    | CD Conj Disj
    | CB Conj Bare
    | BC Bare Conj
    | BD Bare Disj
    | BB Bare Bare
