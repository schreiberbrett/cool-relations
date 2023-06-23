# miniKanren expression functions

Functions that operate on the quoted form of miniKanren expressions, where a miniKanren expression is defined as:


```haskell
data MkExp
    = Fresh [Symbol] [MkExp]
    | Conde [[MkExp]]
    | Relation Symbol [SExp]
```