
type Symbol = string

type SExp = {
    kind: 'SYMBOL',
    sym: Symbol
} | {
    kind: 'CONS',
    car: SExp,
    cdr: SExp
}
