
from typing import List, Tuple
from dataclasses import dataclass

Symbol = str

SExp = Symbol | Tuple['SExp', 'SExp']

def symbol_occurs_in_sexp(sym: Symbol, sexp: SExp) -> bool:
    match sexp:
        case (car, cdr):
            return (
                symbol_occurs_in_sexp(sym, car) or
                symbol_occurs_in_sexp(sym, cdr)
            )

        case s:
            return sym == s

def replace_in_sexp(sym: Symbol, replacement: SExp, body: SExp) -> SExp:
    

@dataclass
class Relation:
    name: Symbol
    args: List[SExp]

@dataclass
class Conde:
    conjunctions: List[List['Clause']]

@dataclass
class Fresh:
    vars: List[Symbol]
    clauses: List['Clause']

Clause = Relation | Conde | Fresh

@dataclass
class Defrel:
    name: Symbol
    args: List[Symbol]
    clauses: List[Clause]

appendo = Defrel('appendo', ['l', 'r', 'o'], [
    Conde([
        [Relation('==', ['l', 'nil']), Relation('==', ['r', 'o'])],
        [Fresh(['h', 't', 'rec'], [
            Relation('==', ['l', ('h', 't')]),
            Relation('==', ['o', ('h', 'rec')]),
            Relation('appendo', ['t', 'r', 'rec'])])]])])
