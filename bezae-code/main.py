
from typing import List
from dataclasses import dataclass

Symbol = str

@dataclass
class Sym:
    sym: Symbol

@dataclass
class Cons:
    car: 'SExp'
    cdr: 'SExp'

SExp = Sym | Cons

def symbol_occurs_in_sexp(x: Symbol, sexp: SExp) -> bool:
    match sexp:
        case Sym(sym):
            return x == sym

        case Cons(car, cdr):
            return (
                symbol_occurs_in_sexp(x, car) or
                symbol_occurs_in_sexp(x, cdr)
            )

def replace_symbol_in_sexp(replace_me: Symbol, replace_with: SExp, sexp: SExp) -> SExp:
    match sexp:
        case Sym(sym):
            if sym == replace_me:
                return replace_with
            else:
                return Sym(sym)

        case Cons(car, cdr):
            return Cons(
                replace_symbol_in_sexp(replace_me, replace_with, car),
                replace_symbol_in_sexp(replace_me, replace_with, cdr)
            )

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

def replace_symbol_in_clause(replace_me: Symbol, replace_with: SExp, clause: Clause) -> Clause:
    match clause:
        case Relation(name, args):
            return Relation(name, [replace_symbol_in_sexp(replace_me, replace_with, arg) for arg in args])

        case Conde(conjunctions):
            return Conde([[replace_symbol_in_clause(replace_me, replace_with, clause)
                for clause in conjunction]
                for conjunction in conjunctions
            ])

        case Fresh(vars, clauses):
            if replace_me in vars:
                return Fresh(vars, clauses)
            else:
                return Fresh(vars, [
                    replace_symbol_in_clause(replace_me, replace_with, clause)
                    for clause in clauses
                ])

@dataclass
class Defrel:
    name: Symbol
    args: List[Symbol]
    clauses: List[Clause]

appendo = Defrel('appendo', ['l', 'r', 'o'], [
    Conde([
        [Relation('==', [Sym('l'), Sym('nil')]), Relation('==', [Sym('r'), Sym('o')])],
        [Fresh(['h', 't', 'rec'], [
            Relation('==', [Sym('l'), Cons(Sym('h'), Sym('t'))]),
            Relation('==', [Sym('o'), Cons(Sym('h'), Sym('rec'))]),
            Relation('appendo', [Sym('t'), Sym('r'), Sym('rec')])])]])])
