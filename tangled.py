
from typing import List, Set, Tuple, Dict, TypeVar, Iterator, Generic, Callable
from dataclasses import dataclass
                  
X = TypeVar('X')
Y = TypeVar('Y')
T = TypeVar('T')
# .\README.md
# .\0-Types\0-Intro.md
# .\0-Types\1-Basic-Types.md
# .\0-Types\2-Higher-Order-Types.md
# .\0-Types\3-Natmaps.md
# .\0-Types\4-Natsets.md
# .\1-Arithmetic\0-Intro.md
# .\1-Arithmetic\1-Inequalities.md
# .\1-Arithmetic\2-Divisibility-by-Three.md
# .\1-Arithmetic\3-Fresh-Multiples-of-Three.md
# .\1-Arithmetic\4-Prime-Factorization.md
# .\1-Arithmetic\5-GCD.md
# .\1-Arithmetic\6-Triangle-Numbers.md
# .\1-Arithmetic\7-Modular-Arithmetic.md
# .\2-Combinatorics\1-Riffle.md
# .\2-Combinatorics\2-Cartesian-Product.md
# .\2-Combinatorics\3-Associative-Cartesian-Product.md
def concrete_product(l: List[List[T]]) -> List[List[T]]:
    l1, l2, l3, l4 = l
    result = []
    for e1 in l1:
        for e2 in l2:
            for e3 in l3:
                for e4 in l4:
                    result.append([e1, e2, e3, e4])

    return result

# .\2-Combinatorics\4-Multiset-Venn-Diagrams.md
# .\3-Theory-of-Computation\0-Intro.md
# .\3-Theory-of-Computation\1-Lexing.md
# .\3-Theory-of-Computation\2-Context-Free-Grammars.md
# .\3-Theory-of-Computation\3-3SAT.md
# .\3-Theory-of-Computation\4-3COLOR.md
# .\3-Theory-of-Computation\5-UNSAT.md
# .\5-Puzzles-and-Games\1-The-Zebra-Puzzle.md
# .\6-Tricky-Relations\1-Length.md
# .\6-Tricky-Relations\2-Majority.md
# .\6-Tricky-Relations\3-Equal-Popcount.md
# .\6-Tricky-Relations\4-HAMPATH.md
# .\7-Techniques\1-Fresh-Tagging.md
# .\7-Techniques\2-Polymorphism-in-miniKanren.md
# .\7-Techniques\3-One-to-One-Relationships.md
# .\8-Implementing-miniKanren\1-Dovetailing-Streams.md
def dovetail(
    iterX: Iterator[X],
    iterY: Iterator[Y]
) -> Iterator[Tuple[X, Y]]:
    hasX = True
    hasY = True

    seenX: List[X] = []
    seenY: List[Y] = []

    while hasX or hasY:
        if hasX:
            try:
                x = next(iterX)
                yield from ((x, y) for y in seenY)
                seenX.append(x)
            except StopIteration:
                hasX = False

        if hasY:
            try:
                y = next(iterY)
                yield from ((x, y) for x in seenX)
                seenY.append(y)
            except StopIteration:
                hasY = False

def count_up():
    i = 0
    while True:
        yield i
        i += 1

# .\8-Implementing-miniKanren\2-Defunctionalization.md
@dataclass
class Empty:
    pass

@dataclass
class Nonempty(Generic[T]):
    first: T
    rest: 'Stream[T]'

@dataclass
class Suspended(Generic[T]):
    suspension: Callable[[], 'Stream[T]']

Stream = Empty | Nonempty[T] | Suspended[T]

def append_inf(s: Stream[T], t: Stream[T]) -> Stream[T]:
    match s:
        case Empty():
            return t
        case Nonempty(first, rest):
            return Nonempty(first, append_inf(rest, t))
        case Suspended(sus):
            return Suspended(lambda: append_inf(t, sus()))

# .\8-Implementing-miniKanren\3-miniKanren-in-C.md
# .\9-Misc\1-A-Rule-of-Inference.md
# .\9-Misc\2-Rational-Numbers.md
# .\9-Misc\3-Utility-Definitions.md
# .\first-order-miniKanren\defrel-and-run\README.md
# .\first-order-miniKanren\existential-disjunctive-normal-form\README.md
# .\first-order-miniKanren\misc\README.md
# .\first-order-miniKanren\mk-expression-functions\README.md
# .\first-order-miniKanren\s-expression-functions\README.md
# .\liarKanren\README.md
# .\misc\expression-problem.md
# .\misc\natset.md
# .\misc\projection-and-selection.md
# .\misc\statically-typed-relations.md
# .\scribble-htmls\katex\README.md
