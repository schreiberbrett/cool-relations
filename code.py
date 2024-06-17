
from typing import List, Set, Tuple, Dict, TypeVar
from dataclasses import dataclass
                  
A = TypeVar('A')
B = TypeVar('B')
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
    iterA: Iterator[A],
    iterB: Iterator[B]
) -> Iterator[Tuple[A, B]]:
    hasA = True
    hasB = True

    seenA: List[A] = []
    seenB: List[B] = []

    while hasA or hasB:
        if hasA:
            try:
                a = next(iterA)
                yield from (a, b for b in seenB)
                seenA.append(a)
            except StopIteration:
                hasA = False

        if hasB:
            try:
                b = next(iterB)
                yield from (a, b for a in seenA)
                seenB.append(b)
            except StopIteration:
                hasB = False

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
