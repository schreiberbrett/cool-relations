# `mk-compiler.scm`

Defines the following:
* The macro `defrel-optimized`. Equivalent to `defrel` from The Reasoned Schemer 2nd ed. (https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd/blob/master/trs2-impl.scm). Only supports `fresh`, `conde`, and `==`. It does not support `=/=` or constructs from the host language, e.g., `define`, `lambda`, and forms of `let`.
* The function `optimize-conde` which takes a quoted `conde` clause and performs this greedy divide-and-conquer algorithm:
1. Find the most *frequent* relation, i.e., one that appears in the most `conde` clauses. Only consider relations that appear in at least 2 clauses.
2. Partition the clauses into those that contain the relation, and those that do not. Extract the relation.
3. Recur on both partitions until there are no more shared relations.

It relies on the following nonstandard Scheme constructs:
* Pattern matching from pmatch.scm (Oleg Kiselyov, https://github.com/webyrd/quines/blob/master/pmatch.scm)
* One occurence of `(gensym)` from Chez Scheme (https://www.scheme.com/csug6/objects.html)

Further improvements:
* Algorithmic improvements.
* Rewrite instances of `letrec*` where `let` would do fine. Same for `equal?` to `eqv?`, `eq?`.
* Support for `=/=`.
* Rewrite `syntax-rules` to not use `eval`.
* Turn top-level definitions into local definitions, where possible, to clean up the namespace.
* Maybe even remove instances of `pmatch`.