# Detecting unsatisfiable formulas in miniKanren

My favorite complexity class is [coNP](https://complexityzoo.net/Complexity_Zoo:C#conp), which is the set of all languages that have a polynomial-sized proof of non-membership. The $NP vs coNP$ question is important in the study of proof complexity.

One [coNP-complete](https://en.wikipedia.org/wiki/Co-NP-complete) language is CNF-UNSATISFIABILITY, which is defined as all logical formulas in conjunctive normal form which are unsatisfiable. There is a nondeterministic algorithm for this language that uses resolution. Here is my description of the algorithm:

* If there is an empty clause in the CNF, the formula is unsatisfiable.
* Otherwise, nondeterministically pick 2 of the clauses who have a common negated variable, and compute their resolution. Recur on the CNF + new resolved clause.

And here is that same algorithm, written in miniKanren.

```
(defrel (unsato cnf)
  (conde ((any-emptyo cnf))
         ((fresh (c1 c2 cr cnf+cr)
            (pick2 cnf c1 c2)
            (resolveo c1 c2 cr)
            (add-clauseo cnf cr cnf+cr)
            (unsato cnf+cr)))))
```

