# Higher-Order Types

These types require passing a relation as one of its arguments. At present, this implies the argument must always be ground, which breaks relationality. So use these relations with care, just like how you should use the [basic type relations](./0-Basic-Types.md) with care, since they ground their arguments.

## Sets and multisets as ordered lists

My current favorite way to model sets in miniKanren is as sorted list with no duplicates. TODO: Argue why.

A sorted set without duplicates is one that succeeds the `ordo` relation, pairwise, and where `ordo` fails on unifiable (i.e., equal) arguments.

If you pass in an `ordo` that acts like "less than or equal", you get a multiset!

```scheme
(defrel (ordered-listo ordo l)
  (conde ((== l '()))
         ((fresh (a d)
            (== l `(,a . ,d))
            (conde ((== d '()))
                   ((fresh (ad dd)
                      (== d `(,ad . ,dd))
                      (ordo a ad)
                      (ordered-listo  ordo d))))))))
```