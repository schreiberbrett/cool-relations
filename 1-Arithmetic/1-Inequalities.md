# Inequalities

Peano numbers are ordered.

```scheme
(defrel (peano-<o n m)
  (fresh (n-1 m-1)
    (== m `(s . ,m-1))
    (conde ((== n '()))
           ((== n `(s . ,n-1))
            (peano-<o n-1 m-1)))))
```

The less-than-or-equal relation over Oleg numbers is provided by `faster-miniKanren` as `<=o`.