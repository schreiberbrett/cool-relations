# Inequalities

Peano numbers are ordered.

```scheme
(defrel (<o/peano n m)
  (fresh (n-1 m-1)
    (== m `(s . ,m-1))
    (conde ((== n '()))
           ((== n `(s . ,n-1))
            (<o/peano n-1 m-1)))))
```

The less-than-or-equal relation over Oleg numbers is provided by `faster-miniKanren` as `<=o`.