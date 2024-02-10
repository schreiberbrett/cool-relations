# Factoring miniKanren expressions

```minikanren
(require "../faster-minikanren/mk.rkt")
```

Suppose I had the following relation, which asserts that every element in `l` is `'banana`.

```minikanren
(defrel (bananaso l)
  (conde ((== l '()))
         ((fresh (d)
            (== l `(banana . ,d))
            (bananaso d)))))
```

Given the following two expressions:

v1:
```minikanren
(define (v1 n)
  (run n (a b) (conde ((bananaso a))
                      ((bananaso b)))))
```

And v2:
```minikanren
(define (v2 n)
  (run n (a b)
    (fresh (x)
      (conde ((== x a))
             ((== x b)))
      (bananaso x))))
```
