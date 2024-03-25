# Reversible relations in miniKanren

Apparently, this gate is reversible.
```scheme
(defrel (+-o n₁ n₂ n₁+n₂ n₁−n₂)
  (fresh (n₁−1 n₂−1 n₁−1+n₂−1)
    (conde ((== n₂ '())
            (== n₁+n₂ n₁)
            (== n₁−n₂ n₁))
            
           ((== n₂ `(s . ,n₂−1))
            (== n₁ `(s . ,n₁−1))
            (== n₁+n₂ `(s s . ,n₁−1+n₂−1))
            (+-o n₁−1 n₂−1 n₁−1+n₂−1 n₁−n₂)))))
```

Asking for the first five results on this relation with only free variables:
```
> (run 5 (a b c d) (+-o a b c d))
'((_.0 () _.0 _.0)
  ((s . _.0) (s) (s s . _.0) _.0)
  ((s s . _.0) (s s) (s s s s . _.0) _.0)
  ((s s s . _.0) (s s s) (s s s s s s . _.0) _.0)
  ((s s s s . _.0) (s s s s) (s s s s s s s s . _.0) _.0))
```
This is like asking for the first five lines in the infinite table of nonoverlapping truths:
a = x + 0, b = 0, a + b = x + 0, a - b = x
a = x + 1, b = 1, a + b = x + 2, a - b = x
a = x + 2, b = 2, a + b = x + 4, a - b = x
a = x + 3, b = 3, a + b = x + 6, a - b = x
a = x + 4, b = 4, a + b = x + 8, a - b = x


A back-of-the-napkin version of the relation:

```scheme
(defrel (plus-minuso x y x+y x-y)
  (appendo x y x+y)
  (appendo x-y y x))
```


