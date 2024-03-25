# Utility Functions

The classics

```scheme
(defrel (pairo x)
  (fresh (a d) (== x `(,a . ,d))))

(defrel (appendo l r l++r)
  (conde ((== l '()) (== r l++r))
         ((fresh (a d d++r)
                 (== l `(,a . ,d))
                 (== l++r `(,a . ,d++r))
                 (appendo d r d++r)))))

(defrel (>0o x)
  (fresh (a d) (== x `(,a . ,d))))

(defrel (>1o x)
  (fresh (a ad dd) (== x `(,a ,ad . ,dd))))
```


## Peano Natural Numbers

`list-refᵒ`: List `l` has value `val` at index `n`. Inspired by Racket's [`list-ref`](https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list-ref%29%29).

```scheme
(defrel (list-refᵒ l n val)
  (fresh (a d n-1)
         (== l `(,a . ,d))

         (conde ((== n '()) (== a val))

                ((== n `(s . ,n-1))
                 (list-refᵒ d n-1 val)))))
```
