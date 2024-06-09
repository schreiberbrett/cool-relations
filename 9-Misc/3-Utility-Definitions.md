# Utility Functions

The classics

```scheme
(defrel (pairo x)
  (fresh (a d)
    (== x `(,a . ,d))))

(defrel (appendo l r l++r)
  (conde ((== l '()) (== r l++r))
         ((fresh (a d d++r)
            (== l `(,a . ,d))
            (== l++r `(,a . ,d++r))
            (appendo d r d++r)))))

(defrel (>1o x)
  (fresh (a ad dd)
    (== x `(,a ,ad . ,dd))))

(defrel (membero x l)
  (fresh (a d)
    (== l `(,a . ,d))
    (conde ((== x a))
           ((membero x d)))))
```


## Peano Natural Numbers

`list-refo`: List `l` has value `val` at index `n`. Inspired by Racket's [`list-ref`](https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list-ref%29%29).

```scheme
(defrel (list-refo l n val)
  (fresh (a d)
    (== l `(,a . ,d))
    (conde ((== n '()) (== a val))
           ((fresh (n-1)
              (== n `(s . ,n-1))
              (list-refo d n-1 val))))))
```

# `build-num` analog for building nats

```scheme
(define (build-nat n)
  (if (zero? n) '() `(s . ,(build-nat (- n 1)))))

(defrel (nat-lengtho n l)
  (conde ((== n '()) (== l '()))
         ((fresh (n-1 a d)
            (== n `(s . ,n-1))
            (== l `(,a . ,d))
            (nat-lengtho n-1 d)))))
```

