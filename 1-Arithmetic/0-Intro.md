See [Pure, Declarative, and Constructive Arithmetic Relations by Kiselyov, et al.](https://okmij.org/ftp/Prolog/Arithm/arithm.pdf) for further details.

# Peano numbers

```scheme
(defrel (groundo/peano n)
  (conde ((== n '()))
         ((fresh (n-1)
            (== n `(s . ,n-1))
            (groundo/peano n-1)))))
```

```scheme
(defrel (poso/peano n)
  (fresh (n-1)
    (== n `(s . ,n-1))))
```

```scheme
(defrel (+1o/peano n n+1)
  (== n+1 `(s . ,n)))
```

## Linear Peano relationships

To prove that a heap of pebbles is twice as large as another, match each pebble in the first heap with two in the second.

This technique is used to assert that one Peano nat is double another in miniKanren.

```scheme
(defrel (*2o/peano n 2n)
  (conde ((== n '()) (== 2n '()))
         ((fresh (n-1 2n-2)
            (== n `(s . ,n-1))
            (== 2n `(s s . ,2n-2))
            (*2o/peano n-1 2n-2)))))
```

# Oleg numbers

Oleg numbers are well-covered in the literature. Here are some helpers.

## Successorship

```scheme
(defrel (+1o n n+1)
  (conde ((== n '()) (== n+1 '(1)))
         ((fresh (a d)
            (== n `(,a . ,d))
            (conde ((== a 0) (poso d) (== n+1 `(1 . ,d)))
                   ((fresh (d+1)
                      (== a 1)
                      (== n+1 `(0 . ,d+1))
                      (+1o d d+1))))))))
```


# Integers

Integers can be defined like they are in Lean: https://stackoverflow.com/a/68606389.

```scheme
(defrel (groundo/int x)
  (fresh (n)
    (conde ((== x `(nat ,n)))
           ((== n `(neg-succ ,n))))
    (groundo/peano n)))
```

```scheme
(defrel (poso/int x)
  (fresh (n)
    (== x `(nat ,n))
    (poso/peano n)))
```

## Int successor relation

```scheme
(defrel (+1o/int x x+1)
  (fresh (n n+1)
    (conde ((== x `(nat ,n)) (== x+1 `(nat ,n+1)) (+1o/peano n n+1))
           ((== x '(neg-succ ())) (== x+1 '(nat ())))
           ((== x `(neg-succ (s . ,n))) (== x+1 `(neg-succ ,n))))))
```

