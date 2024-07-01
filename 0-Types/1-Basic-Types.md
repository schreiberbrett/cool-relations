# Basic Types

## Lists

```scheme
(defrel (listo l)
  (conde ((== l '()))
         ((fresh (a d)
            (== l `(,a . ,d))
            (listo d)))))
```

```scheme
(defrel (length-parityo l p)
  (conde ((== l '()) (== p 'even))
         ((fresh (a d rec)
            (== l `(,a . ,d))
            (conde ((== p 'odd)  (== rec 'even))
                   ((== p 'even) (== rec 'odd)))
            (length-parityo d rec)))))
```

## Unary natural numbers:

```scheme
(defrel (peanoo n)
  (conde ((== n '())
         ((fresh (n-1)
            (== n `(s . ,n-1))
            (peanoo n-1))))))
```

## Little-endian binary numerals: "Oleg numbers"

The solution set is a subset of `listo`. It directly encodes `listo` with further binary constraints:
* `n` is a list of ones or zero's.
* If `n` is nonzero, then it is a nonempty list, and its last element must be a 1.

```scheme
(defrel (olego n)
  (conde ((== n '()))
         ((fresh (a d)
            (== n `(,a . ,d))
            (conde ((== a 0) (poso d))
                   ((== a 1)))
            (olego d)))))
```

`<=o` for Oleg numbers is provided by the core arithmetic miniKanren library.