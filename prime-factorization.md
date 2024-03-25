# Prime Factorization in miniKanren

There is a one-to-one relation between an Oleg number and the multiset of its prime factors, which can be represented as a sorted list. The definition of `sorted-listo` can be found in [data-structures.md](data-structures.md).


```scheme
(defrel (prime-factorso1 l n)
  (sorted-listo l)
  (all-primeo l)
  (producto l n))
```

If this were an imperative language, I could check all three properties (check list is sorted, check every element is prime, and accumulate their product) in a single for-loop. A similar optimization can be done in miniKanren. (Compare [equational reasoning](https://www.youtube.com/watch?v=MjpZJA1jIqU) and [Hutton 1999](https://www.cs.nott.ac.uk/~pszgmh/fold.pdf), although I don't think `sorted-list?`, the functional version of `sorted-listo`, can be expressed as a `fold`.)

It's a really boring step, and I wish a miniKanren compiler could do it.

```scheme
(defrel (prime-factorso2 l n)
  (conde ((== l '()) (== n '(1)))
         ((fresh (a d a/n)
            (== l `(,a . ,d))
            (primeo a)
            (*o a a/n n)
            (conde ((== d '()) (== n a))
                   ((fresh (ad dd)
                      (== d `(,ad . ,dd))
                      (<=o a ad)
                      (prime-factorso2 d a/n))))))))
```

A cheat relation that uses `project`.
```scheme
(defrel (primeo n)
  (olego n)
  (project (n)
    (== #t (prime? (unbuild-num n)))))
```

A set data structure can be useful in writing the relationship between a number and its prime factors. (But a multiset would be better).

There are 2 non-overlapping conditions:
1. `n` is prime. Then `n` is its only factor. (Only is a bit too negative for me, so I'll just say `n` is a factor.)
2. `n` is composite, that is`n = a * b` for some a, b greater than 1. Then `n`'s factors are exactly the union of the factors of `a`and `b`. In other words, `a`'s factors are my factors, and `b`'s factors are my factors, too.

Notice that neither condition succeeds when `n` is 0 and when `n` is 1.

`>1o` and `*o` come from TRS2E. `primeo` is a cheat relation that takes advantage of the TRS2E miniKanren internals. It succeeds on fully ground, prime, Oleg numbers, enumerates all primes when a fresh variable, and fails on all other inputs.

```scheme
(define (unbuild-num n)
  (cond ((null? n) 0)
        (else (+ (car n) (* 2 (unbuild-num (cdr n)))))))

(define (prime? n)
  (let loop ((i 2))
    (cond ((<= n 1) #f)
          ((= i n) #t)
          ((= (modulo n i) 0) #f)
          (else (loop (+ i 1))))))
```

Results:

```
> (run 1 (q) (primeo (build-num 37)))
'((_0))
> (run* (q) (primeo (build-num 37)))
'((_0))
> (run* (q) (primeo (build-num 38)))
'()
> (run* (q) (primeo (build-num 39)))
'()
> (run 5 (q) (primeo q))
'(((0 1)) ((1 1)) ((1 0 1)) ((1 1 1)) ((1 1 0 1)))
```

## Miscellaneous definitions

```scheme
(defrel (producto l n)
  (conde ((== l '()) (== n '(1)))
         ((fresh (a d n/a)
            (== l `(,a . ,d))
            (*o a n/a n)
            (producto d n/a)))))
            
(defrel (all-primeo l)
  (conde ((== l '()))
         ((fresh (a d)
            (== l `(,a . ,d))
            (primeo a)
            (all-primeo d)))))
```
