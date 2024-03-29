# Prime Factorization in miniKanren

I love the fundamental theorem of arithmetic, which asserts that there's a one-to-one relationship between:
- `n`, a positive natural number in Oleg form, and:
- `l`, a multiset of its prime factors.
```scheme
(defrel (naive-prime-factorso n l)
  (sorted-listo l)
  (all-primeo l)
  (producto l n))
```
Results:
```
> (run 1 (q) (naive-prime-factorso q (list (build-num 2) (build-num 3) (build-num 5))))
'((0 1 1 1 1))
> (run 1 (q) (naive-prime-factorso (build-num 13) q))
...
```
The first query succeeds with 30. The second query may halt at some point, but I am impatient.

If this were an imperative language, I could check all three properties (check list is sorted, check every element is prime, and accumulate their product) in a single for-loop. A similar optimization can be done in miniKanren. (Compare [equational reasoning](https://www.youtube.com/watch?v=MjpZJA1jIqU) and [Hutton 1999](https://www.cs.nott.ac.uk/~pszgmh/fold.pdf), although I don't think `sorted-list?`, the functional version of `sorted-listo`, can be expressed as a `fold`.)

The calls to `sorted-listo`, `all-primeo`, and `producto` must each recur through `l`. There's a way to rewrite the relation that only recurs through `l` once. It's a boring mechanical step. I wish a miniKanren compiler could do it.
```scheme
(defrel (prime-factorso n l)
  (conde ((== l '()) (== n '(1)))
         ((fresh (a d a/n)
            (== l `(,a . ,d))
            (primeo a)
            (*o a a/n n)
            (conde ((== d '()) (== n a))
                   ((fresh (ad dd)
                      (== d `(,ad . ,dd))
                      (<=o a ad)
                      (prime-factorso a/n d))))))))
```
Results:
```
> (run 1 (q) (prime-factorso q (list (build-num 2) (build-num 3) (buil
d-num 5))))
'((0 1 1 1 1))
```
Runs backwards just fine, same as the naive version. But it can run forwards, too:
```
> (run 1 (q) (prime-factorso (build-num 156) q))
'(((0 1) (0 1) (1 1) (1 0 1 1)))
```
The prime factors of 156 are 2, 2, 3, and 13. Are there any other ways to prime factorize 156?
```
> (run 2 (q) (prime-factorso (build-num 156) q))
...
```
This query diverges. There may be a way to use `(once ...)` in the relation definition to enfore that this is a one-to-one relationship.

`primeo` is a cheat relation.

```scheme
(defrel (primeo n)
  (olego n)
  (project (n)
    (== #t (prime? (unbuild-num n)))))
```

Finally, just for fun:

```
> (time (length (run 15 (a b) (naive-prime-factorso a b))))
cpu time: 21345 real time: 21387 gc time: 5714
15
> (time (length (run 15 (a b) (prime-factorso a b))))
cpu time: 465 real time: 466 gc time: 91
15
```

## Miscellaneous definitions

Scheme-specific:
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


Here are two miscellaneous miniKanren definitions.
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
