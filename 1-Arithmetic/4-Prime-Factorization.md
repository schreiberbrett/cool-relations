# Prime Factorization in miniKanren

I'm interested in the fundamental theorem of arithmetic, which asserts that there's a one-to-one relationship between:
- `n`, a positive natural number in Oleg form, and:
- `l`, a multiset of its prime factors.

Below is my attempt to define this relationship in miniKanren. I was able to get it working in some modes with `(run 1 ...)`. I had trouble with `(run 2 ...)`.

First, the naive version:
```scheme
(defrel (naive-prime-factorso n l)
  (ordered-listo <=o l)
  (all-primeo l)
  (producto l n))
```
Results:
```
> (run 1 (q) (naive-prime-factorso q (map build-num '(2 3 5))))
'((0 1 1 1 1))
> (run 1 (q) (naive-prime-factorso (build-num 13) q))
...
```
The first query succeeds with 30. The second query may halt at some point, but I am impatient.

The calls to `ordered-listo`, `all-primeo`, and `producto` must each recur through `l`. There's a way to rewrite the relation that only recurs through `l` once. It's a boring mechanical step. I wish a miniKanren compiler could do it.
```scheme
(defrel (prime-factorso n l)
  (conde ((== l '()) (== n '(1)))
         ((fresh (a d a/n)
            (== l `(,a . ,d))
            (*o a a/n n)
            (primeo a)
            (conde ((== d '()) (== n a))
                   ((fresh (ad dd)
                      (== d `(,ad . ,dd))
                      (<=o a ad)
                      (prime-factorso a/n d))))))))
```

Some quick and dirty conjunction analysis:
- `(*o a a/n n)` should come before `(primeo a)`, because`(*o a a/n n)` bounds `a` when `n` is ground, whereas `(primeo a)`, given a fresh `a`, yields infinitely many results. See [Euclid's theorem](https://en.wikipedia.org/wiki/Euclid%27s_theorem).
- `(<=o a ad)` should probably come before `(prime-factorso a/n d)`. This one I'm less sure about, I just always do it.

Every one-to-one relation should strive to be as reliable as `==` and `=/=`.

Results:
```
> (run 1 (q) (prime-factorso q (map build-num '(2 3 5))))
'((0 1 1 1 1))
```
Runs backwards just fine, same as the naive version. But this version can run forwards:
```
> (run 1 (q) (prime-factorso (build-num 156) q))
'(((0 1) (0 1) (1 1) (1 0 1 1)))
```
The prime factors of 156 are 2, 2, 3, and 13. Are there any other ways to prime factorize 156?
```
> (run* (q) (prime-factorso (build-num 156) 
q))
'(((0 1) (0 1) (1 1) (1 0 1 1)))
```

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
cpu time: 184 real time: 185 gc time: 27
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

Throwaway miniKanren definitions:
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
