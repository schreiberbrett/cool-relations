# A fresh multiple of three

Here's my attempt at a multiple of three relation that keeps its bits fresh. It works by nondeterministically "crossing off" even- and odd-indexed bits that are equal (to maintain that the sum mod 3 equals zero), and by crossing off groups of three even- and odd- indexed bits. It only grounds the result somewhat -- the final 1 digit and one zero bit somewhere in `n` if `n` has an odd bitlength.

```scheme
(defrel (fresh-multiple-of-threeo n)
  (fresh (labeled e o prefix)
    (=/= e o)
    (ends-in-1o n)
    (label-odds-and-evenso n labeled)
    (algo e o labeled)))
```

```
> (run 15 (q) (fresh-multiple-of-threeo q))
'(()
  (1 1)
  (0 1 1)
  (_.0 _.0 1 1)
  (1 _.0 _.0 1)
  (1 _.0 _.0 1)
  (_.0 _.0 1 1)
  (_.0 _.0 0 1 1)
  (_.0 1 0 _.0 1)
  (0 _.0 _.0 1 1)
  (_.0 _.0 _.1 _.1 1 1)
  (0 1 _.0 _.0 1)
  (_.0 1 0 _.0 1)
  (_.0 _.0 0 1 1)
  (_.0 _.0 1 _.1 _.1 1))
```

Lots of overlap right now, because I have a conjunction of two riffles, which introduces permuted duplicates. These duplicates can be removed with specialized "cross-off" relations.

```scheme
(defrel (ends-in-1o n)
  (conde ((== n '()))
         ((fresh (prefix)
            (appendo prefix '(1) n)))))
```

```scheme
(defrel (label-odds-and-evenso l labeled)
  (conde ((== l '()) (== labeled '()))
         ((fresh (a) (== l `(,a)) (== labeled `((,a even)))))
         ((fresh (a1 a2 d rec)
            (== l `(,a1 ,a2 . ,d))
            (== labeled `((,a1 even) (,a2 odd) . ,rec))
            (label-odds-and-evenso d rec)))))
```

```scheme
(defrel (algo e o n)
  (conde ((== n '()))
         ((fresh (x) (== n `((0 ,x)))))
         ((fresh (rec)
            (conde ((fresh (x n-x n-xx)
                      (riffleo `((,x even)) n-x n)
                      (riffleo `((,x odd)) rec n-x)))
                   ((fresh (n-eee)
                      (riffleo `((,e even) (,e even) (,e even)) n-eee n)
                      (riffleo `((,o odd) (,o odd) (,o odd)) rec n-eee))))
            (algo e o rec)))))
```


```scheme
(define (h n)
  (length (run* (q)
    (lengtho q (build-num n))
    (fresh-multiple-of-threeo q))))
```

```
> (map h '(0 1 2 3 4 5 6 7 8 9 10))
'(1 0 1 1 4 8 37 111 608 2432 15600)
```

```scheme
(defrel (brett n rec)
  (fresh (n-eee)
    (riffleo `(a a) n-eee n)
    (riffleo `(o o) rec n-eee)))
```
