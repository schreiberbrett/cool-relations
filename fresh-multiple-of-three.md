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


Another way that a number is a multiple of three:
* it is zero, or
* it is three more than a multiple of three.

```scheme
(defrel (another-multiple-of-threeo +3o n)
  (conde ((== n '()))
         ((fresh (n-3)
            (+3o n-3 n)
            (another-multiple-of-threeo +3o n-3)))))
            
(defrel (+3o-naive1 n n+3)
  (pluso '(1 1) n n+3))
  
(defrel (+3o-naive2 n n+3)
  (pluso n '(1 1) n+3))
```
Very slow, maybe specializing `(pluso '(1 1) n-3 n)` can help.

```scheme
(defrel (+3o-special n n+3)
  (fresh (n+1 n+2)
    (succo n n+1)
    (succo n+1 n+2)
    (succo n+2 n+3)))
```


```scheme
(defrel (succo n n+1)
  (conde ((== n '()) (== n+1 '(1)))
         ((fresh (a d d+1)
            (== n `(,a . ,d))
            (conde ((== a 0) (pairo d) (== n+1 `(1 . ,d)))
                   ((== a 1) (== n+1 `(0 . ,d+1)) (succo d d+1)))))))
                   
                   
(defrel (add-bito b n n+b)
  (conde ((== n '()) (== n+b `(,b)))
         ((fresh (a d a+b d+b carry)
            (== n `(,a . ,d))
            (== n+b `(,a+b . ,d+b))
            
            (conde ((== a b) (== a+b 0))
                   ((=/= a b) (== a+b 1)))
            (conde ((=/= `(,a ,b) '(1 1)) (== d+b d))
                   ((== `(,a ,b) '(1 1)) (== carry 1) (add-bito carry d d+b)))))))
```

```scheme
(defrel (==* x l)
  (conde ((== l '()))
         ((fresh (d)
            (== l `(,x . ,d))
            (==* x d)))))
```


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

## algo2
```scheme
(defrel (algo2 e o n)
  (conde ((== n '()))
         ((fresh (x) (== n `((0 ,x)))))
         ((fresh (x rec a0 a1 a2 a3 a4 a5)
            (conde ((== `(,a0 ,a1 ,a2 ,a3 ,a4 ,a5) `((s)     (,x even) (s)     (,x odd) n rec)))
                   ((== `(,a0 ,a1 ,a2 ,a3 ,a4 ,a5) `((s s s) (,e even) (s s s) (,o odd) n rec))))
            (removeNMo a0 a1 a2 a3 a4 a5)
            (algo2 e o rec)))))
```


## removeNMo
`o` is `l` with its first `n` copies of `x` and first `m` copies of `y` removed.
```scheme
(defrel (removeNMo n x m y l o)
  (conde ((== n '()) (== m '()) (== l o))
         ((fresh (a d rec n-1 m-1)
            (== l `(,a . ,d))
            (conde ((== n `(s . ,n-1))) ((== m `(s . ,m-1))))
            (conde ((fresh (n-1) (== a x) (== o rec) (== n `(s . ,n-1)) (removeNMo n-1 x m y d rec)))
                   ((fresh (m-1) (== a y) (== o rec) (== m `(s . ,m-1)) (removeNMo n x m-1 y d rec)))
                   ((=/= a x) (=/= a y) (== o `(,a . ,rec)) (removeNMo n x m y d rec)))))))
```
