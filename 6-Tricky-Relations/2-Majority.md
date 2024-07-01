# The Majority Relation

Consider a relation over a value `x` and a list `l` where `x` is the majority (more than half) of all values in `l`. This has a natural correspondence to the majority gate in circuit complexity.

A list `l` has a majority element `x` when more than half of its elements unify to `x`.

```scheme
(defrel (majorityo/diseq x l)
  (fresh (difference)
    (poso/int difference)
    (count-differenceo/diseq x l difference)))
```

THe relation `(count-differenceo/diseq x l c)` asserts that the integer `c` is the number of elements unifiable with `x` minus the number of elements in `l` not unifiable with `x`.

```scheme
(defrel (count-differenceo/diseq x l c)
  (conde ((== l '()) (== c '(nat ())))
         ((fresh (a d rec)
            (== l `(,a . ,d))
            (conde ((==  a x) (+1o/int rec c))
                   ((=/= a x) (+1o/int c rec)))
            (count-differenceo/diseq x d rec)))))
```

The results of running `majorityo/diseq` on a fresh list is a combinatorial explosion.

```
> (run 10 (q) (majorityo/diseq 'x q))
'((x)
  (x x)
  ((_.0 x x) (=/= ((_.0 x))))
  (x x x)
  ((x _.0 x) (=/= ((_.0 x))))
  ((x x _.0) (=/= ((_.0 x))))
  ((_.0 x x x) (=/= ((_.0 x))))
  (x x x x)
  ((x x _.0 x) (=/= ((_.0 x))))
  ((x x x _.0) (=/= ((_.0 x)))))
```

One way to mitigate this is to recognize majorities only up to the deciding vote, and leave all other list elements unconstrained and fresh, because they may be additions to the majority.

```scheme
(defrel (majorityo/monotonic x l)
  (fresh (difference p)
    (conde ((== difference '(nat (s))))
           ((== difference '(nat (s s)))))
    (count-differenceo/trs2e x l difference)))
```


This requires `count-differenceo/trs2e`. It has this name because it and its inner relations only use `==`, never `=/=`. Disequality is not supported in TRS2E miniKanren.

```scheme
(defrel (count-differenceo/trs2e x l c)
  (conde ((== l '()) (== c '(nat ())))
         ((fresh (a d rec)
            (== l `(,a . ,d))
            (conde ((==  a x) (+1o/int rec c))
                   ((+1o/int c rec)))
            (count-differenceo/trs2e x d rec)))))
```

And its results:

```
> (run 15 (q) (majorityo/monotonic 'x q))
'((x)
  (x x)
  (_.0 x x)
  (x _.0 x)
  (x x _.0)
  (_.0 x x x)
  (x _.0 x x)
  (x x _.0 x)
  (x x x _.0)
  (_.0 _.1 x x x)
  (_.0 x _.1 x x)
  (x _.0 _.1 x x)
  (_.0 x x _.1 x)
  (x _.0 x _.1 x)
  (x x _.0 _.1 x))
```

Here is where they differ:

```
> (run* (q) (majorityo/diseq     'x '(x x x x x)))
'(_.0)
> (run* (q) (majorityo/monotonic 'x '(x x x x x)))
'(_.0 _.0 _.0 _.0 _.0 _.0 _.0 _.0 _.0 _.0)
```

Tabling could solve this. Another way to solve this is by using `project` to probe for an existing majority and succeeding once.

```scheme
(defrel (majorityo/project x l)
  (listo l)
  (project (x l)
    (if (> (length (filter (lambda (e)      (equal? e x))  l))
           (length (filter (lambda (e) (not (equal? e x))) l)))
        (== #t #t)
        (majorityo/monotonic x l))))
```

This gives the best of both worlds. But `majorityo/project` might fail on some other query.

```
> (run* (q) (majorityo/project 'x '(x x x x x)))
'(_.0)
"code.rkt"> (run 15 (l) (majorityo/project 'x l))
'((x)
  (x x)
  (_.0 x x)
  (x _.0 x)
  (x x _.0)
  (_.0 x x x)
  (x _.0 x x)
  (x x _.0 x)
  (x x x _.0)
  (_.0 _.1 x x x)
  (_.0 x _.1 x x)
  (x _.0 _.1 x x)
  (_.0 x x _.1 x)
  (x _.0 x _.1 x)
  (x x _.0 _.1 x))
```


TODO: Write a version of `count-differenceo/trs2e` with `conda` or `condu` and see how it fares. Add `conda` and `condu` to `faster-minikanren`.