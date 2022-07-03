I like to watch movies. One thing I've started to notice is an editing style known as [cross-cutting](https://en.wikipedia.org/Cross-cutting). This is where two scenes are edited to be shown to the audience at the same time. My favorite example is from Christopher Nolan's _Inception_, which cuts back and forth between different scenes to convey simultaneous action in both levels of the dream.

Here is the best picture I found online of cross-editing.

![](https://www.filmeditingpro.com/wp-content/uploads/2018/09/Crosscutting_01b.jpg)

This made me think that there's a relationship between two source scenes and the final cross-cut output. But film scenes are continuous, they exist on a timeline. So I need to think of a new analogy that works better with miniKanren's discrete lists and symbols.

# A better analogy

Consider the way a dealer shuffles cards like in the below GIF. Two separate decks get combined into one larger deck.

![](https://www.geogebra.org/resource/wbf3as26/nNXngeMdvMRfGZiL/material-wbf3as26.png)

This discrete analogy work much better for modeling a relationship in miniKanren.

# THe riffle relation

Consider the relationship between a list `a`, a list `b` and the riffled output `o`. We wil write the miniKanren code as if it is checking to make sure the riffled output is correct. Let's take care of the easy case:

* If one of `a` or `b` is empty then the riffle output is is equal to the other list. (This actually covers the first case).

In code, this would be:

```scheme
(defrel (riffleo a b o)
    (conde
        ;; When one of `a` or `b` is empty
        ((== a '()) (== b o))
        ((== b '()) (== a o))))

        ;; TODO: When both `a` and `b` are nonempty
```

These cases are overlapping when both `a` and `b` are empty. It is good practice to "expand out" the overlapping cases. So the base cases become:
* If `a` and `b` are both empty, then the output is empty.
* If `a` is empty and `b` is nonempty, then the output is equal to `b`.
* If `a` is nonempty and `b` is empty, then the output is equal to `a`.

We need fresh variables now to capture the potential non-emptiness of `a` and `b`. I will use a `car-*` and `cdr-*` convention.

```scheme
(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b)
        (conde
            ;; When one of `a` or `b` is empty
            ((== a '()) (== b '()) (== o '()))
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b)))))

            ;; TODO: When both `a` and `b` are nonempty
```

The more difficult case is when both `a` and `b` are nonempty. Let's look at "verifying" a riffle shuffle, per the above GIF: either a left card falls into the output deck, or the right card does. So we have two nonempty cases:

* Either `car-a` gets shuffled in, (that is, `car-a` equals `car-o`). And so the rest of the output (`cdr-o`). is the result of riffling `cdr-a` with `b`.
* Or `car-b` gets shuffled in, in which case the rest of the output is the result of riffling `a` with the `cdr-b`.

We also have implicitly stated that `o` is nonempty since it is made up of `car-o` and `cdr-o`. Which makes sense since riffling two nonempty decks should not result in an empty output. So this is true in both cases.

In code:

```scheme
(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o)
        (conde
            ;; When one of `a` or `b` is empty
            ((== a '()) (== b '()) (== o '()))
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are nonempty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o))
                (conde
                    ((== car-o car-a) (riffleo cdr-a b cdr-o))
                    ((== car-o car-b) (riffleo a cdr-b cdr-o)))))))
```

This completes the riffle relation.

# Optimizing `riffleo`

Notice that our previous definition of `riffleo` leaves recursive calls within two different `conde` clauses. We can use a mechanical correctness-preserving transformation to introduce three placeholder arguments in an unconditional call to unify against conditionally. A miniKanren compiler (in the works!) would be able to do this step automatically. This transformation yields:

```scheme
(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o z0 z1 z2)
        (conde
            ;; When one of `a` or `b` is empty
            ((== a '()) (== b '()) (== o '()))
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are nonempty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o))
                (conde
                    ((== car-o car-a) (== z0 cdr-a) (== z1 b) (== z2 cdr-o))
                    ((== car-o car-b) (== z0 a) (== z1 cdr-b) (== z2 cdr-o)))
                    
                (riffleo z0 z1 z2)))))
```

We can extract out the common `(== z2 cdr-o)` from both clauses of the inner `conde`. Once again this is a simple correctness preserving transformation.

```scheme
(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o z0 z1 z2)
        (conde
            ;; When one of `a` or `b` is empty
            ((== a '()) (== b '()) (== o '()))
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are nonempty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o)) (== z2 cdr-o)
                (conde
                    ((== car-o car-a) (== z0 cdr-a) (== z1 b))
                    ((== car-o car-b) (== z0 a) (== z1 cdr-b)))
                    
                (riffleo z0 z1 z2)))))
```

We can eagerly unify `z2` and `cdr-o` in order to remove all uses of `z2` in the relation, and therefore can be removed:

```scheme
(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o z0 z1)
        (conde
            ;; When one of `a` or `b` is empty
            ((== a '()) (== b '()) (== o '()))
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are nonempty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o))
                (conde
                    ((== car-o car-a) (== z0 cdr-a) (== z1 b))
                    ((== car-o car-b) (== z0 a) (== z1 cdr-b)))
                    
                (riffleo z0 z1 cdr-o)))))
```

Since the only recursive call is in the final `conde` clause, this relation 

# A surprising application

Consider the NP-complete [three-partition problem](https://en.wikipedia.org/wiki/3-partition_problem), which is a relationship between a list of numbers of length `3n`, call it `l`, and a list of triples of length `n`, call it `partitions`, where each partition adds up to the same number, call it `sum`, and each element in each triple of the partition is contained exactly once in `l`.

Since this relationship involves addition, we need to import the `pluso` relation like so:

```scheme
(load "~/CodeFromTheReasonedSchemer2ndEd/trs2-arith.scm")
```

Let's write a miniKanren relation in the style of "nondeterministic programming". That is, we will take advantage of the fact that each relation can produce multiple answers, and we code along the "happy path", assuming that the answer returned is the one we were looking for.

Once again, consider the base case first since it is easier. 
- If there are no numbers in `l` to pick, then there are no partitions to make.

```scheme
(defrel (three-partitiono l partitions sum)
    (conde
        ;; Base case
        ((== l '()) (== partitions '()))))
        ;; TODO: Recursive case
```

But for the recursive case, how do you choose 3 elements from a list? Well, you can do it exactly with `riffleo`! Consider:

```
> (run* (e0 e1 e2) (fresh (x) (riffleo `(,e0 ,e1 ,e2) x '(a b c d))))
((b c d) (a c d) (a b c) (a b d))
```

So we can use `riffleo` _nondeterministically_ to pick three elements from `l`. Note also that this yields the elements of the list that are **not** chosen.
```scheme
(defrel (three-partitiono l partitions sum)
    (fresh (e0 e1 e2 rest-l)
        (conde
            ;; Base case
            ((== l '()) (== partitions '()))))
            ;; Recursive case
            ((riffleo `(,e0 ,e1 ,e2) rest-l l)))
```

Notice here that the base case and the recursive case are non-overlapping. The list is either empty, or it has at least three elements that can be chosen. But what do we do with those three elements? According to the definition of the three-partition problem, they must add up to `sum`. `pluso` only adds up two numbers, so we must introduce an intermediate addition with a fresh variable `e0+e1`, and a second `pluso` to get the final sum. So we have:

```scheme
(defrel (three-partitiono l partitions sum)
    (fresh (e0 e1 e2 rest-l e0+e1)
        (conde
            ;; Base case
            ((== l '())
                (== partitions '()))))

            ;; Recursive case
            ((riffleo `(,e0 ,e1 ,e2) rest-l l)
                (pluso e0 e1 e0+e1)
                (pluso e0+e1 e2 sum)))
```

At this point we've verified that either the list `l` is empty, or it contains 3 numbers that add up to `sum`. If we verify that the rest of `l` -- that is, all elements other thatn the three chosen, `e0`, `e1`, and `e2` -- also satisfies the three-partition relation, then the definition will be complete. We can achieve this with a recursive call, along with an assertion that `partitions` is made up of the triple `e0` `e1` `e2`, as well as the rest of the partitions discovered nondeterministically in the recursive call. So that gives us:

```scheme
(defrel (three-partitiono l partitions sum)
    (fresh (e0 e1 e2 rest-l e0+e1 rest-partitions)
        (conde
            ;; Base case
            ((== l '())
                (== partitions '()))))

            ;; Recursive case
            ((riffleo `(,e0 ,e1 ,e2) rest l)
                (pluso e0 e1 e0+e1)
                (pluso e0+e1 e2 sum)
                (three-partitiono rest-l rest-partitions sum)
                (== partitions `((,e0 ,e1 ,e2) . ,rest-partitions))))
```

This completes the definition of the three-partition relation, although there is an opportunity for two correctness-preserving transformations:
1. Move any unifications to the top of the conjunction.
2. Move the recursive to the bottom of the conjunction.

Applying both of these yields:
```scheme
(defrel (three-partitiono l partitions sum)
    (fresh (e0 e1 e2 rest-l e0+e1 rest-partitions)
        (conde
            ;; Base case
            ((== l '())
                (== partitions '()))))

            ;; Recursive case
            ((== partitions `((,e0 ,e1 ,e2) . ,rest-partitions)
                (riffleo `(,e0 ,e1 ,e2) rest l)
                (pluso e0 e1 e0+e1)
                (pluso e0+e1 e2 sum)
                (three-partitiono rest-l rest-partitions sum))))
```
Notice that this adds more confidence that these are non-overlapping conditions, since `partitions` is empty in the first case and non-empty in the second.

This completes the definition of the three-partition relation. And it works great. Consider the following example, [taken from Wikipedia](https://en.wikipedia.org/wiki/3-partition_problem#Example), noting that the integers must be converted into Oleg numerals using `build-num` in order to work with the `pluso` operation.

```scheme
(define example (map build-num
    '(20 23 25 30 49 45 27 30 30 40 22 19)))
```

And we don't know what `sum` should be, so we can introduce a fresh variable to act as a statement like "there should exist a sum, but I don't know what it is".

```
> (run 1 (partitions sum) (three-partitiono example partitions sum))
(((((0 0 1 0 1) (1 0 0 1 1) (1 0 1 1 0 1))
    ((1 1 1 0 1) (1 1 0 1 1) (0 0 0 1 0 1))
    ((0 1 1 1 1) (0 1 1 1 1) (0 1 1 1 1))
    ((1 0 0 0 1 1) (0 1 1 0 1) (1 1 0 0 1)))
   (0 1 0 1 1 0 1)))
```

It produces a solution, but for readability's sake we can apply `deep-unbuild-num` to the solution. This is a Scheme helper function which converts a nested list of Oleg numbers back into base 10.

```
> (deep-unbuild-num (run 1 (partitions sum) (three-partitiono example partitions sum)))
((((20 25 45) (23 27 40) (30 30 30) (49 22 19)) 90))
```

This matches our expectation per the aforementioned Wikipedia example that there is a three-partitioning of the given list of integers, whose partitions each sum to 90.



The code from this article, in full:
```scheme
(load "~/CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(load "~/CodeFromTheReasonedSchemer2ndEd/trs2-arith.scm")

(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o z0 z1)
        (conde
            ;; When one of `a` or `b` is empty
            ((== a '()) (== b '()) (== o '()))
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are nonempty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o))
                (conde
                    ((== car-o car-a) (== z0 cdr-a) (== z1 b))
                    ((== car-o car-b) (== z0 a) (== z1 cdr-b)))
                    
                (riffleo z0 z1 cdr-o)))))


(defrel (three-partitiono l partitions sum)
    (fresh (e0 e1 e2 rest-l e0+e1 rest-partitions)
        (conde
            ;; Base case
            ((== l '())
                (== partitions '())))

            ;; Recursive case
            ((== partitions `((,e0 ,e1 ,e2) . ,rest-partitions))
                (riffleo `(,e0 ,e1 ,e2) rest-l l)
                (pluso e0 e1 e0+e1)
                (pluso e0+e1 e2 sum)
                (three-partitiono rest-l rest-partitions sum))))

                
(define example (map build-num
    '(20 23 25 30 49 45 27 30 30 40 22 19)))
    
(define (deep-unbuild-num l)
    (cond
        ((or (null? l) (eq? (car l) 0) (eq? (car l) 1))
            (unbuild-num l))
        (else
            (map deep-unbuild-num l))))
            
(define solution (deep-unbuild-num (run 1 (partitions sum) (three-partitiono example partitions sum))))
```
