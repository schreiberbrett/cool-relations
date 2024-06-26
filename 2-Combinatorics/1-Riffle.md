Brett Schreiber, 24 October 2022

Consider the way a dealer shuffles cards like in the below GIF. Two separate decks get combined into one larger deck.

![](https://www.geogebra.org/resource/wbf3as26/nNXngeMdvMRfGZiL/material-wbf3as26.png)


So let's model this relationship discretely. In miniKanren, this would be a relationship between
1. a list `a`
2. a list `b`
3. and their riffled output `o`.

We will write the miniKanren code as if it is checking to make sure the riffled output is correct and follows logically from the inputs. First, let's check the easy case:

> If one of `a` or `b` is empty then the riffle output is is equal to the other list.

In miniKanren, this would be:

```
(defrel (riffleo a b o)
    (conde
        ;; If one of `a` or `b` is empty then the riffle output is is equal to the other list.
        ((== a '()) (== b o))
        ((== b '()) (== a o))))

        ;; TODO: When both `a` and `b` are non-empty
```

These cases are overlapping when both `a` and `b` are empty. It is good practice to "expand out" the overlapping cases. So the base cases become:
> If `a` and `b` are both empty, then the output is empty.
> If `a` is empty and `b` is non-empty, then the output is equal to `b`.
> If `a` is non-empty and `b` is empty, then the output is equal to `a`.

We need fresh variables now to capture the potential non-emptiness of `a` and `b`. I will use a `car-*` and `cdr-*` convention.

```
(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b)
        (conde
            ;; If `a` and `b` are both empty, then the output is empty.
            ((== a '()) (== b '()) (== o '()))
            
            ;; If `a` is non-empty and `b` is empty, then the output is equal to `a`.
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            
            ;; If `a` is empty and `b` is non-empty, then the output is equal to `b`.
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b)))))

            ;; TODO: When both `a` and `b` are non-empty
```

The more difficult case is when both `a` and `b` are non-empty. Let's look at "verifying" a riffle shuffle, per the above GIF: either a left card falls into the output deck, or the right card does. So we have two non-empty cases:

> Either `car-a` gets shuffled in, (that is, `car-a` equals `car-o`). And so the rest of the output (`cdr-o`). is the result of riffling `cdr-a` with `b`.
> Or `car-b` gets shuffled in, in which case the rest of the output is the result of riffling `a` with the `cdr-b`.

We also have implicitly stated that `o` is non-empty since it is made up of `car-o` and `cdr-o`. Which makes sense since riffling two non-empty decks should not result in an empty output. So this is true in both cases and therefore can be lifted outside of the `conde`. (As a general rule of thumb, lift common unification `(== ... ...)` clauses upwards, and push everything else out downwards.)

In code:

```
(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o)
        (conde
            ;; If `a` and `b` are both empty, then the output is empty.
            ((== a '()) (== b '()) (== o '()))
            
            ;; If `a` is non-empty and `b` is empty, then the output is equal to `a`.
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            
            ;; If `a` is empty and `b` is non-empty, then the output is equal to `b`.
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are non-empty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o))
                (conde
                    ((== car-o car-a) (riffleo cdr-a b cdr-o))
                    ((== car-o car-b) (riffleo a cdr-b cdr-o)))))))
```

This completes the riffle relation.

# Optimizing `riffleo`

Notice that our previous definition of `riffleo` leaves recursive calls within two different `conde` clauses. We can use a mechanical correctness-preserving transformation to introduce three placeholder arguments in an unconditional call to unify against conditionally. A miniKanren compiler would be able to do this step automatically. This transformation yields:

```
(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o z0 z1 z2)
        (conde
            ;; If `a` and `b` are both empty, then the output is empty.
            ((== a '()) (== b '()) (== o '()))
            
            ;; If `a` is non-empty and `b` is empty, then the output is equal to `a`.
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            
            ;; If `a` is empty and `b` is non-empty, then the output is equal to `b`.
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are non-empty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o))
                (conde
                    ((== car-o car-a) (== z0 cdr-a) (== z1 b) (== z2 cdr-o))
                    ((== car-o car-b) (== z0 a) (== z1 cdr-b) (== z2 cdr-o)))
                    
                (riffleo z0 z1 z2)))))
```

We can extract out the common `(== z2 cdr-o)` from both clauses of the inner `conde`. This is another correctness-preserving transformation.

```
(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o z0 z1 z2)
        (conde
            ;; If `a` and `b` are both empty, then the output is empty.
            ((== a '()) (== b '()) (== o '()))
            
            ;; If `a` is non-empty and `b` is empty, then the output is equal to `a`.
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            
            ;; If `a` is empty and `b` is non-empty, then the output is equal to `b`.
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are non-empty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o)) (== z2 cdr-o)
                (conde
                    ((== car-o car-a) (== z0 cdr-a) (== z1 b))
                    ((== car-o car-b) (== z0 a) (== z1 cdr-b)))
                    
                (riffleo z0 z1 z2)))))
```

We can eagerly unify `z2` and `cdr-o` in order to remove all uses of `z2` in the relation, and therefore can be removed:

```
(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o z0 z1)
        (conde
            ;; If `a` and `b` are both empty, then the output is empty.
            ((== a '()) (== b '()) (== o '()))
            
            ;; If `a` is non-empty and `b` is empty, then the output is equal to `a`.
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            
            ;; If `a` is empty and `b` is non-empty, then the output is equal to `b`.
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are non-empty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o))
                (conde
                    ((== car-o car-a) (== z0 cdr-a) (== z1 b))
                    ((== car-o car-b) (== z0 a) (== z1 cdr-b)))
                    
                (riffleo z0 z1 cdr-o)))))
```

Since the only recursive call is in the final `conde` clause, this relation satisfies Will's law.

# Some mathematical properties of riffling

Here is a brief detour into some useful observations about riffling, asserted without proofs:

1. Riffling is _associative_, that is, it does not matter what order a collection of lists is riffled together, all orderings are equivalent. Look for example at the cross-cutting picture from above. It doesn't make sense to reason about which two of the three scenes were cross-cut together first -- they are all equally riffled together.
2. Riffling is _commutative_. In the GIF of the decks of cards above, the dealer could have swapped the two decks in his hands and the result could produce the same shuffled deck.
3. Riffling two lists together produces an output list whose length equals the sum of the lengths of its input lists. In the deck of cards GIF, no cards are created or destroyed, and we are confident that the 
4. If some element `a` comes before some element `b` in one of the input lists to a riffle, then `a` must also come before `b` in the output list. Riffling, in some sense, preserves order of its input lists.

The last two properties are also true of appending two lists together. In fact, riffling is just a generalization of appending, especially since appending two lists constitutes a valid riffle.

> This section is reserved for a future proof using equational reasoning to show that the miniKanren implementation of `riffleo` exhibits the above properties.

# A surprising application

Consider the NP-complete [three-partition problem](https://en.wikipedia.org/wiki/3-partition_problem), which is a relationship between a list of numbers of length `3n`, call it `l`, and a list of triples of length `n`, call it `partitions`, where each partition adds up to the same number, call it `sum`, and each element in each triple of the partition is contained exactly once in `l`.

Let's write a miniKanren relation in the style of **nondeterministic programming**. We will take advantage of the fact that each relation can produce multiple answers, and we code along the "happy path", assuming that the answer returned is the one we were looking for.

Once again, consider the base case first since it is easier. 
- If there are no numbers in `l` to pick, then there are no partitions to make.

```
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

So we can use `riffleo` _nondeterministically_ to pick three elements from `l`. Note also that this yields the elements of the list that are not chosen. We capture these in `rest-l`.
```
(defrel (three-partitiono l partitions sum)
    (fresh (e0 e1 e2 rest-l)
        (conde
            ;; Base case
            ((== l '()) (== partitions '()))))
            ;; Recursive case
            ((riffleo `(,e0 ,e1 ,e2) rest-l l)))
```

Notice here that the base case and the recursive case are non-overlapping. The list is either empty, or it has at least three elements that can be chosen. But what do we do with those three elements? According to the definition of the three-partition problem, they must add up to `sum`. `pluso` only adds up two numbers, so we must introduce an intermediate addition with a fresh variable `e0+e1`, and a second `pluso` to get the final sum. So we have:

```
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

```
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
```
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

```
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

# Further thoughts about relations

How could we model this mixing GIF relationally?

![](https://upload.wikimedia.org/wikipedia/commons/8/8c/Baker%27s_map_mixing.gif)

# Acknowledgements

This implementation of miniKanren (`trs2-impl.scm` and `trs2-arith.scm`) was provided from [Github](https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd) by Daniel P. Friedman, William E. Byrd, Oleg Kiselyov, and Jason Hemann.

Thank you to [John Hydrisko](https://johnhydrisko.substack.com/) for proofreading this article.

# Reference

The final Scheme code from this article, in full:
```scheme
(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o z0 z1)
        (conde
            ;; If `a` and `b` are both empty, then the output is empty.
            ((== a '()) (== b '()) (== o '()))
            
            ;; If `a` is non-empty and `b` is empty, then the output is equal to `a`.
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            
            ;; If `a` is empty and `b` is non-empty, then the output is equal to `b`.
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are non-empty
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
                (== partitions '()))

            ;; Recursive case
            ((== partitions `((,e0 ,e1 ,e2) . ,rest-partitions))
                (riffleo `(,e0 ,e1 ,e2) rest-l l)
                (pluso e0 e1 e0+e1)
                (pluso e0+e1 e2 sum)
                (three-partitiono rest-l rest-partitions sum)))))

                
(define example (map build-num
    '(20 23 25 30 49 45 27 30 30 40 22 19)))
    
(define (deep-unbuild-num l)
    (cond
        ((or (null? l) (eq? (car l) 0) (eq? (car l) 1))
            (unbuild-num l))
        (else
            (map deep-unbuild-num l))))
```

And the result:
```
> (deep-unbuild-num (run 1 (partitions sum) (three-partitiono example partitions sum)))
'((((20 25 45) (23 27 40) (30 30 30) (49 22 19)) 90))
```
