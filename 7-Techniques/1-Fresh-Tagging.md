# Keep variables fresh by using binary lists for tagging.

I want to maintain an RSVP list in miniKanren. Something like this:

```
'((fred    yes)
  (allison maybe)
  (bob     no)
  (gina    maybe)
  (tracy   no)
  (tim     yes))
```

I want to find everyone who isn't a definite `yes`, so that I know who to not invite next time.


Recall the Haskell data type `Xor`:

```haskell
data Xor a b = Left a
             | Right b
             | Both a b
```

The equivalent in miniKanren:

```scheme
(defrel (xoro x)
  (fresh (a b)
         (conde ((== x `(left ,a)))
                ((== x `(right ,b)))
                ((== x `(both ,a ,b))))))
```

Suppose we had the following Haskell function:

```haskell
lefts :: [Xor a b] -> [a]
lefts [] = []
lefts (x:xs) = case x of
    (Left left) -> left:(lefts xs)
    (Right _) -> lefts xs
    (Both left _) -> left:(lefts xs)
```

Translating that into miniKanren:

```scheme
(defrel (leftso l o)
  (conde ((== l '()) (== o '()))
         ((fresh (a d left _ rec)
            (== l `(,a . ,d))
            (conde ((== a `(left ,left)) (== o `(,left . ,rec)))
                   ((== a `(right ,_)) (== o rec))
                   ((== a `(both ,left ,_)) (== o `(,left . ,rec))))
            (leftso d rec)))))
```
