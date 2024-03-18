# Data structures in miniKanren

Below is a list of miniKanren relations, each describing a data structure. The relations should not be used directly, because they they can ground variables too soon. Instead, use these as a pattern match for each clause.

Related to the logical rule:

```
  ((p ∧ a)     ∨ (¬p ∧ b))
∧ ((p ∧ x)     ∨ (¬p ∧ y))
-----------------------
  ( p ∧ a ∧ x) ∨ (¬p ∧ b ∧ y)
```

## List

```haskell
data List a = Cons a (List a) | Nil
```

```
(defrel (listo l)
  (conde ((== l '())
         ((fresh (a d) (== l `(,a . ,d)) (listo d))))))
```

## Unary natural numbers:
```haskell
data UnaryNat = Zero | Succ UnaryNat
```

```scheme
(defrel (peanoo n)
  (conde ((== n '())
         ((fresh (n-1)
            (== n `(s . ,n-1))
            (peanoo n-1))))))
```

Peano numbers are ordered.
```scheme
(defrel (peano-<o n m)
  (fresh (n-1 m-1)
    (== m `(s . ,m-1))
    (conde ((== n '()))
           ((== n `(s . ,n-1))
            (peano-<o n-1 m-1)))))
```


## Binary natural numbers:

The solution set is a subset of `listo`. It directly encodes `listo` with further binary constraints:
* `b :: List (0 | 1)`
* `b`s final digit must be `1`.

```scheme
(defrel (olego n)
  (conde ((== n '()))
         ((fresh (bit ⌊n/2⌋)
            (== n `(,bit . ,⌊n/2⌋))
            (conde ((== bit 0) (>0o ⌊n/2⌋))
                   ((== bit 1)))
            (olego ⌊n/2⌋)))))
```

`<=o` for Oleg numbers is provided by the core arithmetic miniKanren library.

## Sets

```haskell
data Set a = List a
```

The set is a sorted, unique list. Therefore, every element is less than its tail, not less-than-or-equal.

The `<o` must be an actual relation.

```
(defrel (unique-ordered-listo s)
  (conde ((== s '())
         ((fresh (a d)
            (== s `(,a . ,d))
            (conde ((== d `()))
                   ((fresh (ad dd)
                      (== d `(,ad . ,dd))
                      (<o a ad)
                      (unique-ordered-listo d)))))))))
```


## Multisets

Multisets can be 

