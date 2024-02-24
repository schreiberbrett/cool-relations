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

```minikanren
(defrel (listᵒ l)
  (conde ((≡ x '())
         ((fresh (a d) (≡ l `(,a . ,d)) (listᵒ d))))))
```

## Unary natural numbers:
```haskell
data UnaryNat = Zero | Succ UnaryNat
```

```minikanren
(defrel (nat₁ᵒ n)
  (conde ((≡ n '())
         ((fresh (n-1)
            (≡ n `(s . ,n-1))
            (nat₁ᵒ n-1))))))
```

## Binary natural numbers:

The solution set is a subset of `listᵒ`. It directly encodes `listᵒ` with further binary constraints:
* `b :: List (0 | 1)`
* `b`s final digit must be `1`.

```minikanren
(defrel (nat₂ᵒ n)
  (conde ((≡ n '()))
         ((fresh (bit ⌊n/2⌋)
            (≡ n `(,bit . ,⌊n/2⌋))
            (conde ((≡ bit 0) (posᵒ ⌊n/2⌋))
                   ((≡ bit 1)))
            (nat₂ᵒ ⌊n/2⌋)))))

(defrel (posᵒ b)
  (fresh (a d)
    (≡ b `(,a . ,d))))
```


## Sets

```haskell
data Set a = List a
```

The set is a sorted, unique list. Therefore, every element is less than its tail, not less-than-or-equal.

```minikanren
(defrel (set₁ᵒ s)
  (conde ((≡ s '())
         ((fresh (a d)
            (≡ s `(,a . ,d))
            (conde ((≡ d `()))
                   ((fresh (ad dd)
                      (≡ d `(,ad . ,dd))
                      (<ᵒ a ad)
                      (set₁ᵒ d)))))))))
```




