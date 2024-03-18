# 3-coloring a graph in minikanren

Given a graph `g` (represented as an edge-set) and a mapping `m` from natural number to `'red`, `'green`, or `'blue` (represented as an indexable list), verify that the mapping is a valid 3-coloring of the graph.

Recurs on `g` using the `set` induction strategy, ordered by pairs of nats. From [./data-structures.md](./data-structures.md). The 





```scheme
(defrel (3color₁ᵒ g m)
  (conde ((== g '()))
         ((fresh (a d u v cᵤ cᵥ)
            (== g `(,a . ,d))
            (== a `(,u ,v))
            (different-colorsᵒ cᵤ cᵥ)
            (list-refᵒ m u cᵤ)
            (list-refᵒ m v cᵥ)
            (conde ((== d `()))
                   ((fresh (ad dd)
                      (== d `(,ad . ,dd))
                      (<ₚᵒ a ad)
                      (3color₁ᵒ d m))))))))
```




`different−colorsᵒ`: Written positively, no disequality needed.

```scheme
(defrel (different-colorsᵒ c₁ c₂)
  (conde ((== c₁ 'red)   (== c₂ 'blue))
         ((== c₁ 'red)   (== c₂ 'green))
         ((== c₁ 'blue)  (== c₂ 'red))
         ((== c₁ 'blue)  (== c₂ 'green))
         ((== c₁ 'green) (== c₂ 'red))
         ((== c₁ 'green) (== c₂ 'blue))))
```

## Deriving `(Ord (Nat, Nat))`

The less-than relation over natural numbers:

```scheme
(defrel (<ₙᵒ n₁ n₂)
  (fresh (n₂-1)
    (== n₂ `(s . ,n₂-1))
    (conde ((== n₁ '()))
           ((fresh (n₁-1)
              (== n₁ `(s . ,n₁-1))
              (<ₙᵒ n₁-1 n₂-1))))))
```

The less-than relation over pairs of natural numbers. (Using 2-lists instead of actual cons pairs to avoid running into the list of successors of the car and cdr.)

```
(defrel (<ₚᵒ p₁ p₂)
  (fresh (l₁ r₁ l₂ r₂ α₁ α₂)
    (== p₁ `(,l₁ ,r₁))
    (== p₂ `(,l₂ ,r₂))
    
    (conde ((<ₙᵒ l₁ l₂))
           ((== l₁ l₂) (<ₙᵒ r₁ r₂)))))
```

Factoring out the common `<ₙᵒ`.

```scheme
(defrel (<ₚᵒ p₁ p₂)
  (fresh (l₁ r₁ l₂ r₂ α₁ α₂)
    (== p₁ `(,l₁ ,r₁))
    (== p₂ `(,l₂ ,r₂))
    
    (conde ((== `(,α₁ ,α₂) `(,l₁ ,l₂)))
           ((== l₁ l₂) (== `(,α₁ ,α₂) `(,r₁ ,r₂))))

    (<ₙᵒ α₁ α₂)))
```

