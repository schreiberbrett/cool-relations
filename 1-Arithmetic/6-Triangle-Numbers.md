# Triangle Numbers

```math
T_n = \frac{n(n+1)}{2} \\
2T_n = n(n+1)
```

```scheme
(defrel (triangleo n Tn)
  (fresh (n+1)
    (+1o n n+1)
    (*o n n+1 `(0 . ,Tn))))
```