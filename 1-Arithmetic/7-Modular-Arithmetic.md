# Modular Arithmetic

## Congruence

The relation $a \equiv b \mod k$ ought to hold for any Oleg numerals `a`, `b`, and `k`.

One easy answer is to use multiplication. $a \equiv b \mod k \iff ak = bk$.

```scheme
(defrel (congruento/* a b k)
  (fresh (product)
    (*o a k product)
    (*o b k product)))
```
