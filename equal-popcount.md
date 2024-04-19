# Equal Popcount

Assert that two lists of bits (not necessarily ending in 1) have the same number of 0s and 1s.

This relation can be expressed as two riffles like so:

```scheme
(defrel (equal-popcounto-riffle l1 l2)
  (fresh (zeros ones)
    (==* 0 zeros)
    (==* 1 ones)
    (riffleo zeros ones l1)
    (riffleo zeros ones l2)))
```

## A helper relation
- `l1` and `l2` are permutations of each other when `diff` is `'()`.
- `diff` is popcount `l1` - popcount `l2`
- `b0` and `b1` are proxies for 0 and 1. If you leave them fresh you get proxies for 1 and 0, respectively.
- Assumes `(=/= b0 b1)`.
```scheme
(defrel (equal-popcount-helpero b0 b1 l1 l2 diff)
  (conde ((== l1 '()) (== l2 '()) (== diff '()))
         ((fresh (a1 a2 d1 d2 rec)
            (== l1 `(,a1 . ,d1))
            (== l2 `(,a2 . ,d2))
            
            (conde ((== a1 a2) (== rec diff))
                   ((== a1 b1) (== a2 b0) (+1o rec diff))
                   ((== a1 b0) (== a2 b1) (+1o diff rec)))
            (equal-popcount-helpero b0 b1 d1 d2 rec)))))
```
