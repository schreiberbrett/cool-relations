# Euclid's GCD algorithm in miniKanren

Implemented over Peano nats.

```scheme
(defrel (gcdo1 s n m gcd)
  (conde ((== m '()) (== gcd n))
         ((fresh (m-1 sub)
            (pairo m)
            (+o s sub m n)
            (gcdo1 s m sub gcd)))))
```
