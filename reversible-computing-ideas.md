# Reversible relations in miniKanren
```minikanren
(require "../faster-minikanren/main.rkt")

(define ≡ ==)
```

Apparently, this gate is reversible.
```minikanren
(defrel (±ᵒ n₁ n₂ n₁+n₂ n₁−n₂)
  (fresh (n₁−1 n₂−1 n₁−1+n₂−1)
    (conde ((≡ n₂ '())
            (≡ n₁+n₂ n₁)
            (≡ n₁−n₂ n₁))
            
           ((≡ n₂ `(s . ,n₂−1))
            (≡ n₁ `(s . ,n₁−1))
            (≡ n₁+n₂ `(s s . ,n₁−1+n₂−1))
            (±ᵒ n₁−1 n₂−1 n₁−1+n₂−1 n₁−n₂)))))
```

