# Venn Diagram of a multiset of natural numbers in miniKanren

```scheme
(defrel (venn-diagramo x y l c r)
  (conde ((== l x) (== c '()) (== r y)
          (conde ((== x '())) ((== y '()))))
          
         ((fresh (ax dx ay dy)
            (== x `(,ax . ,dx))
            (== y `(,ay . ,dy))))))
```
