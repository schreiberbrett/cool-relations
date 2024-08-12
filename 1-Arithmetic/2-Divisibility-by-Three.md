# Divisibility by 3

TODO: Import from "Relation drag racing"


## Recognizing well-formed Oleg numbers interleaved with `$`
```scheme
(defrel (interleaved-hasho n)
  (conde ((== n '()))
         ((== n '(1)))
         ((fresh (b1 b2 rest)
            (== n `(,b1 $ ,b2 . ,rest))
            (conde ((== b1 0))
                   ((== b1 1)))
            (interleaved-hasho `(,b2 . ,rest))))))
```