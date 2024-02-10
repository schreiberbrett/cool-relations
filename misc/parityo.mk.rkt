#lang racket

(require "../../faster-minikanren/mk.rkt")

(provide parityo)

(defrel (parityo l p)
  (conde ((== l '()) (== p 'even))
         ((fresh (x) (== l `(,x)) (== p 'odd)))
         ((fresh (a₁ a₂ d)
                 (== l `(,a₁ ,a₂ . ,d))
                 (parityo d p)))))