#lang racket

(require "../../faster-minikanren/mk.rkt")

(provide oddo)

(defrel (oddo l)
  (conde ((fresh (x)
                 (== l `(,x))))
         ((fresh (a₁ a₂ d)
                 (== l `(,a₁ ,a₂ . ,d))
                 (oddo d)))))