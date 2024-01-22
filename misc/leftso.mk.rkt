#lang racket

(require "../../faster-minikanren/mk.rkt")

(defrel (xoro x)
  (fresh (a b)
         (conde ((== x `(left ,a)))
                ((== x `(right ,b)))
                ((== x `(both ,a ,b))))))


(defrel (leftso l o)
  (conde ((== l '()) (== o '()))
         ((fresh (a d left _ rec)
                 (== l `(,a . ,d))
                 (conde ((== a `(left ,left)) (== o `(,left . ,rec)))
                        ((== a `(right ,_)) (== o rec))
                        ((== a `(both ,left ,_)) (== o `(,left . ,rec))))
                 (leftso d rec)))))