#lang racket

(require "../../first-order-miniKanren/mk-fo.rkt")

(define snippet
  (fresh (a b c)
         (conde ((== a 'apple) (== b 'banana) (== c 'cherry))
                ((== a 'alpha) (== b 'bravo) (== c 'charlie))
                ((== a 'alice) (== b 'bob) (== c 'carol)))))