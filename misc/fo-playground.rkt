#lang racket

(require "../../first-order-miniKanren/mk-fo.rkt")

(define-relation (pairo x)
  (fresh (h t) (== x `(,h . ,t))))

(define-relation (circleo m has l r)
  (== m `(circle ,has ,l ,r)))

(define-relation (squareo m l r)
  (== m `(square ,l ,r)))

(define-relation (elemo x m)
  (fresh (m l r has h t s _)
         (conde ((== x '()) (circleo m #t l r))
                ((== x `(,h . ,t))
                 (conde ((== h 0) (pairo t) (circleo m has s _))
                        ((== h 0) (pairo t) (squareo m s _))

                        ((== h 1) (circleo m has _ s))
                        ((== h 1) (squareo m _ s)))
                 (elemo t s)))))