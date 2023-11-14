#lang racket

(require "../../first-order-miniKanren/common.rkt")
(require "../../first-order-miniKanren/microk-fo.rkt")
(require "../../first-order-miniKanren/mk-fo.rkt")

; Dummy relations, meant to feel scary, or recursive
(define-relation (A n) (== n n))
(define-relation (B n) (== n n))
(define-relation (C n) (== n n))
(define-relation (D n) (== n n))
(define-relation (P n) (== n n))
(define-relation (Q n) (== n n))
(define-relation (R n) (== n n))
(define-relation (S n) (== n n))
(define-relation (X n) (== n n))
(define-relation (Y n) (== n n))
(define-relation (Z n) (== n n))

; conj left-associates
(define test (fresh (x)
                    (P x)
                    (Q x)
                    (R x)
                    (S x)))
#;(conj (conj (conj (P x) (Q x))
              (R x))
        (S x))

; disj right-associates
(define test2 (fresh (x)
                     (conde ((P x))
                            ((Q x))
                            ((R x))
                            ((S x)))))
#;(disj (P x)
        (disj (Q x)
              (disj (R x) (S x))))


; testing both
(define test-both (fresh (x)
                         (conde ((A x) (B x) (C x) (D x))
                                ((P x) (Q x) (R x) (S x))
                                ((X x) (Y x) (Z x)))))
#;(disj (conj (conj (conj (A x) (B x))
                    (C x))
              (D x))
        (disj (conj (conj (conj (P x) (Q x))
                          (R x))
                    (S x))
              (conj (conj (X x) (Y x))
                    (Z x))))


(define test-expansion (conde ('a 'b 'c 'd)
                              ('f)
                              ('p 'q 'r)
                              ('m 'n)
                              ('x 'y 'z 'w)))
#;(disj (conj (conj (conj a
                          b)
                    c)
              d)
        (disj f
              (disj (conj (conj p
                                q)
                          r)
                    (disj (conj m
                                n)
                          (conj (conj (conj x
                                            y)
                                      z)
                                w)))))
(define (frequency x)
  (match x
    ((disj a b) (max (frequency a)
                     (frequency b)))))

(define-relation (appendo x y z)
  (== x y) (== y z))
