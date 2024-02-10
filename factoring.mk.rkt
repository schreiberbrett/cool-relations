#lang racket

(require "../faster-minikanren/mk.rkt")

(defrel (bananaso l)
  (conde ((== l '()))
         ((fresh (d)
            (== l `(banana . ,d))
            (bananaso d)))))

(define (v1 n)
  (run n (a b) (conde ((bananaso a))
                      ((bananaso b)))))

(define (v2 n)
  (run n (a b)
    (fresh (x)
      (conde ((== x a))
             ((== x b)))
      (bananaso x))))

