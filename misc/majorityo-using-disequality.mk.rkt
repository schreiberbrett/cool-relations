#lang racket

#;(require (rename-in "../../first-order-miniKanren/mk-fo.rkt" [define-relation defrel]))
(require "../../faster-minikanren/main.rkt")

(defrel (+1o x x+1)
  (fresh (d)
         (conde ((== x '()) (== x+1 '(+)))
                ((== x `(+ . ,d)) (== x+1 `(+ + . ,d)))
                ((== x `(- . ,d)) (== x+1 d)))))

(defrel (margino x l m)
  (conde ((== l '()) (== m '()))
         ((fresh (a d rec)
                 (== l `(,a . ,d))
                 (conde ((==  a x) (+1o rec m))
                        ((=/= a x) (+1o m rec)))
                 (margino x d rec)))))

(defrel (majorityo x l)
  (fresh (m d)
         (== m `(+ . ,d))
         (margino x l m)))