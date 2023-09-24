#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

#;(require (rename-in "../../first-order-miniKanren/mk-fo.rkt" [define-relation defrel]))

(require "pairo.mk.rkt")

(provide riffleo)

;; This is the basic definition of riffleo. It seems to work just fine, but it may diverge on certain inputs.
#;(defrel (riffleo l1 l2 lo)
    (fresh (h t to)
           (conde ((== l1 '()) (== l2 '()) (== lo '()))
                  ((== l1 `(,h . ,t)) (== lo `(,h . ,to)) (riffleo t l2 to))
                  ((== l2 `(,h . ,t)) (== lo `(,h . ,to)) (riffleo l1 t to)))))


;; This version of riffleo has only one recursive call, so it should diverge less. But I don't know exactly when.
(defrel (riffleo l1 l2 lo)
  (fresh (h1 t1 h2 t2 ho to z0 z1)
         (conde ((== l1 '()) (== l2 '()) (== lo '()))
                ((pairo l1) (== l2 '()) (== lo l1))
                ((== l1 '()) (pairo l2) (== lo l2))

                ((== l1 `(,h1 . ,t1))
                 (== l2 `(,h2 . ,t2))
                 (== lo `(,ho . ,to))

                 ;; Avoid an extra recursive call by introducing z0 and z1
                 (conde ((== ho h1) (== z0 t1) (== z1 l2))
                        ((== ho h2) (== z0 l1) (== z1 t2)))
                   
                 (riffleo z0 z1 to)))))