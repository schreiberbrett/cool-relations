#lang racket
;; Conversion of the paper "On Fair Relational Conjunction" (http://minikanren.org/workshop/2020/minikanren-2020-paper1.pdf) from Haskell into Racket
;; Relations taken from https://github.com/Lozov-Petr/2021-PEPM-fair-miniKanren

#;(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

;; Even this doesn't help
#;(require "../../Towards-a-miniKanren-with-fair-search-strategies/Code/mk-BFSimp.rkt")


(require (rename-in "../../first-order-miniKanren/mk-fo.rkt" (define-relation defrel)))
(require "../../first-order-miniKanren/tools.rkt")

(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(defrel (appendo x y xy)
  (conde ((== x '()) (== y xy))
         ((fresh (e xs xys)
                 (== x `(,e . ,xs))
                 (== xy `(,e . ,xys))
                 (appendo xs y xys)))))

(defrel (reverso1 x y)
  (conde ((== x '()) (== y '()))
         ((fresh (e xs ys)
                 (== x `(,e . ,xs))
                 (reverso1 xs ys)
                 (appendo ys `(,e) y)))))


#;(run 2 (q) (reverso1 '(1 2 3) q))
#;'(((3 2 1)))

#;(run 2 (q) (reverso1 q '(1 2 3)))
;; Diverges


;; Note the last two lines switched
(defrel (reverso2 x y)
  (conde ((== x '()) (== y '()))
         ((fresh (e xs ys)
                 (== x `(,e . ,xs))
                 (appendo ys `(,e) y)
                 (reverso2 xs ys)))))


#;(run 2 (q) (reverso2 '(1 2 3) q))
;; Diverges

#;(run 2 (q) (reverso2 q '(1 2 3)))
#;'(((3 2 1)))
