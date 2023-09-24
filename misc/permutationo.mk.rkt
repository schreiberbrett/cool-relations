;; Relationship between two lists which are permutations of each other

#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
#;(require "riffleo.mk.rkt")

(provide permutationo)

;; Pick one element out of the list, append the rest together.
;; Like membero, but includes the "rest"

(defrel (pick1o picked unpicked choices)
  (fresh (h t)
         (== choices `(,h . ,t))
         (conde ((== picked h) (== unpicked t))
                ((fresh (trest)
                        (== unpicked `(,h . ,trest))
                        (pick1o picked trest t))))))

(defrel (permutationo l1 l2)
  (conde ((== l1 '()) (== l2 '()))
         ((fresh (h t1 t2)
                 (== `(,h . ,t1) l1)
                 (pick1o h t2 l2)
                 (permutationo t1 t2)))))

#;(run* (q) (permutationo q '(a b c)))
#;'(((a b c))
    ((a c b))
    ((b a c))
    ((c a b))
    ((b c a))
    ((c b a)))

#;(run* (q) (permutationo '(a b c) q))
;; Diverges
