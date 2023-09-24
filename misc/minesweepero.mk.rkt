#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

;; A one-dimensional version of minesweeper
(defrel (minesweepero l)
  (conde ((== l '()))
         ((== l '(0)))
         ((== l '(*)))
         ((== l '(0 0)))
         ((== l '(1 *)))
         ((== l '(* 1)))
         ((== l '(* *)))
         ((fresh (a b c x y rest)
                 (== l `(,a ,b ,c . ,rest))
                 (not-bombo x)
                 (not-bombo y)
                 ;; Remove fresh veriables, list out all combos
                 (conde ((== `(,a ,b ,c) `( * 2  *)))
                        ((== `(,a ,b ,c) `( * 1 ,y)))
                        ((== `(,a ,b ,c) `(,x 1  *)))
                        ((== `(,a ,b ,c) `(,x 0 ,y))))

                 (minesweepero `(,b ,c . ,rest))))))

(defrel (not-bombo x)
  (conde ((== x 0))
         ((== x 1))
         ((== x 2))))