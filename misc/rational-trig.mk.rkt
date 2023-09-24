#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-arith.scm")

(defrel (quadranceo a1 a2 q)
  (fresh (x1 y1 x2 y2 diff1 diff2 diff1^2 diff2^2)
         (== a1 `(,x1 ,y1))
         (== a2 `(,x2 ,y2))
         (pluso x1 diff1 x2)
         (pluso y1 diff2 y2)
         (pluso diff1^2 diff2^2 q)
         (*o diff1 diff1 diff1^2)
         (*o diff2 diff2 diff2^2)))