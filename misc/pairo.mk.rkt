#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(provide pairo)

(defrel (pairo x)
  (fresh (h t) (== x `(,h . ,t))))