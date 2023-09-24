;; Operations on bags (multisets).

#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(require "appendo.mk.rkt")
(require "permutationo.mk.rkt")

(defrel (uniono b1 b2 bo)
  (appendo b1 b2 bo))

(defrel (bag-== b1 b2)
  (permutationo b1 b2))