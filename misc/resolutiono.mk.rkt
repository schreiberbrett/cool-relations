#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(require "appendo.mk.rkt")
(require "riffleo.mk.rkt")

(defrel (resolutiono clauses)
  (fresh (rest-clauses p n c1 c2 x new-clause rest-p rest-n)
         (conde ((== clauses `(() . ,rest-clauses)))

                ((riffleo `(,c1 ,c2) rest-clauses clauses)
                 (riffleo `(,p) `(,n) `(,c1 ,c2))
                 (riffleo `((pos ,x)) rest-p p)
                 (riffleo `((neg ,x)) rest-n n)
                 (appendo rest-p rest-n new-clause)
                 (resolutiono `(,new-clause . ,clauses))))))
