#lang racket
(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(require "riffleo.mk.rkt")

(provide picko)

(defrel (picko picked unpicked l)
  (conde ((== picked '()) (== unpicked l))
         
         ((fresh (a d all-but-a)
                 (== picked `(,a . ,d))
                 (riffleo `(,a) all-but-a l)
                 (picko d unpicked all-but-a)))))