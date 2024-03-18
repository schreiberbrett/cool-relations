#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(define (eveno n)
  (lambda (s)
    (lambda ()
      ((let ((n (walk n s)))
        (cond
          ((var? n) (eveno-helper! n 0))
          (else (if (and (number? n) (even? n)) succeed fail))))))))

(defrel (eveno-helper! x ground!)
  (conde
    ((== x ground!))
    ((eveno-helper! x (+ 2 ground!)))))
