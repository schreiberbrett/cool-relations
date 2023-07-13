#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm") ; for append-map-inf
(require "stream.rkt")

(provide naturals-inf evens-inf odds-inf integers-inf)

(define naturals-inf (count-up-inf 0))

(define evens-inf (map-inf (lambda (x)
                             (* 2 x)) naturals-inf))

(define odds-inf (map-inf (lambda (x)
                            (+ (* 2 x) 1)) naturals-inf))

(define integers-inf (cons 0
                           (append-map-inf (lambda (x)
                                             (list x (- x))) (count-up-inf 1))))