#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(require "number-types.rkt")
(require "primes.rkt")
(require "stream.rkt")
(require "util.rkt")

(provide naturalo eveno oddo integero primeo prime-factorso)

(define (naturalo n)
  (set-and-unary-predicate-relation naturals-inf
                                    exact-nonnegative-integer?))


(define (eveno n)
  (setpredo evens-inf
            (lambda (n) (and (number? n) (even? n)))))

(define (oddo n)
  (setpredo odds-inf
            (lambda (n) (and (number? n) (odd? n)))))

(define (integero n)
  (setpredo integers-inf
            (integer? n)))

(define (primeo n)
  (setpredo primes-inf
            (lambda (n) (and (number? n) (prime? n)))))




(defrel (prime-factorso n factors)
  (==-inf `(,n . ,factors) (map-inf (lambda (x) `(,x . ,(prime-factors x))) (count-up-inf 2))))