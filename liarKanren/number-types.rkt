#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(require "util.rkt")

(provide naturalo eveno oddo integero)

(define (naturalo n)
  (lambda (s)
    (let ((n (walk n s)))
      (lambda ()
        (cond
          ((exact-nonnegative-integer? n) (succeed s))
          ((var? n) ((==-inf n naturals-inf) s))
          (else (fail s)))))))

(define (eveno n)
  (lambda (s)
    (let ((n (walk n s)))
      (lambda ()
        (cond
          ((and (number? n) (even? n)) (succeed s))
          ((var? n) ((==-inf n evens-inf) s))
          (else (fail s)))))))

(define (oddo n)
  (lambda (s)
    (let ((n (walk n s)))
      (lambda ()
        (cond
          ((and (number? n) (even? n)) (succeed s))
          ((var? n) ((==-inf n odds-inf) s))
          (else (fail s)))))))

(define (integero n)
  (lambda (s)
    (let ((n (walk n s)))
      (lambda ()
        (cond
          ((exact-integer? n) (succeed s))
          ((var? n) ((==-inf n integers-inf) s))
          (else (fail s)))))))

(define naturals-inf (count-up-inf 0))

(define evens-inf (map-inf (lambda (x)
                             (* 2 x)) naturals-inf))

(define odds-inf (map-inf (lambda (x)
                            (+ (* 2 x) 1)) naturals-inf))

(define integers-inf (cons 0
                           (append-map-inf (lambda (x)
                                             (list x (- x))) (count-up-inf 1))))