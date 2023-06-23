#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(provide ==-inf count-up-inf map-inf filter-inf)

;; Dangerous goal only to be used in lies
;; Takes a variable and a (potentially infinite) stream of ground values.
;; The variable can be equal to any of the values in the stream
(define (==-inf v s-inf)
  (lambda (s)
    (cond
      ((null? s-inf) (succeed s))
      ((pair? s-inf) ((disj2 (== v (car s-inf))
                             (==-inf v (cdr s-inf))) s))

      (else (lambda () ((==-inf v (s-inf)) s))))))

(define (count-up-inf n)
  (cons n (lambda () (count-up-inf (+ 1 n)))))

;; map and filter inspired by the -inf functions in trs2-impl.scm.
(define (map-inf f s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf) (cons
                    (f (car s-inf))
                    (map-inf f (cdr s-inf))))

    (else (lambda ()
            (map-inf f (s-inf))))))

(define (filter-inf p? s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf) (if (p? (car s-inf))
                       (cons (car s-inf) (filter-inf p? (cdr s-inf)))
                       (filter-inf p? (cdr s-inf))))

    (else (lambda () (filter-inf p? (s-inf))))))