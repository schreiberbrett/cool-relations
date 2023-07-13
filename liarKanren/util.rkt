#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(require "stream.rkt")

(provide ==-inf)

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


;; set-and-predicate-relation : constructor for relations
;; Given a stream of all elements from the set, and a predicate that all elements must obey, define a miniKanren relation.
;; Do not pass in incomplete streams, that would make miniKanren incomplete!
(define (setpredo set predicate?)
  (lambda (x)
    (lambda (s)
      (let ((x (walk x s)))
        (lambda ()
          (cond
            ((predicate? x) (succeed s))
            ((var? x) ((==-inf x set-inf) s))
            (else (fail s))))))))