#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

;(require debug/trace)

(provide naturalo eveno oddo)

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

;; Given an integer greater than 2, return an ordered list of its prime factors

;; Adapted from Graham Hutton's lazy Haskell implementation demonstrated on Computerphile: https://www.youtube.com/watch?v=bnRNiE_OVWA

(define (sieve ns)
  (cond
    ((pair? ns) (cons (car ns) (sieve (filter-inf (lambda (x) (not (zero? (modulo x (car ns))))) (cdr ns)))))
    (else (lambda () (sieve (ns))))))

;; Special goal NOT to be used outside this file.
;; Takes a variable and a (potentially infinite) stream of values.
;; The variable can be equal to any of the values in the stream
(define (==-inf v s-inf)
  (lambda (s)
    (cond
      ((null? s-inf) (succeed s))
      ((pair? s-inf) ((disj2 (== v (car s-inf))
                             (==-inf v (cdr s-inf))) s))

      (else (lambda () ((==-inf v (s-inf)) s))))))

;; inspired by the -inf functions in trs2-impl.scm.
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


(define (count-up-inf n)
  (cons n (lambda () (count-up-inf (+ 1 n)))))

(define naturals-inf (count-up-inf 0))

(define evens-inf (map-inf (lambda (x)
                             (* 2 x)) naturals-inf))

(define odds-inf (map-inf (lambda (x)
                            (+ (* 2 x) 1)) naturals-inf))

(define integers-inf (cons 0
                           (append-map-inf (lambda (x)
                                             (list x (- x))) (count-up-inf 1))))