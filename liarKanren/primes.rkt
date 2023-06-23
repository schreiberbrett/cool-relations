#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(require "util.rkt")

(provide primeo prime-factorso)

(define (primeo n)
  (lambda (s)
    (let ((n (walk n s)))
      (lambda ()
        (cond
          ((and (number? n) (prime? n)) (succeed s))
          ((var? n) ((==-inf n primes-inf) s))
          (else (fail s)))))))

;; Adapted from Graham Hutton's lazy Haskell implementation.
;; Demonstrated on Computerphile: https://www.youtube.com/watch?v=bnRNiE_OVWA
(define (sieve ns)
  (cond
    ((pair? ns) (cons (car ns) (sieve (filter-inf (lambda (x) (not (zero? (modulo x (car ns))))) (cdr ns)))))
    (else (lambda () (sieve (ns))))))


(define primes-inf (sieve (count-up-inf 2)))

;; via ChatGPT 3.5: "Write me a function (prime? ) in Racket"
(define (prime? n)
  (cond
    [(<= n 1) #f] ; Numbers less than or equal to 1 are not prime
    [(= n 2) #t] ; 2 is the only even prime number
    [(even? n) #f] ; Even numbers greater than 2 are not prime
    [else (check-prime n 3)])) ; Check for primality starting from 3

(define (check-prime n divisor)
  (cond
    [(> (* divisor divisor) n) #t] ; No divisor found, n is prime
    [(zero? (modulo n divisor)) #f] ; Divisible by divisor, not prime
    [else (check-prime n (+ divisor 2))])) ; Try next odd divisor



;; -- Prime factorization --

(define (sorted-prime-factors n)
  (sorted-prime-factors-helper n primes-inf))

(define (sorted-prime-factors-helper n primes-left)
  (cond
    ((pair? primes-left) (let ((p (car primes-left)))
                           (cond
                             ((> p n) '())
                             ((zero? (modulo n p)) (cons p (sorted-prime-factors-helper (/ n p) primes-left)))
                             (else (sorted-prime-factors-helper n (cdr primes-left))))))

    (else (sorted-prime-factors-helper n (primes-left)))))


(defrel (prime-factorso n factors)
  (==-inf `(,n . ,factors) (map-inf (lambda (x)
                                      `(,x . ,(sorted-prime-factors x))) (count-up-inf 2))))



















