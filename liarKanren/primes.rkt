#lang racket

(require "stream.rkt")
(require racket/trace)
(provide prime? primes-inf prime-factors)

;; Adapted from Graham Hutton's lazy Haskell implementation.
;; Demonstrated on Computerphile: https://www.youtube.com/watch?v=bnRNiE_OVWA
(define (sieve ns)
  (cond
    ((pair? ns)
     (cons (car ns) (sieve (filter-inf (lambda (x) (not (zero? (modulo x (car ns))))) (cdr ns)))))
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

(define (prime-factors n)
  (prime-factors-helper n (prime-sieve-to n)))

(define (prime-factors-helper n primes-left)
  (cond
    ((or (null? primes-left) (< n 2)) '())
    ((zero? (modulo n (car primes-left)))
     (cons (car primes-left)
           (prime-factors-helper (/ n (car primes-left))
                                 primes-left)))
    (else (prime-factors-helper n (cdr primes-left)))))

;; https://stackoverflow.com/a/16629458/20284526
(define (prime-sieve-to n)
  (let* ((sz (quotient n 2)) (sv (make-vector sz 1)) (lm (integer-sqrt n)))
    (for ((i (in-range 1 lm))) 
      (cond ((vector-ref sv i)
             (let ((v (+ 1 (* 2 i))))
               (for ((i (in-range (+ i (* v (/ (- v 1) 2))) sz v)))
                 (vector-set! sv i 0))))))
    (cons 2
          (for/list ((i (in-range 1 sz)) 
                     #:when (and (> (vector-ref sv i) 0) (> i 0)))
            (+ 1 (* 2 i))))))




