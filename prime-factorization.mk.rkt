#lang racket

(include "../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(include "../CodeFromTheReasonedSchemer2ndEd/trs2-arith.scm")

(defrel (>0o n)
  (fresh (a d) (== n `(,a . ,d))))

(defrel (elemo n s)
  (fresh (l m r)
    (== s `(,l ,m ,r))
    (conde ((== n '()) (== m #t))
           ((fresh (a d rec)
              (== n `(,a . ,d))
              (conde ((== a 0) (>0o d) (== rec l))
                     ((== a 1) (== rec r)))
              (elemo d rec))))))

(defrel (prime-factorso n factors)
  (conde ((fresh (a b)
            (>1o a)
            (>1o b)
            (*o a b n)
            (prime-factorso a factors)
            (prime-factorso b factors)))
         ((primeo n) (elemo n factors))))

(define (sieve ns)
  (cond ((pair? ns) (cons (build-num (car ns)) (sieve (filter-inf (lambda (x) (not (zero? (modulo x (car ns))))) (cdr ns)))))
        (else (lambda () (sieve (ns))))))
    
(define (filter-inf p? s-inf)
  (cond ((null? s-inf) '())
        ((pair? s-inf) 
         (if (p? (car s-inf))
           (cons (car s-inf) (filter-inf p? (cdr s-inf)))
           (filter-inf p? (cdr s-inf))))
        (else (lambda () (filter-inf p? (s-inf))))))

(define (count-up-inf n)
  (cons n (lambda () (count-up-inf (+ 1 n)))))

(define primes-inf (sieve (count-up-inf 2)))

(define ((==-inf v s-inf) s)
  (cond ((null? s-inf) (succeed s))
        ((pair? s-inf) ((disj2 (== v (car s-inf))
                               (==-inf v (cdr s-inf))) s))

        (else (lambda () ((==-inf v (s-inf)) s)))))

(define ((primeo n) s)
  (cond ((and (nat? n) (prime? n)) (succeed s))
        ((var? n) ((==-inf n primes-inf) s))
        (else (fail s))))
        
(define (nat? n)
  (or (null? n)
      (and (pair? n)
           (or (and (equal? (car n) 0) (pair? (cdr n)) (nat? (cdr n)))
               (and (equal? (car n) 1) (nat? (cdr n)))))))

(define (unbuild-num n)
  (cond ((null? n) 0)
        (else (+ (car n) (* 2 (unbuild-num (cdr n)))))))

(define (prime? n)
  (prime-helper n primes-inf))

(define (prime-helper n primes)
  (cond ((and (pair? primes) (equal? n (car primes))) #t)
        ((and (pair? primes) (< (unbuild-num n) (unbuild-num (car primes)))) #f)
        ((and (pair? primes) (prime-helper n (cdr primes))))
        ((pair? primes) (prime-helper n (cdr primes)))
        (else (prime-helper n (primes)))))

