# Prime Factorization

Now that I have a set data structure, here's my attempt at writing the relationship between a number `n` and its prime factors `factors`.

There are 2 non-overlapping conditions:
1. `n` is prime. Then `n` is its only factor. (Only is a bit too negative for me, so I'll just say `n` is a factor.)
2. `n` is composite, that is`n = a * b` for some a, b greater than 1. Then `n`'s factors are exactly the union of the factors of `a`and `b`. In other words, `a`'s factors are my factors, and `b`'s factors are my factors, too.

Notice that neither condition succeeds when `n` is 0 and when `n` is 1.

```minikanren
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
```

`>1o` and `*o` come from TRS2E. `primeo` is a cheat relation that takes advantage of the TRS2E miniKanren internals. It succeeds on fully ground, prime, Oleg numbers, enumerates all primes when a fresh variable, and fails on all other inputs.

```minikanren
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
```

Results:

```
> (run 1 (q) (primeo (build-num 37)))
'((_0))
> (run* (q) (primeo (build-num 37)))
'((_0))
> (run* (q) (primeo (build-num 38)))
'()
> (run* (q) (primeo (build-num 39)))
'()
> (run 5 (q) (primeo q))
'(((0 1)) ((1 1)) ((1 0 1)) ((1 1 1)) ((1 1 0 1)))
```
