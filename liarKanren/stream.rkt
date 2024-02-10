#lang typed/racket

(provide Stream count-up-inf map-inf filter-inf)

(define-type (Stream a) (U Null
                           (Pairof a (Stream a))
                           (-> (Stream a))))

(: count-up-inf (-> Number (Stream Number)))
(define (count-up-inf n)
  (cons n (lambda () (count-up-inf (+ 1 n)))))

;; map and filter inspired by the -inf functions in trs2-impl.scm.
(: map-inf (All (a b) (-> (-> a b) (Stream a) (Stream b))))
(define (map-inf f s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf) (cons
                    (f (car s-inf))
                    (map-inf f (cdr s-inf))))

    (else (lambda ()
            (map-inf f (s-inf))))))

(: filter-inf (All (a) (-> (-> a Boolean) (Stream a) (Stream a))))
(define (filter-inf p? s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf) 
     (if (p? (car s-inf))
         (cons (car s-inf) (filter-inf p? (cdr s-inf)))
         (filter-inf p? (cdr s-inf))))

    (else (lambda () (filter-inf p? (s-inf))))))




#;(: diagonalize-inf (All (a b) (-> (Stream a) (Stream b) (Stream (Pairof a b)))))
#;(define (diagonalize-inf $1 $2)
  #f)


#;(define (diagonalize-inf-helper $1 $2 seen1 seen2)

  
    (cond ((and (null? $1) (null? $2)) '())
          ((null? $1)
           (map-inf (lambda (x2)
                      (map (lambda (x1) (cons x1 x2)) seen1)) $2))

          ((null? $2)
           (map-inf (lambda (x1)
                      (map (lambda (x2) (cons x1 x2)) seen2)) $1))))
      





