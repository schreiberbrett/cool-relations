(load "~/CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(define (nato n)
  (lambda (s)
    (lambda ()
      (let ((n (walk n s)))
	    (cond
	      ((and (number? n) (>= 0 n)) (succeed s))
	      ((var? n) ((nato-helper! n 0) s))
	      (else (fail s)))))))


(defrel (nato-helper! var num)
  (disj
    (== var num)
    (nato-helper! var (+ 1 num))))

#|
;; TODO: implement
(define-syntax letrel
  (syntax-rules ()
    ((letrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (lambda ()
           ((conj g ...) s)))))))
|#

(define (eveno n)
  (lambda (s)
    (lambda ()
      ((let ((n (walk n s)))
        (cond
          ((and (number? n) (even? n)) succeed)
          ((var? n) (eveno-helper! n 0))
          (else fail))) s))))

(defrel (eveno-helper! x n)
  (disj
    (== x n)
    (eveno-helper! x (+ 2 n))))

(define (succo n-1 n)
  (lambda (s)
    (lambda ()
      (let ((n-1 (walk n-1 s))
            (n   (walk n   s)))
        ;; Here we simulate multiple dispatch
        ((cond
          ((and (number? n-1) (number? n) (eq? n-1 (- n 1))) succeed)
          ((and (number? n-1) (var? n)) (== n (+ n-1 1)))
          ((and (var? n-1) (number? n)) (== n-1 (- n 1)))
          ((and (var? n-1) (var? n)) (succo-helper! n-1 n 0))
          (else fail)) s))))) ;; One of the number? checks must have failed. Type failure.

;; TODO letrel
(defrel (succo-helper! n-1 n x)
  (conde
    ((== n-1 x) (== n (+ 1 x)))
    ((succo-helper! n-1 n (+ 1 x)))))
 

(define (<o n m)
  (lambda (s)
    (lambda ()
      (let ((n (walk n s))
            (m (walk m s)))
        ((cond
          ((and (number? n) (number? m) (< n m)) succeed)
          ((and (number? n) (var? m)) (<o-helper1! n m))
          ((and (var? n) (number? m)) (<o-helper2! n m 0))
          ((and (var? n) (var? m)) (conj (nato n) (<o n m)))
          (else fail)) s)))))

(defrel (<o-helper1! ng m)
  (disj
    (== m (+ ng 1))
    (<o-helper1! (+ ng 1) m)))
    
(defrel (<o-helper2! n mg num)
  (if (< num mg)
    (disj (== n num) (<o-helper2! n mg (+ num 1)))
    fail))
    
(defrel (<=o n m)
  (disj
    (== n m)
    (<o n m)))
    
(defrel (>o n m) (<o m n))

(defrel (>=o n m) (<=o m n))

(define (+o a b out)
  (lambda (s)
    (lambda ()
      (let ((a   (walk a s))
            (b   (walk b s))
            (out (walk out s)))
        ((cond
          ((and (number? a) (number? b) (number? out) (eq? (+ a b) out)) succeed)
          ((and (number? a) (number? b)    (var? out)) (== out (+ a b)))
          ((and (number? a)    (var? b) (number? out)) (== b (- out a)))
          ((and (number? a)    (var? b)    (var? out)) (conj (nato b) (+o a b out)))
          ((and    (var? a) (number? b) (number? out)) (== a (- out b)))
          ((and    (var? a) (number? b)    (var? out)) (conj (nato a) (+o a b out)))
          ((and    (var? a)    (var? b) (number? out)) (conj (<=o a out) (+o a b out)))
          ((and    (var? a)    (var? b)    (var? out)) (conj (nato a) (+o a b out)))
          (else fail)) s)))))


#|
(define (*o a b out)
  (lambda (s)
    (lambda ()
      (let ((a   (walk a s))
            (b   (walk b s))
            (out (walk out s)))
        ((cond
          ((and (number? a) (number? b) (number? out) (eq (* a b) out)) succeed)
          ((and (number? a) (number? b)    (var? out)) (== out (* a b)))
          ((and (number? a)    (var? b) (number? out)) (if (eq? a 0) (nato b) (
|#

(defrel (uniono a b out)
  (fresh (x xs y ys)
    (conde
      ((== a '()) (== b '()) (== out '()))
      ((== a `(,x . ,xs)) (== b '()) (== out a))
      ((== a '()) (== b `(,y . ,ys)) (== out b))
      ((== a `(,x . ,xs)) (== b `(,y . ,ys))
        (fresh (an bn out-rec)
          (conde
            ((== x y) (== out `(,x . ,out-rec)) (== an xs) (== bn ys))
            ((<o x y) (== out `(,x . ,out-rec)) (== an xs) (== bn b))
            ((>o x y) (== out `(,y . ,out-rec)) (== an a) (== bn ys)))
          (uniono an bn out-rec))))))


(defrel (multiset-uniono a b out)
  (fresh (x xs y ys)
    (conde
      ((== a '()) (== b '()) (== out '()))
      ((== a `(,x . ,xs)) (== b '()) (== out a))
      ((== a '()) (== b `(,y . ,ys)) (== out b))
      ((== a `(,x . ,xs)) (== b `(,y . ,ys))
        (fresh (an bn out-rec)
          (conde
            ((== x y) (== out `(,x ,y . ,out-rec)) (== an xs) (== bn ys))
            ((<o x y) (== out `(,x . ,out-rec)) (== an xs) (== bn b))
            ((>o x y) (== out `(,y . ,out-rec)) (== an a) (== bn ys)))
          (multiset-uniono an bn out-rec))))))
        

(define (range start stop)
  (cond
    ((eq? start stop) '())
    (else (cons start (range (+ 1 start) stop)))))

(define (prime? x)
  (and
    (> x 1)
    (none (lambda (n) (eq? 0 (mod x n))) (range 2 x))))
  
(define (none p l)
  (cond
    ((null? l) #t)
    (else (and (not (p (car l))) (none p (cdr l))))))



(define (primeo x)
  (lambda (s)
    (lambda ()
      (let ((x (walk x s)))
        ((cond
          ((and (number? x) (prime? x)) succeed)
          ((var? x) (conj (nato x) (primeo x)))
          (else fail)) s)))))


;; The power of relational programming!!!
(defrel (goldbacho p a b)
  (eveno p)
  (+o a b p)
  (primeo a)
  (primeo b))

(defrel (brett x)
  (== x 'foo))
  

(defrel (appendo a b out)
  (conde
    [(== a '()) (== b out)]
    [(fresh (first rest out-rest)
      (== a `(,first . ,rest)) (== out `(,first . ,out-rest))
      (appendo rest b out-rest))]))

(defrel (lengtho l n)
  (conde
    [(== l '()) (== n 0)]
    [(fresh (first rest n-1)
     (== l `(,first . ,rest))
     (succo n-1 n)
     (lengtho rest n-1))]))

(defrel (differento a b)
  (conde
    [(== a 'red) (== b 'blue)]
    [(== a 'blue) (== b 'red)]
    [(== a 'red) (== b 'green)]
    [(== a 'green) (== b 'red)]
    [(== a 'blue) (== b 'green)]
    [(== a 'green) (== b 'blue)]))
    

(defrel (bretto a b c d e)
  (differento a b)
  (differento b c)
  (differento a c)
  (differento b d)
  (differento c d)
  (differento d e))

  
  
  



