(load "~/CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(define (nato n)
  (lambda (s)
    (lambda ()
      (let ((n (walk n s)))
	    (cond
	      ((and (integer? n) (>= 0 n)) (succeed s))
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
          ((and (var? n-1) (number? n)) (if (zero? n) fail (== n-1 (- n 1))))
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

(define (squareo x x2)
  (lambda (s)
    (lambda ()
      (let ((x  (walk x s))
            (x2 (walk x2 s)))
        ((cond
          ((and (number? x) (number? x2) (eq? (* x x) x2)) succeed)
          ((and (number? x)    (var? x2)) (== x2 (* x x)))
          ((and    (var? x) (number? x2)) (if (integer? (sqrt x2)) (== x (sqrt x2)) fail))
          ((and    (var? x)    (var? x2)) (conj (nato x) (squareo x x2)))
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


(defrel (pairo x) (fresh (first rest) (== x `(,first . ,rest))))

;; Goldbach conjecture
;; "Every even number >= 4 can be written as the sum of two primes.

;; even number -- n
;; prime number -- p1
;; prime number -- p2

(defrel (goldbach-conjectureo n p1 p2)
  (eveno n) ;; IDK / K
  (+o p1 p2 n)
  (primeo p1)
  (primeo p2))


(defrel (pythagorean-tripleo a b c)
  (nato a)
  (nato b)

  (fresh (a2 b2 c2)
    (squareo a a2)
    (squareo b b2)
    (+o a2 b2 c2)
    
    (squareo c c2))

  (>o a 0)
  (>o b 0))


(defrel (weaveo l r out)
  (conde
    ((== l '()) (== r '()) (== out '()))
    ((pairo l) (== r '()) (== out l))
    ((== l '()) (pairo r) (== out r))
    ((fresh (l-first l-rest r-first r-rest l-rec r-rec out-first out-rest)
       (== l `(,l-first . ,l-rest))
       (== r `(,r-first . ,r-rest))
       (== out `(,out-first . ,out-rest))
       
       (conde
         ((== out-first l-first) (== l-rec l-rest) (== r-rec r))
         ((== out-first r-first) (== l-rec l) (== r-rec r-rest)))
       
       (weaveo l-rec r-rec out-rest)))))


(defrel (three-partitiono nums partitions)
  (fresh (sum) (three-partition-helpero nums partitions sum)))
  
(defrel (three-partition-helpero nums partitions sum)
  (conde
    ((== nums '()) (== partitions '()))
    
    ((fresh (a b ab c rest rest-partitions)
      (== partitions `((,a ,b ,c) . ,rest-partitions))
      (weaveo `(,a ,b ,c) rest nums)
      (+o a b ab)
      (+o ab c sum)
      (three-partition-helpero rest rest-partitions sum)))))
      

(define partition-example
  '(20 23 25 30 49 45 27 30 30 40 22 19))


#|
;; First draft -- line up recursive conditionals
(defrel (picko n picked rest l)
  (conde
    ;; 
    ((== n 0) (== picked '()) (== rest l))
    
    ((fresh (n-1 l-car l-cdr)
      (== l `(,l-car . ,l-cdr))
      (succo n-1 n)
      (conde
        ((fresh (rest-cdr)
           (== rest `(,l-car . ,rest-cdr))
           (picko n picked rest-cdr l-cdr)))

        ((fresh (picked-cdr)
           (== picked `(,l-car . ,picked-cdr))
           (picko n-1 picked-cdr rest l-cdr))))))))
|#     
 
;; Second draft -- line up recursive conditionals
(defrel (picko n picked rest l)
  (conde
    ;; 
    ((== n 0) (== picked '()) (== rest l))
    
    ((fresh (n-1 l-car l-cdr)
      (== l `(,l-car . ,l-cdr))
      (succo n-1 n)
      (fresh (x0 x1 x2)
        (conde
          ((fresh (rest-cdr)
             (== rest `(,l-car . ,rest-cdr))
             (== x0 n) (== x1 picked) (== x2 rest-cdr)))

          ((fresh (picked-cdr)
             (== picked `(,l-car . ,picked-cdr))
             (== x0 n-1) (== x1 picked-cdr) (== x2 rest))))
           
        (picko x0 x1 x2 l-cdr))))))

  
(defrel (vertex-covero vertices edges cover)
  (fresh (k rest-vertices)
    (picko k cover rest-vertices vertices)
    (every-edge-coveredo edges cover)))

(define sample-vertices '(0 1 2 3 4 5 6 7))

(define sample-edges
  '((0 . 1)
    (0 . 2)
    (0 . 3)
    (0 . 4)
    (0 . 5)
    (6 . 1)
    (6 . 2)
    (6 . 3)
    (6 . 4)
    (6 . 5)
    (6 . 7)
    (7 . 4)
    (7 . 5)))

(defrel (every-edge-coveredo edges cover)
  (conde
    ((== edges '()))
    ((fresh (u v uv rest)
      (== edges `((,u . ,v) . ,rest))
      
      (conde
        ((== u uv))
        ((== v uv)))

      (containso cover uv)
      (every-edge-coveredo rest cover)))))
      
(defrel (containso l x)
  (fresh (first rest)
    (== l `(,first . ,rest))
    (conde
      ((== x first))
      ((containso rest x)))))

;; TODO: force the mapping to be 1-to-1
(defrel (subgraph-isomorphismo g-edges h-edges mapping)
  (conde
    (((== g-edges '()) (== mapping '()))
    ((fresh (g-u g-v h-u h-v g-edges-cdr)
      (== g-edges `((,g-u . ,g-v) . ,g-edges-cdr))
      (lookupo g-u mapping h-u)
      (lookupo g-v mapping h-v)
      
      (fresh (l r)
        (conde
          ((== h-u l) (== h-v r))
          ((== h-u r) (== h-v l)))
        
        (containso h-edges l r))
      
      (subgraph-isomporphismo g-edges-cdr h-edges mapping))))))

(defrel (rewriteo in out)
  (fresh (ro a b c d e f x y z)
    (== in `(conde ((,ro ,a ,b ,c)) ((,ro ,d ,e ,f))))
    (== out
      `(fresh (,x ,y ,z)
        (conde
          ((== ,x ,a) (== ,y ,b) (== ,z ,c))
          ((== ,x ,d) (== ,y ,e) (== ,z ,f)))
        (,ro ,x ,y ,z)))))



(defrel (defrelo x)
  (fresh (y z)
    (== x `(defrel ,y . ,z))
    (defrel-argso y)
    (nonemptyo z)
    (statementso z)))
    
(defrel (defrel-argso x)
  (fresh (r first rest)
    (== x `(,r ,first . ,rest))
    (fresh-listo rest)))
    
(defrel (defrel-bodyo x)
  (nonemptyo x)
  (statementso x)
  (fresh (first rest)
    (== x `(,first . ,rest))
    (statemento first)
    (statementso rest)))

(defrel (pairo x)
  (fresh (l r) (== x `(,l . ,r))))

(defrel (nonemptyo x)
  (pairo x))
  
(defrel (statementso x)
  (conde
    ((== x '()))
    ((fresh (first rest) (== x `(,first . ,rest))
      (statemento first)
      (statementso rest)))))
      
(defrel (statemento x)
  (conde
    ((fresh (r a) (== x `(,r ,a))))
    ((fresh (r a b) (== x `(,r ,a ,b))))
    ((fresh (r a b c) (== x `(,r ,a ,b ,c))))
    ((fresh (fst snd)
      (== x `(conde (,fst) (,snd)))
      (statementso fst) (statementso snd)))
    ((fresh (vars body) (== x `(fresh ,vars ,body)) (nonemptyo vars) (nonemptyo body) (fresh-listo vars) (statementso body)))))

(defrel (fresh-listo l)
  (conde
    ((== l '()))
    ((fresh (first rest) (== l `(,first . ,rest)) (fresh-listo rest))))) 
  
  


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
