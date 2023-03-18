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


(define riffleo-stupid '(defrel (riffleo-stupid l r out)
  (conde
    ((== l '()) (== r '()) (== out '()))
    ((pairo l) (== r '()) (== out l))
    ((== l '()) (pairo r) (== out r))
    ((fresh (l-first l-rest r-first r-rest l-rec r-rec out-first out-rest)
       (== l `(,l-first . ,l-rest))
       (== r `(,r-first . ,r-rest))
       (== out `(,out-first . ,out-rest))
       
       (conde
         ((== out-first l-first) (riffleo l-rest r out-rest))
         ((== out-first r-first) (riffleo l r-rest out-rest))))))))


(define riffleo-stupid-portion '(conde
  ((== out-first l-first) (riffleo l-rest r out-rest))
  ((== out-first r-first) (riffleo l r-rest out-rest))))


(defrel (riffleo l r out)
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
       
       (riffleo l-rec r-rec out-rest)))))



(defrel (three-partitiono nums partitions)
  (fresh (sum) (three-partition-helpero nums partitions sum)))
  
(defrel (three-partition-helpero nums partitions sum)
  (conde
    ((== nums '()) (== partitions '()))
    
    ((fresh (a b ab c rest rest-partitions)
      (== partitions `((,a ,b ,c) . ,rest-partitions))
      (riffleo `(,a ,b ,c) rest nums)
      (+o a b ab)
      (+o ab c sum)
      (three-partition-helpero rest rest-partitions sum)))))
      

(define partition-example
  '(20 23 25 30 49 45 27 30 30 40 22 19))


#|
;; First draft -- line up recursive conditionals
(defrel (chooseo k picked rest l)
  (conde
    ;; 
    ((== k 0) (== picked '()) (== rest l))
    
    ((fresh (n-1 l-car l-cdr)
      (== l `(,l-car . ,l-cdr))
      (succo k-1 k)
      (conde
        ((fresh (rest-cdr)
           (== rest `(,l-car . ,rest-cdr))
           (chooseo k picked rest-cdr l-cdr)))

        ((fresh (picked-cdr)
           (== picked `(,l-car . ,picked-cdr))
           (chooseo k-1 picked-cdr rest l-cdr))))))))
|#     
 
;; Second draft -- line up recursive conditionals
(defrel (chooseo k picked rest l)
  (conde
    ;; 
    ((== k 0) (== picked '()) (== rest l))
    
    ((fresh (k-1 l-car l-cdr)
      (== l `(,l-car . ,l-cdr))
      (succo k-1 k)
      (fresh (x0 x1 x2)
        (conde
          ((fresh (rest-cdr)
             (== rest `(,l-car . ,rest-cdr))
             (== x0 k) (== x1 picked) (== x2 rest-cdr)))

          ((fresh (picked-cdr)
             (== picked `(,l-car . ,picked-cdr))
             (== x0 k-1) (== x1 picked-cdr) (== x2 rest))))
           
        (chooseo x0 x1 x2 l-cdr))))))

(defrel (picko elem rest l)
  (riffleo `(,elem) rest l))

(defrel (vertex-covero vertices edges cover)
  (fresh (k rest-vertices)
    (chooseo k cover rest-vertices vertices)
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

(defrel (cuto l r out)
  (conde
    ((== l '()) (== r out))
    ((fresh (l-x out-x)
       (== l `(o . ,l-x))
       (== out `(o . ,out-x))
       (cuto l-x r out-x)))))

(defrel (cutso cuts full)
  (conde
    ((== cuts '()) (== full '()))
    ((fresh (cut-len cuts-cdr full-cdr x)
       (== cuts `((o . ,cut-len) . ,cuts-cdr))
       (== full `(o . ,full-cdr))
       (cuto cut-len x full-cdr)
       (cutso cuts-cdr x)))))

(defrel (map-cutso cuts fulls)
  (conde
    ((== cuts '()) (== fulls '()))
    ((fresh (cuts-car cuts-cdr fulls-car fulls-cdr)
       (== cuts `(,cuts-car . ,cuts-cdr))
       (== fulls `(,fulls-car . ,fulls-cdr))
       (cutso cuts-car fulls-car)
       (map-cutso cuts-cdr fulls-cdr)))))

(defrel (all-subcontainso elems l)
  (conde
    ((== elems '()))
    ((fresh (elems-car elems-cdr my-list my-elem rest-lists rest-elems)
       (== elems `(,my-elem . ,elems-cdr))
       (picko my-list rest-lists l)
       (picko my-elem rest-elems my-list)
       
       (all-subcontainso elems-cdr `(,rest-elems . ,rest-lists))))))

(defrel (pipe-cutso have need cuts)
  (map-cutso cuts have)
  (all-subcontainso need cuts))

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










(defrel (willableo full r x-l x0 x1 x-r y-l y0 y1 y-r)
  (fresh (fst snd)
    (== full `(conde ,fst ,snd))
    (appendo x-l `( (,r ,x0 ,x1) . ,x-r) fst)
    (appendo y-l `( (,r ,y0 ,y1) . ,y-r) snd)))



(defrel (willedo full r x-l x0 x1 x-r y-l y0 y1 y-r)
  (fresh (z0 z1 fst snd)
    (== full `(fresh (,z0 ,z1)
      (conde ,fst ,snd)
      (,r ,z0 ,z1)))
    
    (appendo x-l `((== ,z0 ,x0) (== ,z1 ,x1) . ,x-r) fst)
    (appendo y-l `((== ,z0 ,y0) (== ,z1 ,y1) . ,y-r) snd)))

(defrel (willslaw2o in out)
  (fresh (r x-l x0 x1 x-r y-l y0 y1 y-r)
    (willableo in r x-l x0 x1 x-r y-l y0 y1 y-r)
    (willedo out r x-l x0 x1 x-r y-l y0 y1 y-r)))


(defrel (willslaw3o in out)
  (fresh (in-fst in-snd out-fst out-snd r x0 x1 x2 y0 y1 y2 z0 z1 z2 x-l x-r y-l y-r)
    (== in `(conde ,in-fst ,in-snd))
    (== out `(fresh (,z0 ,z1 ,z2) (conde ,out-fst ,out-snd) (,r ,z0 ,z1 ,z2)))
    
    (appendo x-l `((,r ,x0 ,x1 ,x2) . ,x-r) in-fst)
    (appendo y-l `((,r ,y0 ,y1 ,y2) . ,y-r) in-snd)
    
    (appendo x-l `((== ,z0 ,x0) (== ,z1 ,x1) (== ,z2 ,x2) . ,x-r) out-fst)
    (appendo y-l `((== ,z0 ,y0) (== ,z1 ,y1) (== ,z2 ,y2) . ,y-r) out-snd)))
    
    

(defrel (willslawo in out)
  (fresh (in-fst in-snd out-fst out-snd r x y z z-fst z-snd x-l x-r y-l y-r)
    (== in `(conde ,in-fst ,in-snd))
    (== out `(fresh ,z (conde ,out-fst ,out-snd) (,r . ,z)))
    
    (appendo x-l `((,r . ,x) . ,x-r) in-fst)
    (appendo y-l `((,r . ,y) . ,y-r) in-snd)
    
    (will-helpero x y z z-fst z-snd)

    (fresh (x-full y-full)
      (appendo z-fst x-r x-full)
      (appendo z-snd y-r y-full)
      (appendo x-l x-full out-fst)
      (appendo y-l y-full out-snd))))


(defrel (will-helpero x y z fst snd)
  (conde
    ((== x '()) (== y '()) (== z '()) (== fst '()) (== snd '()))
    
    ((fresh (car-x cdr-x car-y cdr-y car-z cdr-z cdr-fst cdr-snd)
      (== x `(,car-x . ,cdr-x))
      (== y `(,car-y . ,cdr-y))
      (== z `(,car-z . ,cdr-z))
      (== fst `((== ,car-z ,car-x) . ,cdr-fst))
      (== snd `((== ,car-z ,car-y) . ,cdr-snd))

      (will-helpero cdr-x cdr-y cdr-z cdr-fst cdr-snd)))))


(defrel (containso haystack needle)
  (fresh (first rest)
    (== haystack `(,first . ,rest))
    (conde
      ((== needle first))
      ((containso rest needle)))))
        
    
;; A selection from a menu
;; whose sum weights are lesser than capacity
;; and whose sum values equals wealth
(defrel (knapsacko menu selection capacity wealth)
  (conde
    ((== selection '()) (== wealth 0))
    
    ((fresh (name weight value rest-selection rest-capacity rest-wealth)
        (== selection `((,name ,weight ,value) . ,rest-selection))

        (+o weight rest-capacity capacity)
        (+o value rest-wealth wealth)

        (containso menu `(,name ,weight ,value))
        (knapsacko menu rest-selection rest-capacity rest-wealth)))))

        
;; A selection from a menu
;; whose sum weights are lesser than capacity
;; and whose sum values equals wealth
(defrel (knapsack-2o menu selection capacity wanted-wealth)
  (fresh (actual-wealth)
    (>=o actual-wealth wanted-wealth)
    (knapsacko menu selection capacity actual-wealth)))


(define mcdonalds-menu '(    
    (french-fries 23 92)
    (big-mac 31 57)
    (soda 29 49)
    (apple-pie 44 68)
    (happy-meal 53 60)
    (salad 38 43)
    (filet-o-fish 63 67)
    (mcrib 85 84)
    (chicken-nuggets 89 87)
    (mcflurry 82 87)))



(defrel (riffleo l r o)
    (fresh (l1 l2 l3 r1 r2 r3 o1 o2)
        (conde
            ((== `(,l ,r ,o) `(         ()          ()          ())))
            ((== `(,l ,r ,o) `((,l1 . ,l2)          () (,l1 . ,l2))))
            ((== `(,l ,r ,o) `(         () (,r1 . ,r2) (,r1 . ,r2))))
            ((== `(,l ,r ,o) `((,l1 . ,l2) (,r1 . ,r2) (,o1 . ,o2)))
                (conde
                    ((== `(,o1 ,l3 ,r3) `(,l1 ,l2 ,r)))
                    ((== `(,o1 ,l3 ,r3) `(,r1 ,l ,r2))))
                (riffleo l3 r3 o2)))))


(defrel (evalo x)
    (fresh (x0 x1 x2 x3 x4 x5 x6)
        (conde
            ((== x `(x0 = x1)) (== x0 x1))
            ((== x `(,x0 + ,x1 = ,x2)) (+o x0 x1 x2))
            ((== x `(,x0 = ,x1 + 1)) (succo x0 x1))
            ((== x `(,x0 < ,x1)) (<o x0 x1))
            ((== x `(,x0 > ,x1)) (>o x0 x1))
            ((== x `(,x0 >= ,x1)) (>=o x0 x1)))))

(defrel (evalo* x)
    (conde
        ((== x '()))
        ((== x `(,first . ,rest))
            (evalo first)
            (evalo* rest))))

            
(defrel (reverse-and-appendo l r out)
    (conde
        ((== l '()) (== r out))
        ((fresh (car-l cdr-l)
            (== l `(,car-l . ,cdr-l))
            (reverse-and-appendo cdr-l `(,car-l . ,r) out)))))

;;(defrel (reverseo abc cba) (reverse-and-appendo abc '() cba))

(define (reverseo p q)
    (lambda (s)
        (lambda ()
            (let ((p (walk p s)) (q (walk q s)))
                ((cond
                    ((and (list? p) (list? q)) (== p (reverse q)))
                    ((and (list? p) (var? q)) (== (reverse p) q))
                    ((and (var? p) (list? q)) (== p (reverse q)))
                    ((and (var? p) (var? q)) (reverse-and-appendo p '() q))
                    (else fail)) s))))) ;; Fails due to type mismatch

(defrel (converto n h)
    (conde
        ((== n '(0 0 0 0)) (== h '0))
        ((== n '(0 0 0 1)) (== h '1))
        ((== n '(0 0 1 0)) (== h '2))
        ((== n '(0 0 1 1)) (== h '3))
        ((== n '(0 1 0 0)) (== h '4))
        ((== n '(0 1 0 1)) (== h '5))
        ((== n '(0 1 1 0)) (== h '6))
        ((== n '(0 1 1 1)) (== h '7))
        ((== n '(1 0 0 0)) (== h '8))
        ((== n '(1 0 0 1)) (== h '9))
        ((== n '(1 0 1 0)) (== h 'A))
        ((== n '(1 0 1 1)) (== h 'B))
        ((== n '(1 1 0 0)) (== h 'C))
        ((== n '(1 1 0 1)) (== h 'D))
        ((== n '(1 1 1 0)) (== h 'E))
        ((== n '(1 1 1 1)) (== h 'F))))

(defrel (converto* ns hs)
    (conde
        ((== ns '()) (== hs '()))
        ((fresh (car-ns cdr-ns car-hs cdr-hs)
            (== ns `(,car-ns . ,cdr-ns))
            (== hs `(,car-hs . ,cdr-hs))
            (converto car-ns car-hs)
            (converto* cdr-ns cdr-hs)))))



(defrel (group-into-fourso n g)
    (fresh (n1 g1)
        (reverseo n n1)
        (reverseo g g1)
        (group-into-fours-helpero n1 g1)))

        
(defrel (group-into-fours-helpero n g)
    (fresh (b0 b1 b2 b3 rest g-rest)
        (conde
            ((== n '()) (== g '()))
            ((== n `(,b0)) (== g `((0 0 0 ,b0))))
            ((== n `(,b0 ,b1)) (== g `((0 0 ,b1 ,b0))))
            ((== n `(,b0 ,b1 ,b2)) (== g `((0 ,b2 ,b1 ,b0))))
            ((== n `(,b0 ,b1 ,b2 ,b3 . ,rest))
                (== g `((,b3 ,b2 ,b1 ,b0) . ,g-rest))
                (group-into-fours-helpero rest g-rest)))))


(defrel (binary-hexo binary hex)
    (fresh (fours)
        (group-into-fourso binary fours)
        (converto* fours hex)))
        

;; (defrel (convert2o 
        
(define (time-suppress-output x) (time (and #t x)))
        
(defrel (binary-hex2o binary hex)
    (fresh (b h)
        (reverseo binary b)
        (reverseo hex h)
        (convert2o b h)))
        
(defrel (convert2o b h)
    (conde
        ((== b '()) (== h '()))
        ;; ((== b '(0)) (== h '(0))) ;; Disallow ending in 0
        ((== b '(1)) (== h '(1)))
        ;; ((== b '(0 0)) (== h '(0)))
        ;; ((== b '(1 0)) (== h '(1)))
        ((== b '(0 1)) (== h '(2)))
        ((== b '(1 1)) (== h '(3)))
        ;; ((== b '(0 0 0)) (== h '(0)))
        ;; ((== b '(1 0 0)) (== h '(1)))
        ;; ((== b '(0 1 0)) (== h '(2)))
        ;; ((== b '(1 1 0)) (== h '(3)))
        ((== b '(0 0 1)) (== h '(4)))
        ((== b '(1 0 1)) (== h '(5)))
        ((== b '(0 1 1)) (== h '(6)))
        ((== b '(1 1 1)) (== h '(7)))
        
        ((fresh (x y z w rest o out-rest)
            (== b `(,x ,y ,z ,w . ,rest))
            (== h `(,o . ,out-rest))

            (conde
                ((== `(,x ,y ,z ,w) '(0 0 0 0)) (== o '0))
                ((== `(,x ,y ,z ,w) '(1 0 0 0)) (== o '1))
                ((== `(,x ,y ,z ,w) '(0 1 0 0)) (== o '2))
                ((== `(,x ,y ,z ,w) '(1 1 0 0)) (== o '3))
                ((== `(,x ,y ,z ,w) '(0 0 1 0)) (== o '4))
                ((== `(,x ,y ,z ,w) '(1 0 1 0)) (== o '5))
                ((== `(,x ,y ,z ,w) '(0 1 1 0)) (== o '6))
                ((== `(,x ,y ,z ,w) '(1 1 1 0)) (== o '7))
                ((== `(,x ,y ,z ,w) '(0 0 0 1)) (== o '8))
                ((== `(,x ,y ,z ,w) '(1 0 0 1)) (== o '9))
                ((== `(,x ,y ,z ,w) '(0 1 0 1)) (== o 'A))
                ((== `(,x ,y ,z ,w) '(1 1 0 1)) (== o 'B))
                ((== `(,x ,y ,z ,w) '(0 0 1 1)) (== o 'C))
                ((== `(,x ,y ,z ,w) '(1 0 1 1)) (== o 'D))
                ((== `(,x ,y ,z ,w) '(0 1 1 1)) (== o 'E))
                ((== `(,x ,y ,z ,w) '(1 1 1 1)) (== o 'F)))
            
            (convert2o rest out-rest)))))
            
            
            
            
            
            
            
            
            
            
(defrel (needs-3-colorso graph subgraph proof)
    (fresh (order o1 i1 o1i1 subproof1 i2 o2 o2i2 subproof2)
        (conde
            ((odd-cycleo graph subgraph order)
                (== proof `(odd-cycle ,order)))

            ((xio graph subgraph o1 i1 i2 o2)
                (== proof `(xi ,o1 ,i1 ,i2 ,o2 ,subproof1 ,subproof2))
                (disjoint-uniono o1 i1 o1i1)
                (disjoint-uniono o2 i2 o2i2)
                (needs-3-colorso graph o1i1 subproof1)
                (needs-3-colorso graph o2i2 subproof2)))))
                
                
(defrel (odd-cycleo graph subgraph order) (fail))


;; A graph is a lexicographically ordered edgelist
(define my-graph `(
    (0 . 4)
    (0 . 6)
    (0 . 7)
    (1 . 3)
    (1 . 4)
    (1 . 5)
    (2 . 5)
    (2 . 7)
    (3 . 5)
    (5 . 7)))

(defrel (has-triangleo graph u v w)
    (fresh (rest)
        (riffleo `((,u . ,v) (,u . ,w) (,v . ,w)) rest graph)))


#|
(defrel (partition-edgeso pivot x lo hi)
    (fresh ()
        (conde
            ((== l '()) (== lo '()) (== hi '()))
            
            ((== l `(,first . ,rest))
                (conde
                    ((edge-<o first pivot) )
                    ((edge-<o pivot first) ))
                    
                (partition-edgeso pivot rest lo-rec hi-rec)))))
        
(defrel (sort-edgeso x o)
    (fresh (pivot rest lower higher)
        (conde
            ((== x '()) (== o '()))
            ((== x `(,pivot . ,rest))
                (partition-edgeso pivot rest lower higher)
                (appendo l `(,pivot . ,h) o)
                (sort-edgeso lower l)
                (sort-edgeso higher h)))))
|#


(defrel (edge-<o l r)
    (fresh (ul vl ur vr)
        (== l `(,ul . ,vl))
        (== r `(,ur . ,vr))
        
        ;; these two may be redundant
        (<o ul vl)
        (<o ur vr)

        (conde
            ((== ul ur) (<o vl vr))
            ((<o ul ur)))))
            
(defrel (sort-3-edgeso l3 sorted)
    (fresh (e1 e2 e3 a b c bc)
        (== `(,e1 ,e2 ,e3) l3)
        (== `(,a ,b ,c) sorted)
        (riffleo `(,a) bc l)
        (riffleo `(,b) `(,c) bc)
        (edge-<o a b)
        (edge-<o b c)))

(defrel (edgeo unsorted sorted)
    (fresh (a b c d)
        (== `(,a . ,b) unsorted)
        (== `(,c . ,d) sorted)
        (conde
            ((== `(,a . ,b) `(,c . ,d)))
            ((== `(,a . ,b) `(,d . ,c))))
        (<o c d)))
    
(defrel (has-odd-patho graph odd-path)
    (fresh (u v w uv vw e1 e2 e1l e1r e2l e2r rest)
        (== odd-path `(,u ,v ,w))
        (edgeo `(,u . ,v) `(,e1l . ,e1r))
        (edgeo `(,v . ,w) `(,e2l . ,e2r))
        (picko e1 `(,e2) `((,e1l . ,e1r) (,e2l . ,e2r)))
        (riffleo `(,e1 ,e2) rest graph)))

(defrel (has-odd-path2o graph odd-path)
    (fresh (a b c e1 e2 rest-1 rest-2)
        (== odd-path `(,a ,b ,c))
        (edgeo `(,a . ,b) e1)
        (edgeo `(,b . ,c) e2)
        (picko e1 rest-1 graph)
        (picko e2 rest-2 rest-1)))


(defrel (partitiono pivot l lo hi)
    (fresh (first rest lo-rec hi-rec)
        (conde
            ((== l '()) (== lo '()) (== hi '()))
            ((== l `(,first . ,rest))
                (conde
                    ((<=o first pivot) (== lo `(,first . ,lo-rec)) (== hi hi-rec))
                    ((>o first pivot) (== hi `(,first . ,hi-rec)) (== lo lo-rec)))
                    
                (partitiono pivot rest lo-rec hi-rec)))))

        
(defrel (sorto unsorted sorted)
    (conde
        ((== unsorted '()) (== sorted '()))
        
        ((fresh (pivot rest lo hi l h)
            (== unsorted `(,pivot . ,rest))
            (appendo l `(,pivot . ,h) sorted)
            (partitiono pivot rest hi lo)
            (sorto lo l)
            (sorto hi h)))))

            
(defrel (sort-nondeto unsorted sorted)
    (fresh (x first second others rest)
        (conde
            ((== unsorted '()) (== sorted '()))
            ((== unsorted `(,x)) (== sorted `(,x)))
            
            ((== sorted `(,first ,second . ,rest))
                (picko first others unsorted)
                (<=o first second)
                (sort-nondeto others `(,second . ,rest))))))

        
(defrel (odd-listo l)
    (fresh (t a b c)
        (conde
            ((== l `(,t)))
            ((== l `(,a ,b . ,c)) (odd-listo c)))))
            
(defrel (even-listo r)
    (fresh (x y z)
        (conde
            ((== r '()))
            ((== r `(,x ,y . ,z)) (even-listo z)))))

;; Step 1: make the conjunction its own relation
(defrel (odd-and-even-list1o l r)
    (odd-listo l)
    (even-listo r))

    
;; Step 2: replace each body with its definition
(defrel (odd-and-even-list2o l r)
    (fresh (t a b c)
        (conde
            ((== l `(,t)))
            ((== l `(,a ,b . ,c)) (odd-listo c))))
    (fresh (x y z)
        (conde
            ((== r '()))
            ((== r `(,x ,y . ,z)) (even-listo z)))))
            
;; Step 3: Bubble up fresh (resolve any naming clashes. Luckily my names don't clash)
(defrel (odd-and-even-list3o l r)
    (fresh (t a b c x y z)
        (conde
            ((== l `(,t)))
            ((== l `(,a ,b . ,c)) (odd-listo c)))
        (conde
            ((== r '()))
            ((== r `(,x ,y . ,z)) (even-listo z)))))

;; Step 4: Logical FOIL: (A \/ B) /\ (C \/ D) --> (A /\ C) \/ (A /\ D) \/ (B /\ C) \/ (B /\ D)
(defrel (odd-and-even-list4o l r)
    (fresh (t a b c x y z)
        (conde
            ((== l `(,t)) (== r '()))
            ((== l `(,t)) (== r `(,x ,y . ,z)) (even-listo z))
            ((== l `(,a ,b . ,c)) (odd-listo c) (== r '()))
            ((== l `(,a ,b . ,c)) (odd-listo c)) (== r `(,x ,y . ,z)) (even-listo z))))
            
;; Step 5: Sort unifications and non-unifications
(defrel (odd-and-even-list5o l r)
    (fresh (t a b c x y z)
        (conde
            (
                (== l `(,t)) (== r '()))
            (
                (== l `(,t)) (== r `(,x ,y . ,z))
                (even-listo z))
            (
                (== l `(,a ,b . ,c)) (== r '())
                (odd-listo c))
            (
                (== l `(,a ,b . ,c)) (== r `(,x ,y . ,z))
                (odd-listo c)) (even-listo z))))
                
;; Step 6: Detect conjunctions, 
(defrel (odd-and-even-list6o l r)
    (fresh (t a b c x y z)
        (conde
            (
                (== l `(,t)) (== r '()))
            (
                (== l `(,t)) (== r `(,x ,y . ,z))
                (odd-and-even-list6o `(,t) z))
            (
                (== l `(,a ,b . ,c)) (== r '())
                (odd-and-even-list6o c '()))
            (
                (== l `(,a ,b . ,c)) (== r `(,x ,y . ,z))
                (odd-and-even-list6o c z)))))
                
;; Step 7: Will's law: (A /\ X) \/ (B /\ X) --> (A \/ B) /\ X
(defrel (odd-and-even-list7o l r)
    (fresh (t a b c x y z g1 g2)
        (conde
            (
                (== l `(,t)) (== r '()))
            ((conde
                (
                    (== l `(,t)) (== r `(,x ,y . ,z))
                    (== g1 `(,t)) (== g2 z))
                (
                    (== l `(,a ,b . ,c)) (== r '())
                    (== g1 c) (== g2 '()))
                (
                    (== l `(,a ,b . ,c)) (== r `(,x ,y . ,z))
                    (== g1 c) (== g2 z)))
                    
                (odd-and-even-list7o g1 g2)))))
                
                
(defrel (x-bar-baro x)
    (fresh (specifier x-bar1 x-bar2 head complement adjunct)
        (sepecifiero specifier)
        (x-baro x-bar1)
        (x-baro x-bar2)
        (xo head)
        (complemento complement)
        (adjuncto adjunct)
        (== x `(,specifier (,x-bar1 (,x-bar2 ,head ,complement) ,adjunct)))))
                

                
;; Relational red-black tree adapted from https://cs3110.github.io/textbook/chapters/ds/rb.html
(defrel (coloro x)
    (conde
        ((== x 'red))
        ((== x 'black))))
        
        
(defrel (red-black-treeo x)
    (conde
        ((== x 'leaf))
        ((fresh (c v l r)
            (== x `(node ,c ,v ,l ,r))
            (coloro c)
            (red-black-treeo l)
            (red-black-treeo r)))))

(defrel (emptyo x)
    (== x 'leaf))
    
;; direct implementation
(defrel (membero x t)
    (fresh (c l v r)
        (== t `(node ,c ,v ,l ,r))
        (conde
            ((<o x v) (membero x l))
            ((>o x v) (membero x r))
            ((== x v)))))

;; prioritize unification, extract recursive call to membero
(defrel (membero x t)
    (fresh (c v l r)
        (== t `(node ,c ,v ,l ,r))
        (conde
            ((== x v))
            ((fresh (a0)
                (conde
                    ((<o x v) (== a0 l))
                    ((>o x v) (== a0 r)))
                (membero x a0))))))


(defrel (rotateo t o)
    (fresh (x y z a b c d)
        (== o `(node red ,y (node black ,x ,a ,b) (node black ,z ,c ,d)))
        
        (conde
            ((== t
                `(node black ,z 
                    (node red ,y
                        (node red ,x
                            ,a
                            ,b)
                        ,c)
                    ,d)))

            ((== t
                `(node black ,z 
                    (node red ,x
                        ,a
                        (node red ,y
                            ,b
                            ,c))
                    ,d)))
        
            ((== t
                `(node black ,x
                    ,a
                    (node red ,y
                        ,b
                        (node red ,z
                            ,c
                            ,d)))))
                        
            ((== t
                `(node black ,x
                    ,a
                    (node red ,z
                        (node red ,y
                            ,b
                            ,c)
                        ,d)))))))


        
(defrel (insert-auxo x t o)
    (conde
        ((== t 'leaf) (== o `(node red ,x leaf leaf)))
        
        ((fresh (c l v r rec)
            (== t `(node ,c ,l ,v ,r))
            (conde
                ((<o x v) (balanceo  
                ((>o x v)
                ((== x v) (== o t))))))))))
        
        
        
        
        
(defrel (sumo l o)
    (fresh (car-l cadr-l cdr-l)
        (conde
            ((== l '()) (== o 0))
            ((== l `(,car-l)) (== o car-l)))))
        
        

(defrel (bretto list a b)
    (fresh (y z)
        (+o a b z)
        (riffleo `(,a ,b) y list)
        (primeo z)))
        

(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o z0 z1)
        (conde
            ;; When one of `a` or `b` is empty
            ((== a '()) (== b '()) (== o '()))
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are nonempty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o))
                (conde
                    ((== car-o car-a) (== z0 cdr-a) (== z1 b))
                    ((== car-o car-b) (== z0 a) (== z1 cdr-b)))
                    
                (riffleo z0 z1 cdr-o)))))
        

(define (build-num n)
  (cond
    ((zero? n) '())
    ((even? n)
     (cons 0
       (build-num (quotient n 2))))
    ((odd? n)
     (cons 1
       (build-num (quotient (- n 1) 2))))))

(define (unbuild-num n)
    (cond
        ((null? n) 0)
        (else (+ (car n) (* 2 (unbuild-num (cdr n)))))))
        
(define (deep-unbuild-num l)
    (cond
        ((or (null? l) (eq? (car l) 0) (eq? (car l) 1))
            (unbuild-num l))
        (else
            (map deep-unbuild-num l))))


(defrel (riffleo l r o)
  (fresh (car-l cdr-l car-r cdr-r car-o cdr-o)
    (conde
      ;; Base cases
      ((== l '()) (== r '()) (== o '()))
      ((== l `(,car-l . ,cdr-l)) (== r '()) (== o l))
      ((== l '()) (== r `(,car-r . ,cdr-r)) (== o r))

      ;; Recursive cases
      ((== l `(,car-l . ,cdr-l)) (== r `(,car-r . ,cdr-r)) (== o `(,car-l . ,cdr-o))
      	(riffleo cdr-l r cdr-o))
      ((== l `(,car-l . ,cdr-l)) (== r `(,car-r . ,cdr-r)) (== o `(,car-r . ,cdr-o))
      	(riffleo l cdr-r cdr-o)))))


;; After applying a correctness-preserving transformation

(defrel (riffleo l r o)
    (fresh (car-l cdr-l l3 car-r cdr-r r3 car-o cdr-o)
        (conde
            ((== `(,l ,r ,o) `(               ()                ()                ())))
            ((== `(,l ,r ,o) `((,car-l . ,cdr-l)                () (,car-l . ,cdr-l))))
            ((== `(,l ,r ,o) `(               () (,car-r . ,cdr-r) (,car-r . ,cdr-r))))
            ((== `(,l ,r ,o) `((,car-l . ,cdr-l) (,car-r . ,cdr-r) (,car-o . ,cdr-o)))
                (conde
                    ((== `(,car-o ,l3 ,r3) `(,car-l ,l2 ,r)))
                    ((== `(,car-o ,l3 ,r3) `(,car-r ,l ,cdr-r))))
                (riffleo l3 r3 cdr-o)))))