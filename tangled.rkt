
#lang racket

(require "../faster-minikanren/main.rkt")
(require "../faster-minikanren/numbers.rkt")
;; ./README.md
;; ./6-Tricky-Relations/3-Equal-Popcount.md
(defrel (equal-popcounto-riffle l1 l2)
  (fresh (zeros ones)
    (==* 0 zeros)
    (==* 1 ones)
    (riffleo zeros ones l1)
    (riffleo zeros ones l2)))

(defrel (equal-popcount-helpero b0 b1 l1 l2 diff)
  (conde ((== l1 '()) (== l2 '()) (== diff '()))
         ((fresh (a1 a2 d1 d2 rec)
            (== l1 `(,a1 . ,d1))
            (== l2 `(,a2 . ,d2))
            
            (conde ((== a1 a2) (== rec diff))
                   ((== a1 b1) (== a2 b0) (+1o/int rec diff))
                   ((== a1 b0) (== a2 b1) (+1o/int diff rec)))
            (equal-popcount-helpero b0 b1 d1 d2 rec)))))

;; ./6-Tricky-Relations/4-HAMPATH.md
(defrel (か-with-countdown u s g n)
  (conde ((== n '()) (emptyo s))
         ((fresh (v rest n-1)
            (== n `(o . ,n-1))
            (conjo v #t rest s)
            (edgeo u v g)
            (か-with-countdown v rest g n-1)))))

(defrel (か-w/o-countdown u s g)
  (conde ((emptyo s))
         ((fresh (v rest)
            (conjo v #t rest s)
            (edgeo u v g)
            (か-w/o-countdown v rest g)))))

(defrel (conjo k v s s+1)
  (!elemo k s)
  (elemo k v s+1))

(defrel (edgeo u v g)
  (fresh (neighbors)
    (elemo u neighbors g)
    (elemo v #t neighbors)))

(defrel (emptyo m)
  (== m '()))

(defrel (has-hamiltonian-patho g)
  (fresh (starting-vertex neighbors)
    (elemo starting-vertex neighbors g)
     (か-w/o-countdown starting-vertex neighbors g)))

(defrel (hampatho g)
  (conde ((emptyo g))
         ((fresh (v neighbors)
            (conjo v neighbors g)
            (hampatho neighbors)))))

;; ./6-Tricky-Relations/2-Majority.md
(defrel (majorityo/diseq x l)
  (fresh (difference)
    (poso/int difference)
    (count-differenceo/diseq x l difference)))

(defrel (count-differenceo/diseq x l c)
  (conde ((== l '()) (== c '(nat ())))
         ((fresh (a d rec)
            (== l `(,a . ,d))
            (conde ((==  a x) (+1o/int rec c))
                   ((=/= a x) (+1o/int c rec)))
            (count-differenceo/diseq x d rec)))))

(defrel (majorityo/monotonic x l)
  (fresh (difference p)
    (conde ((== difference '(nat (s))))
           ((== difference '(nat (s s)))))
    (count-differenceo/trs2e x l difference)))

(defrel (count-differenceo/trs2e x l c)
  (conde ((== l '()) (== c '(nat ())))
         ((fresh (a d rec)
            (== l `(,a . ,d))
            (conde ((==  a x) (+1o/int rec c))
                   ((+1o/int c rec)))
            (count-differenceo/trs2e x d rec)))))

(defrel (majorityo/project x l)
  (listo l)
  (project (x l)
    (if (> (length (filter (lambda (e)      (equal? e x))  l))
           (length (filter (lambda (e) (not (equal? e x))) l)))
        (== #t #t)
        (majorityo/monotonic x l))))

;; ./6-Tricky-Relations/1-Length.md
(defrel (lengtho/trs2e l n)
  (conde ((== l '()) (== n '()))
         ((fresh (a d n-1)
            (== l `(,a . ,d))
            (+1o n-1 n)
            (lengtho/trs2e d n-1)))))

(defrel (lengtho/project l n)
  (listo l)
  (project (l) (== n (build-num (length l)))))

;; ./liarKanren/README.md
;; ./1-Arithmetic/0-Intro.md
(defrel (groundo/peano n)
  (conde ((== n '()))
         ((fresh (n-1)
            (== n `(s . ,n-1))
            (groundo/peano n-1)))))

(defrel (poso/peano n)
  (fresh (n-1)
    (== n `(s . ,n-1))))

(defrel (+1o/peano n n+1)
  (== n+1 `(s . ,n)))

(defrel (*2o/peano n 2n)
  (conde ((== n '()) (== 2n '()))
         ((fresh (n-1 2n-2)
            (== n `(s . ,n-1))
            (== 2n `(s s . ,2n-2))
            (*2o/peano n-1 2n-2)))))

(defrel (+1o n n+1)
  (conde ((== n '()) (== n+1 '(1)))
         ((fresh (a d)
            (== n `(,a . ,d))
            (conde ((== a 0) (poso d) (== n+1 `(1 . ,d)))
                   ((fresh (d+1)
                      (== a 1)
                      (== n+1 `(0 . ,d+1))
                      (+1o d d+1))))))))

(defrel (groundo/int x)
  (fresh (n)
    (conde ((== x `(nat ,n)))
           ((== n `(neg-succ ,n))))
    (groundo/peano n)))

(defrel (poso/int x)
  (fresh (n)
    (== x `(nat ,n))
    (poso/peano n)))

(defrel (+1o/int x x+1)
  (fresh (n n+1)
    (conde ((== x `(nat ,n)) (== x+1 `(nat ,n+1)) (+1o/peano n n+1))
           ((== x '(neg-succ ())) (== x+1 '(nat ())))
           ((== x `(neg-succ (s . ,n))) (== x+1 `(neg-succ ,n))))))

;; ./1-Arithmetic/4-Prime-Factorization.md
(defrel (prime-factorso/1 n l)
  (ordered-listo <=o l)
  (all-primeo l)
  (producto l n))

(defrel (prime-factorso/2 n l)
  (conde ((== l '()) (== n '(1)))
         ((fresh (a d a/n)
            (== l `(,a . ,d))
            (*o a a/n n)
            (primeo a)
            (conde ((== d '()) (== n a))
                   ((fresh (ad dd)
                      (== d `(,ad . ,dd))
                      (<=o a ad)
                      (prime-factorso/2 a/n d))))))))

(defrel (primeo n)
  (olego n)
  (project (n)
    (== #t (prime? (unbuild-num n)))))

(define (unbuild-num n)
  (cond ((null? n) 0)
        (else (+ (car n) (* 2 (unbuild-num (cdr n)))))))
        
(define (prime? n)
  (let loop ((i 2))
    (cond ((<= n 1) #f)
          ((= i n) #t)
          ((= (modulo n i) 0) #f)
          (else (loop (+ i 1))))))

(defrel (producto l n)
  (conde ((== l '()) (== n '(1)))
         ((fresh (a d n/a)
            (== l `(,a . ,d))
            (*o a n/a n)
            (producto d n/a)))))
            
(defrel (all-primeo l)
  (conde ((== l '()))
         ((fresh (a d)
            (== l `(,a . ,d))
            (primeo a)
            (all-primeo d)))))

(defrel (pairingo/cantor/1 i j k)
  (fresh (i+j i+j+1 <i+j>*<i+j+1>)
    (pluso i j i+j)
    (+1o i+j i+j+1)
    (pluso `(0 . ,j) <i+j>*<i+j+1> `(0 . ,k))
    (*o i+j i+j+1 <i+j>*<i+j+1>)))

(defrel (pairingo/cantor/2 i j k)
  (fresh (i+j T_i+j)
    (pluso i j i+j)
    (triangleo i+j T_i+j)
    (pluso T_i+j j k)))

;; ./1-Arithmetic/3-Fresh-Multiples-of-Three.md
(defrel (fresh-multiple-of-threeo n)
  (fresh (labeled e o prefix)
    (=/= e o)
    (ends-in-1o n)
    (label-odds-and-evenso n labeled)
    (algo e o labeled)))

(defrel (another-multiple-of-threeo +3o n)
  (conde ((== n '()))
         ((fresh (n-3)
            (+3o n-3 n)
            (another-multiple-of-threeo +3o n-3)))))
            
(defrel (+3o-naive1 n n+3)
  (pluso '(1 1) n n+3))
  
(defrel (+3o-naive2 n n+3)
  (pluso n '(1 1) n+3))

(defrel (+3o-special n n+3)
  (fresh (n+1 n+2)
    (succo n n+1)
    (succo n+1 n+2)
    (succo n+2 n+3)))

(defrel (succo n n+1)
  (conde ((== n '()) (== n+1 '(1)))
         ((fresh (a d d+1)
            (== n `(,a . ,d))
            (conde ((== a 0) (pairo d) (== n+1 `(1 . ,d)))
                   ((== a 1) (== n+1 `(0 . ,d+1)) (succo d d+1)))))))
                   
                   
(defrel (add-bito b n n+b)
  (conde ((== n '()) (== n+b `(,b)))
         ((fresh (a d a+b d+b carry)
            (== n `(,a . ,d))
            (== n+b `(,a+b . ,d+b))
            
            (conde ((== a b) (== a+b 0))
                   ((=/= a b) (== a+b 1)))
            (conde ((=/= `(,a ,b) '(1 1)) (== d+b d))
                   ((== `(,a ,b) '(1 1)) (== carry 1) (add-bito carry d d+b)))))))

(defrel (==* x l)
  (conde ((== l '()))
         ((fresh (d)
            (== l `(,x . ,d))
            (==* x d)))))

(defrel (ends-in-1o n)
  (conde ((== n '()))
         ((fresh (prefix)
            (appendo prefix '(1) n)))))

(defrel (label-odds-and-evenso l labeled)
  (conde ((== l '()) (== labeled '()))
         ((fresh (a) (== l `(,a)) (== labeled `((,a even)))))
         ((fresh (a1 a2 d rec)
            (== l `(,a1 ,a2 . ,d))
            (== labeled `((,a1 even) (,a2 odd) . ,rec))
            (label-odds-and-evenso d rec)))))

(defrel (algo e o n)
  (conde ((== n '()))
         ((fresh (x) (== n `((0 ,x)))))
         ((fresh (rec)
            (conde ((fresh (x n-x n-xx)
                      (riffleo `((,x even)) n-x n)
                      (riffleo `((,x odd)) rec n-x)))
                   ((fresh (n-eee)
                      (riffleo `((,e even) (,e even) (,e even)) n-eee n)
                      (riffleo `((,o odd) (,o odd) (,o odd)) rec n-eee))))
            (algo e o rec)))))

(define (h n)
  (length (run* (q)
    (lengtho/trs2e q (build-num n))
    (fresh-multiple-of-threeo q))))

(defrel (algo2 e o n)
  (conde ((== n '()))
         ((fresh (x) (== n `((0 ,x)))))
         ((fresh (x rec a0 a1 a2 a3 a4 a5)
            (conde ((== `(,a0 ,a1 ,a2 ,a3 ,a4 ,a5) `((s)     (,x even) (s)     (,x odd) n rec)))
                   ((== `(,a0 ,a1 ,a2 ,a3 ,a4 ,a5) `((s s s) (,e even) (s s s) (,o odd) n rec))))
            (removeNMo a0 a1 a2 a3 a4 a5)
            (algo2 e o rec)))))

(defrel (removeNMo n x m y l o)
  (conde ((== n '()) (== m '()) (== l o))
         ((fresh (a d rec n-1 m-1)
            (== l `(,a . ,d))
            (conde ((== n `(s . ,n-1))) ((== m `(s . ,m-1))))
            (conde ((fresh (n-1) (== a x) (== o rec) (== n `(s . ,n-1)) (removeNMo n-1 x m y d rec)))
                   ((fresh (m-1) (== a y) (== o rec) (== m `(s . ,m-1)) (removeNMo n x m-1 y d rec)))
                   ((=/= a x) (=/= a y) (== o `(,a . ,rec)) (removeNMo n x m y d rec)))))))

;; ./1-Arithmetic/1-Inequalities.md
(defrel (<o/peano n m)
  (fresh (n-1 m-1)
    (== m `(s . ,m-1))
    (conde ((== n '()))
           ((== n `(s . ,n-1))
            (<o/peano n-1 m-1)))))

;; ./1-Arithmetic/5-GCD.md
(defrel (gcdo1 s n m gcd)
  (conde ((== m '()) (== gcd n))
         ((fresh (m-1 sub)
            (poso m)
            (+o s sub m n)
            (gcdo1 s m sub gcd)))))

;; ./1-Arithmetic/6-Triangle-Numbers.md
(defrel (triangleo n Tn)
  (fresh (n+1)
    (+1o n n+1)
    (*o n n+1 `(0 . ,Tn))))

;; ./1-Arithmetic/7-Modular-Arithmetic.md
(defrel (congruento/* a b k)
  (fresh (product)
    (*o a k product)
    (*o b k product)))

;; ./1-Arithmetic/2-Divisibility-by-Three.md
(defrel (interleaved-hasho n)
  (conde ((== n '()))
         ((== n '(1)))
         ((fresh (b1 b2 rest)
            (== n `(,b1 $ ,b2 . ,rest))
            (conde ((== b1 0))
                   ((== b1 1)))
            (interleaved-hasho `(,b2 . ,rest))))))

;; ./0-Types/0-Intro.md
;; ./0-Types/3-Natmaps.md
;; ./0-Types/4-Natsets.md
(defrel (elemo n s)
  (fresh (l m r)
    (== s `(,l ,m ,r))
    (conde ((== n '()) (== m #t))
           ((fresh (a d rec)
              (== n `(,a . ,d))
              (conde ((== a 0) (poso d) (== rec l))
                     ((== a 1) (== rec r)))
              (elemo d rec))))))

;; Adapted from code by Raffi Sanna
(defrel (!elemo n s)
  (fresh (l r)
    (conde ((== s '()))
           ((== n '()) (== s `(,l #f ,r)))
           ((fresh (val b rec)
              (== s `(,val ,l ,r))
              (conde ((== n `(0 . ,b)) (poso b) (== rec l))
                     ((== n `(1 . ,b)) (== rec r)))
              (!elemo b rec))))))

;; ./0-Types/2-Higher-Order-Types.md
(defrel (ordered-listo ordo l)
  (conde ((== l '()))
         ((fresh (a d)
            (== l `(,a . ,d))
            (conde ((== d '()))
                   ((fresh (ad dd)
                      (== d `(,ad . ,dd))
                      (ordo a ad)
                      (ordered-listo  ordo d))))))))

;; ./0-Types/1-Basic-Types.md
(defrel (listo l)
  (conde ((== l '()))
         ((fresh (a d)
            (== l `(,a . ,d))
            (listo d)))))

(defrel (length-parityo l p)
  (conde ((== l '()) (== p 'even))
         ((fresh (a d rec)
            (== l `(,a . ,d))
            (conde ((== p 'odd)  (== rec 'even))
                   ((== p 'even) (== rec 'odd)))
            (length-parityo d rec)))))

(defrel (peanoo n)
  (conde ((== n '())
         ((fresh (n-1)
            (== n `(s . ,n-1))
            (peanoo n-1))))))

(defrel (olego n)
  (conde ((== n '()))
         ((fresh (a d)
            (== n `(,a . ,d))
            (conde ((== a 0) (poso d))
                   ((== a 1)))
            (olego d)))))

;; ./9-Misc/1-A-Rule-of-Inference.md
;; ./9-Misc/3-Utility-Definitions.md
(defrel (pairo x)
  (fresh (a d)
    (== x `(,a . ,d))))

(defrel (appendo l r l++r)
  (conde ((== l '()) (== r l++r))
         ((fresh (a d d++r)
            (== l `(,a . ,d))
            (== l++r `(,a . ,d++r))
            (appendo d r d++r)))))

(defrel (>1o x)
  (fresh (a ad dd)
    (== x `(,a ,ad . ,dd))))

(defrel (membero x l)
  (fresh (a d)
    (== l `(,a . ,d))
    (conde ((== x a))
           ((membero x d)))))

(defrel (reverseo l lR)
  (reverse-and-appendo l '() lR))

(defrel (reverse-and-appendo l acc lR++acc)
  (conde ((== l '()) (== lR++acc acc))
         ((fresh (a d)
            (== l `(,a . ,d))
            (reverse-and-appendo d `(,a . ,acc) lR++acc)))))

(defrel (list-refo l n val)
  (fresh (a d)
    (== l `(,a . ,d))
    (conde ((== n '()) (== a val))
           ((fresh (n-1)
              (== n `(s . ,n-1))
              (list-refo d n-1 val))))))

(defrel (lengtho/peano l n)
  (conde ((== l '()) (== n '()))
         ((fresh (a d n-1)
            (== l `(,a . ,d))
            (== n `(s . ,n-1))
            (lengtho/peano d n-1)))))

(define (build-nat n)
  (if (zero? n) '() `(s . ,(build-nat (- n 1)))))

(defrel (nat-lengtho n l)
  (conde ((== n '()) (== l '()))
         ((fresh (n-1 a d)
            (== n `(s . ,n-1))
            (== l `(,a . ,d))
            (nat-lengtho n-1 d)))))

;; ./9-Misc/2-Rational-Numbers.md
;; ./7-Techniques/3-One-to-One-Relationships.md
(defrel (temperatureo f c)
  (fresh (f-32 9c)
    (pluso f-32 (build-num 32) f)
    (*o (build-num 9) c 9c)
    (*o (build-num 5) f-32 9c)))

(defrel (+-o n₁ n₂ n₁+n₂ n₁−n₂)
  (fresh (n₁−1 n₂−1 n₁−1+n₂−1)
    (conde ((== n₂ '())
            (== n₁+n₂ n₁)
            (== n₁−n₂ n₁))
            
           ((== n₂ `(s . ,n₂−1))
            (== n₁ `(s . ,n₁−1))
            (== n₁+n₂ `(s s . ,n₁−1+n₂−1))
            (+-o n₁−1 n₂−1 n₁−1+n₂−1 n₁−n₂)))))

(defrel (plus-minuso x y x+y x-y)
  (appendo x y x+y)
  (appendo x-y y x))

;; ./7-Techniques/1-Fresh-Tagging.md
(defrel (xoro x)
  (fresh (a b)
         (conde ((== x `(left ,a)))
                ((== x `(right ,b)))
                ((== x `(both ,a ,b))))))

(defrel (leftso l o)
  (conde ((== l '()) (== o '()))
         ((fresh (a d left _ rec)
            (== l `(,a . ,d))
            (conde ((== a `(left ,left)) (== o `(,left . ,rec)))
                   ((== a `(right ,_)) (== o rec))
                   ((== a `(both ,left ,_)) (== o `(,left . ,rec))))
            (leftso d rec)))))

;; ./7-Techniques/2-Polymorphism-in-miniKanren.md
(defrel (nato s n)
  (conde ((== s 'peano) (peanoo n))
         ((== s 'oleg)  (olego n))))

(defrel (leqo s n m)
  (conde ((== s 'peano) (== n m))
         ((== s 'peano) (lesso n m))
         ((== s 'oleg)  (<=o n m))))

(defrel (lesso n m)
  (fresh (n-1 m-1)
    (== m `(s . ,m-1))
    (conde ((== n '()))
           ((== n `(s . ,n-1))
            (lesso n-1 m-1)))))

(defrel (+o s n m n+m)
  (conde ((== s 'peano) (appendo n m n+m))

         ((== s 'oleg)  (pluso n m n+m))))

#;(defrel (multo s n m n*m)
  (conde ((== s 'peano)  (peano-*o n m n*m))

         ((== s 'oleg)   (*o n m n*m))))

(defrel (peano-*o n m n*m)
  (conde ((== n '()) (== n*m '()))
         ((pairo n) (== m '()) (== n*m '()))
         ((fresh (n-1 m-1 n*m-n n*m-m)
            (== n `(s . ,n-1))
            (== m `(s . ,m-1))
            (+o 'peano n n*m-n n*m)
            (peano-*o n-1 m n*m-m)))))

;; ./first-order-miniKanren/existential-disjunctive-normal-form/README.md
;; ./first-order-miniKanren/s-expression-functions/README.md
;; ./first-order-miniKanren/misc/README.md
;; ./first-order-miniKanren/defrel-and-run/README.md
;; ./first-order-miniKanren/mk-expression-functions/README.md
;; ./2-Combinatorics/1-Riffle.md
(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o z0 z1)
        (conde
            ;; If `a` and `b` are both empty, then the output is empty.
            ((== a '()) (== b '()) (== o '()))
            
            ;; If `a` is non-empty and `b` is empty, then the output is equal to `a`.
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            
            ;; If `a` is empty and `b` is non-empty, then the output is equal to `b`.
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are non-empty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o))
                (conde
                    ((== car-o car-a) (== z0 cdr-a) (== z1 b))
                    ((== car-o car-b) (== z0 a) (== z1 cdr-b)))
                    
                (riffleo z0 z1 cdr-o)))))


(defrel (three-partitiono l partitions sum)
    (fresh (e0 e1 e2 rest-l e0+e1 rest-partitions)
        (conde
            ;; Base case
            ((== l '())
                (== partitions '()))

            ;; Recursive case
            ((== partitions `((,e0 ,e1 ,e2) . ,rest-partitions))
                (riffleo `(,e0 ,e1 ,e2) rest-l l)
                (pluso e0 e1 e0+e1)
                (pluso e0+e1 e2 sum)
                (three-partitiono rest-l rest-partitions sum)))))

                
(define example (map build-num
    '(20 23 25 30 49 45 27 30 30 40 22 19)))
    
(define (deep-unbuild-num l)
    (cond
        ((or (null? l) (eq? (car l) 0) (eq? (car l) 1))
            (unbuild-num l))
        (else
            (map deep-unbuild-num l))))

;; ./2-Combinatorics/2-Cartesian-Product.md
(defrel (cartesian-producto/v1 l₁ l₂ l₁×l₂)
  (conde ((== l₁ '()) (== l₁×l₂ '()))
         ((fresh (a₁ d₁ d₁×l₂ fusion)
            (== l₁ `(,a₁ . ,d₁))
            (fuseo a₁ l₂ fusion)
            (appendo fusion d₁×l₂ l₁×l₂)
            (cartesian-producto/v1 d₁ l₂ d₁×l₂)))))

(defrel (fuseo sym l o)
  (conde ((== l '()) (== o '()))
         ((fresh (a d rec)
            (== l `(,a . ,d))
            (== o `((,sym ,a) . ,rec))
            (fuseo sym d rec)))))

(defrel (cartesian-producto/v2 l₁ l₂ l₁×l₂)
  (conde ((== l₁ '()) (== l₁×l₂ '()))
         ((== l₂ '()) (== l₁×l₂ '()))
         ((fresh (a₁ d₁ a₂ d₂ d₁×l₂ fusion)
            (== l₁ `(,a₁ . ,d₁))
            (== l₂ `(,a₂ . ,d₂))
            (fuseo a₁ l₂ fusion)
            (appendo fusion d₁×l₂ l₁×l₂)
            (cartesian-producto/v2 d₁ l₂ d₁×l₂)))))

(defrel (cartesian-producto l₁ l₂ l₁×l₂)
  (conde ((== l₁ '()) (== l₁×l₂ '()))
         ((== l₂ '()) (== l₁×l₂ '()))
         ((fresh (a₁ d₁ a₂ d₂ d₁×l₂ fusion)
            (== l₁ `(,a₁ . ,d₁))
            (== l₂ `(,a₂ . ,d₂))
            (fuse-and-appendo a₁ l₂ d₁×l₂ l₁×l₂)
            (cartesian-producto d₁ l₂ d₁×l₂)))))

(defrel (fuse-and-appendo sym l r o)
  (conde ((== l '()) (== r o))
         ((fresh (a d rec)
                 (== l `(,a . ,d))
                 (== o `((,sym ,a) . ,rec))
                 (fuse-and-appendo sym d r rec)))))

;; ./2-Combinatorics/4-Multiset-Venn-Diagrams.md
(defrel (venn-diagramo x y l c r)
  (conde ((== l x) (== c '()) (== r y)
          (conde ((== x '())) ((== y '()))))
          
         ((fresh (ax dx ay dy)
            (== x `(,ax . ,dx))
            (== y `(,ay . ,dy))))))

;; ./2-Combinatorics/3-Associative-Cartesian-Product.md
;; ./5-Puzzles-and-Games/1-The-Zebra-Puzzle.md
;; ./3-Theory-of-Computation/4-3COLOR.md
(defrel (3color₁ᵒ g m)
  (conde ((== g '()))
         ((fresh (a d u v cᵤ cᵥ)
            (== g `(,a . ,d))
            (== a `(,u ,v))
            (different-colorsᵒ cᵤ cᵥ)
            (list-refo m u cᵤ)
            (list-refo m v cᵥ)
            (conde ((== d `()))
                   ((fresh (ad dd)
                      (== d `(,ad . ,dd))
                      (<ₚᵒ a ad)
                      (3color₁ᵒ d m))))))))

(defrel (different-colorsᵒ c₁ c₂)
  (conde ((== c₁ 'red)   (== c₂ 'blue))
         ((== c₁ 'red)   (== c₂ 'green))
         ((== c₁ 'blue)  (== c₂ 'red))
         ((== c₁ 'blue)  (== c₂ 'green))
         ((== c₁ 'green) (== c₂ 'red))
         ((== c₁ 'green) (== c₂ 'blue))))

(defrel (<ₙᵒ n₁ n₂)
  (fresh (n₂-1)
    (== n₂ `(s . ,n₂-1))
    (conde ((== n₁ '()))
           ((fresh (n₁-1)
              (== n₁ `(s . ,n₁-1))
              (<ₙᵒ n₁-1 n₂-1))))))

(defrel (<ₚᵒ p₁ p₂)
  (fresh (l₁ r₁ l₂ r₂ α₁ α₂)
    (== p₁ `(,l₁ ,r₁))
    (== p₂ `(,l₂ ,r₂))
    
    (conde ((== `(,α₁ ,α₂) `(,l₁ ,l₂)))
           ((== l₁ l₂) (== `(,α₁ ,α₂) `(,r₁ ,r₂))))

    (<ₙᵒ α₁ α₂)))

;; ./3-Theory-of-Computation/0-Intro.md
;; ./3-Theory-of-Computation/5-UNSAT.md
;; ./3-Theory-of-Computation/2-Context-Free-Grammars.md
(defrel (balanced-brackets-grammaro g)
  (== g '((B1 -> B B)
          (B1 -> O M)
          (B1 -> O C)
          (B -> B B)
          (B -> O M)
          (B -> O C)
          (M -> B C)
          (O -> <)
          (C -> >))))

(defrel (recognizeo g A sA)
  (fresh (rule B C a sB sC)
    (membero rule g)
    (conde ((== rule `(,A -> ,a))
            (== sA `(,a)))
           ((== rule `(,A -> ,B ,C))
            (appendo sB sC sA)
            (recognizeo g B sB)
            (recognizeo g C sC)))))

;; ./3-Theory-of-Computation/1-Lexing.md
(defrel (segmento text lexicon words)
  (conde ((== text '()) (== words '()))
         ((fresh (first-word rest-words rest-text)
            (== words `(,first-word . ,rest-words))
            (pairo first-word) ; words should be nonempty
            (membero first-word lexicon)
            (appendo first-word rest-text text)
            (segmento rest-text lexicon rest-words)))))

;; ./3-Theory-of-Computation/3-3SAT.md
(defrel (3sato cnf assignments)
  (conde ((== cnf '()))
         ((fresh (c1 c2 c3 rest var val)
            (== cnf `((,c1 ,c2 ,c3) . ,rest))
            (conde ((== c1 `(,var ,val)))
                   ((== c2 `(,var ,val)))
                   ((== c3 `(,var ,val))))
            (lookupo assignments var val)
            (3sato rest assignments)))))

(defrel (lookupo map k v)
  (fresh (l x r a d next)
    (== map `(,x ,l ,r))
    (conde ((== k '()) (== x v))
           ((== k `(,a . ,d))
            (conde ((== a 0) (poso d) (== next l))
                   ((== a 1) (== next r)))
            (lookupo next d v)))))

;; ./misc/statically-typed-relations.md
;; ./misc/natset.md
;; ./misc/expression-problem.md
;; ./misc/projection-and-selection.md
(defrel (unary-naturalo n)
  (conde ((== n '()))
         ((fresh (rec) (== n `(s . ,rec))
                       (unary-naturalo rec)))))

(defrel (unary-eveno n)
  (conde ((== n '()))
         ((fresh (rec) (== n `(s s . ,rec))
                       (unary-eveno rec)))))

(defrel (binary-naturalo n)
  (conde ((== n '()))
         ((fresh (h t)
                 (== n `(,h . ,t))
                 (conde ((== h 0) (pairo t))
                        ((== h 1)))
                 (binary-naturalo t)))))

(defrel (binary-eveno n)
  (conde ((== n '()))
         ((fresh (t)
                 (== n `(0 . t))
                 (binary-naturalo t)))))

(defrel (topo x)
  (== #t #t))

(defrel (bottomo)
  (== #t #f))

;; ./scribble-htmls/katex/README.md
;; ./8-Implementing-miniKanren/1-Dovetailing-Streams.md
(define (dovetail $x $y)
  (let loop (($x $x)      ($y $y)
             (seen-x '()) (seen-y '())
             (turn 'x))
    (cond ((and (null? $x) (null? $y)) '())

          ((and (null? $x) (equal? turn 'x)) (loop '() $y seen-x seen-y 'y))
          ((and (null? $y) (equal? turn 'y)) (loop $x '() seen-x seen-y 'x))

          ((procedure? $x) (λ () (loop ($x) $y  seen-x seen-y turn)))
          ((procedure? $y) (λ () (loop  $x ($y) seen-x seen-y turn)))
          
          ((equal? turn 'x) (append
                              (map (λ (y) (cons (car $x) y)) seen-y)
                              (loop (cdr $x) $y (cons (car $x) seen-x) seen-y 'y)))
          ((equal? turn 'y) (append
                              (map (λ (x) (cons x (car $y))) seen-x)
                              (loop $x (cdr $y) seen-x (cons (car $y) seen-y) 'x))))))

(define (count-up n)
  (cons n (lambda () (count-up (+ n 1)))))

(define (take n $)
  (cond ((or (zero? n) (null? $)) '())
        ((pair? $) (cons (car $) (take (- n 1) (cdr $))))
        (else (take n ($)))))

;; ./8-Implementing-miniKanren/3-miniKanren-in-C.md
;; ./8-Implementing-miniKanren/2-Defunctionalization.md
(define (append-inf s-inf t-inf)
  (cond
    ((null? s-inf) t-inf)
    ((pair? s-inf)
     (cons (car s-inf)
           (append-inf (cdr s-inf) t-inf)))
    (else (lambda ()
            (append-inf t-inf (s-inf))))))

(define (append-map-inf g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf (g (car s-inf))
       (append-map-inf g (cdr s-inf))))
    (else (lambda () 
            (append-map-inf g (s-inf))))))

