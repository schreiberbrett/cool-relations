#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-arith.scm")

(defrel (matrix-multiplyo A X O)
  (fresh (a b c d x y z w ax ay bz bw cx cy dz dw ax+bz ay+bw cx+dz cy+dw)
         (== A `(,a ,b ,c ,d))
         (== X `(,x ,y ,z ,w))
         (== O `(,ax+bz ,ay+bw ,cx+dz ,cy+dw))

         (pluso ax bz ax+bz)
         (pluso ay bw ay+bw)
         (pluso cx dz cx+dz)
         (pluso cy dw cy+dw)

         (*o a x ax)
         (*o a y ay)
         (*o b z bz)
         (*o b w bw)
         (*o c x cx)
         (*o c y cy)
         (*o d z dz)
         (*o d w dw)))

(defrel (matrix-multiply2o A X O)
  (fresh (a b c d x y z w ax ay bz bw cx cy dz dw ax+bz ay+bw cx+dz cy+dw)
         (== A `(,a ,b ,c ,d))
         (== X `(,x ,y ,z ,w))
         (== O `(,ax+bz ,ay+bw ,cx+dz ,cy+dw))

         (*o a x ax)
         (*o a y ay)
         (*o b z bz)
         (*o b w bw)
         (*o c x cx)
         (*o c y cy)
         (*o d z dz)
         (*o d w dw)

         (pluso ax bz ax+bz)
         (pluso ay bw ay+bw)
         (pluso cx dz cx+dz)
         (pluso cy dw cy+dw)))

(defrel (matrix-multiply3o A X O)
  (fresh (a b c d x y z w ax ay bz bw cx cy dz dw ax+bz ay+bw cx+dz cy+dw)
         (== A `(,a ,b ,c ,d))
         (== X `(,x ,y ,z ,w))
         (== O `(,ax+bz ,ay+bw ,cx+dz ,cy+dw))

         (*o a x ax)
         (*o b z bz)
         (pluso ax bz ax+bz)

         (*o a y ay)
         (*o b w bw)
         (pluso ay bw ay+bw)

         (*o c x cx)
         (*o c y cy)
         (pluso cx dz cx+dz)
         
         (*o d z dz)
         (*o d w dw)
         (pluso cy dw cy+dw)))


(defrel (sum-of-products a b x y ab+xy)
  (fresh (ab xy)
         (*o a b ab)
         (*o x y xy)
         (pluso ab xy ab+xy)))


(defrel (=/=-nat n m)
  (fresh (h1 t1 h2 t2)
         (conde ((== n '()) (pairo m))
                ((pairo n) (== m '()))
                ((== n `(,h1 . ,t1))
                 (== m `(,h2 . ,t2))
                 (conde ((== h1 0) (== h2 1) (pairo t1))
                        ((== h1 1) (== h2 0) (pairo t2))
                        ((=/=-nat t1 t2)))))))


(define a1729 '(1 0 0 0 0 0 1 1 0 1 1))


;; Stalls (even when n is ground). Might need to try a few more permutations of clauses before giving up.
(defrel (taxicab-numbero n)
  (fresh (a b c d
            a^3 b^3 c^3 d^3)
         (pluso a^3 b^3 n)
         (pluso c^3 d^3 n)

         (=/=-nat a^3 c^3)
         (=/=-nat a^3 d^3)
         (=/=-nat b^3 c^3)
         (=/=-nat b^3 d^3)
         
         (cubeo a a^3)
         (cubeo b b^3)
         (cubeo c c^3)
         (cubeo d d^3)))


(defrel (cubeo n n^3)
  (fresh (n^2)
         (*o n n n^2)
         (*o n n^2 n^3)))

(defrel (cube2o n n^3)
  (fresh (n^2)
         (*o n n n^2)
         (*o n^2 n n^3)))

(defrel (pairo x)
  (fresh (h t) (== x `(,h . ,t))))

(defrel (add1 n n+1)
  (conde
   ;; 0 + 1 = 0
   ((== n '()) (== n+1 '(1)))

   ((fresh (h t t+1)
           (== n `(,h . ,t))
           (conde
            ;; 0 + 2t + 1 = 1 + 2t
            ((== h 0) (pairo t) (== n+1 `(1 . ,t)))

            ;; 1 + 2t + 1 = 2 + 2t = 2(t + 1)
            ((== h 1) 
             (== n+1 `(0 . ,t+1))
             (add1 t t+1)))))))

(defrel (y=4xo x y)
  (conde ((== x '()) (== y '()))
         ((pairo x) (== y `(0 0 . ,x)))))

(defrel (y=4x+1o x y)
  (conde ((== x '()) (== y '(1)))
         ((pairo x) (== y `(1 0 . ,x)))))


(defrel (squareo n n^2)
  (fresh (b m m^2 m+m^2)
         (conde (;; 0^2 = 0
                 (== n '())
                 (== n^2 '()))
      
                (;; 1^2 = 1
                 (== n '(1))
                 (== n^2 '(1)))
         
                (;; 
                 (== n `(,b . ,m))
                 (pairo m)
                 (conde (;; n^2 = (0 + 2m)^2 = 4m^2
                         (== b 0)
                         (== n^2 `(0 0 . ,m^2)))

                        (;; n^2 = (1 + 2m)^2 = 1 + 4(m + m^2)
                         (== b 1)
                         (== n^2 `(1 0 . ,m+m^2))
                         (pluso m m^2 m+m^2)))

                 (squareo m m^2)))))

(defrel (bo n)
  (fresh (b m)
         (conde ((== n '()))
                ((== n `(,b . ,m))
                 (conde ((== b 0) (pairo m))
                        ((== b 1)))
                 (bo m)))))

(defrel (kleeneo l o)
  (conde ((== o '()))
         ((kleene+1o l o))))

(defrel (kleene+1o l o)
  (fresh (rec)
         (conde ((== l o))
                ((appendo l rec o)
                 (kleene+1o l rec)))))

(define (num n)
  (match n
    ('() 0)
    (`(,b . ,m) (+ b (* 2 (num m))))))