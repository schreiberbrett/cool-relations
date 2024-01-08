#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(defrel (majorityo1 x l)
  (fresh (majority rest)
         (==* x majority)
         (longero majority rest)
         (riffleo majority rest l)))

(defrel (==* x l)
  (conde ((== l '()))
         ((fresh (d)
                 (== l `(,x . ,d))
                 (==* x d)))))

(defrel (longero l₁ l₂)
  (fresh (a₁ d₁ a₂ d₂)
         (== l₁ `(,a₁ . ,d₁))
         (conde ((== l₂ '()))
                ((== l₂ `(,a₂ . ,d₂))
                 (longero d₁ d₂)))))

(defrel (riffleo l₁ l₂ l₁⊔l₂)
  (fresh (a₁ d₁ a₂ d₂ d₁⊔l₂ l₁⊔d₂)
         (conde ((== l₁ '()) (== l₂ '()) (== l₁⊔l₂ '()))
                
                ((== l₁ `(,a₁ . ,d₁)) (== l₂ '()) (== l₁⊔l₂ l₁))
                ((== l₁ '()) (== l₂ `(,a₂ . ,d₂)) (== l₁⊔l₂ l₂))

                ((== l₁ `(,a₁ . ,d₁))
                 (== l₂ `(,a₂ . ,d₂))

                 (fresh (α₁ α₂ α₃)
                        (conde ((== l₁⊔l₂ `(,a₁ . ,d₁⊔l₂)) (== `(,α₁ ,α₂ ,α₃) `(,d₁ ,l₂ ,d₁⊔l₂)))
                               ((== l₁⊔l₂ `(,a₂ . ,l₁⊔d₂)) (== `(,α₁ ,α₂ ,α₃) `(,l₁ ,d₂ ,l₁⊔d₂))))
                        (riffleo α₁ α₂ α₃))))))


;; longero can be replaced with one-longero
(defrel (one-longero l₁ l₂)
  (fresh (a₁ d₁ a₂ d₂)
         (conde ((== l₁ `(,a₁)) (== l₂ '()))
                ((== l₁ `(,a₁ . ,d₁))
                 (== l₂ `(,a₂ . ,d₂))
                 (one-longero d₁ d₂)))))

(defrel (majorityo2 x l)
  (fresh (majority rest)
         (==* x majority)
         (one-longero majority rest)
         (riffleo majority rest l)))

#;(run 1 (q) (majorityo₂ q '(a a)))
; > Now diverges. Freshening variables can cause divergence!

; (rho x l1 l2) <--> ((==* x l1) (longero l1 l2))
(defrel (rho x l1 l2)
  (conde ((== l1 `(,x)) (== l2 '()))
         ((fresh (d1 a2 d2)
                 (== l1 `(,x ,x . ,d1))
                 (== l2 `(,a2 . ,d2))
                 (rho x `(,x . ,d1) d2)))))

(defrel (majorityo3 x l)
  (fresh (majority rest)
         (rho x majority rest)
         (riffleo majority rest l)))

(defrel (majorityo4 x l)
  (fresh (majority rest)
         (riffleo majority rest l)
         (rho x majority rest)))

(defrel (ro x l1 l2 lo)
  (fresh (d1 a2 d2 do)
         (conde ((== l1 '(,x))
                 (== l2 '())
                 (== lo `(,x)))

                ((== l1 `(,x ,x . ,d1))
                 (== l2 `(,a2 . ,d2))
                 (== lo `(,x . ,do))
                 (ro x `(,x . ,d1) l2 do))

                ((== l1 `(,x ,x . ,d1))
                 (== l2 `(,a2 . ,d2))
                 (== lo `(,a2 . ,do))
                 (ro x l1 d2 do)))))

(defrel (majorityo5 x l)
  (fresh (majority rest)
         (ro x majority rest l)))

(defrel (+1o x x+1)
  (fresh (d)
         (conde ((== x '()) (== x+1 '(+)))
                ((== x `(+ . ,d)) (== x+1 `(+ + . ,d)))
                ((== x `(- . ,d)) (== x+1 d)))))

(defrel (margino x l m)
  (conde ((== l '()) (== m '()))
         ((fresh (a d rec)
                 (== l `(,a . ,d))

                 (conde ((== a x) (+1o rec m))
                        ((+1o m rec)))
                 
                 (margino x d rec)))))


(defrel (majorityo6 x l)
  (fresh (m)
         (conde ((== m '(+ +)))
                ((== m '(+))))
         (margino x l m)))

(defrel (majorityo7 x l)
  (fresh (m d)
         (== m `(+ . ,d))
         (margino x l m)))