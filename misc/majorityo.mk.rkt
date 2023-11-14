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

(defrel (riffleo l₁ l₂ lₒ)
  (fresh (a₁ d₁ a₂ d₂ aₒ dₒ)
         (conde ((== l₁ '()) (== l₂ '()) (== lₒ '()))
                
                ((== l₁ `(,a₁ . ,d₁)) (== l₂ '()) (== lₒ l₁))
                ((== l₁ '()) (== l₂ `(,a₂ . ,d₂)) (== lₒ l₂))

                ((== l₁ `(,a₁ . ,d₁))
                 (== l₂ `(,a₂ . ,d₂))
                 (== lₒ `(,aₒ . ,dₒ))
                 (fresh (α₁ α₂ α₃)
                        (conde ((== aₒ a₁) (== `(,α₁ ,α₂ ,α₃) `(,d₁ ,l₂ ,dₒ)))
                               ((== aₒ a₂) (== `(,α₁ ,α₂ ,α₃) `(,l₁ ,d₂ ,dₒ))))
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