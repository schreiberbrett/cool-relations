#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

#;(require (rename-in "../../first-order-miniKanren/mk-fo.rkt" [define-relation defrel]))

(require "pairo.mk.rkt")

(provide riffleo)

;; This is the basic definition of riffleo. It seems to work just fine, but it may diverge on certain inputs.
#;(defrel (riffleo l₁ l₂ lₒ)
    (fresh (h t to)
           (conde ((== l₁ '()) (== l₂ '()) (== lₒ '()))
                  ((== l₁ `(,h . ,t)) (== lₒ `(,h . ,to)) (riffleo t l₂ to))
                  ((== l₂ `(,h . ,t)) (== lₒ `(,h . ,to)) (riffleo l₁ t to)))))


;; This version of riffleo has only one recursive call, so it should diverge less. But I don't know exactly when.
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


(defrel (ko x)
  (conde ((== x '()))
         ((fresh (a d)
                 (== x `(,a . ,d))
                 (conde ((ko a))
                        ((ko d)))))))

(defrel (jo x)
  (conde ((== x '()))
         ((fresh (a d alpha)
                 (== x `(,a . ,d))
                 (conde ((== a alpha))
                        ((== d alpha)))
                 (jo alpha)))))