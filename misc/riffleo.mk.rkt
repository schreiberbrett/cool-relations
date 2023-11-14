#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

#;(require (rename-in "../../first-order-miniKanren/mk-fo.rkt" [define-relation defrel]))

(require "pairo.mk.rkt")

(provide riffleo)

;; This is the basic definition of riffleo. It seems to work just fine, but it may diverge on certain inputs.
#;(defrel (riffleo l1 l2 lo)
    (fresh (h t to)
           (conde ((== l1 '()) (== l2 '()) (== lo '()))
                  ((== l1 `(,h . ,t)) (== lo `(,h . ,to)) (riffleo t l2 to))
                  ((== l2 `(,h . ,t)) (== lo `(,h . ,to)) (riffleo l1 t to)))))


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