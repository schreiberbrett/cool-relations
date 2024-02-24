#lang racket

(require "../faster-minikanren/main.rkt")

(define ≡ ==)

(defrel (3color₁ᵒ g m)
  (conde ((≡ g '()))
         ((fresh (a d u v cᵤ cᵥ)
            (≡ g `(,a . ,d))
            (≡ a `(,u ,v))
            (different-colorsᵒ cᵤ cᵥ)
            (list-refᵒ m u cᵤ)
            (list-refᵒ m v cᵥ)
            (conde ((≡ d `()))
                   ((fresh (ad dd)
                      (≡ d `(,ad . ,dd))
                      (<ₚᵒ a ad)
                      (3color₁ᵒ d m))))))))

(defrel (list-refᵒ l n val)
  (fresh (a d n-1)
         (≡ l `(,a . ,d))

         (conde ((≡ n '()) (≡ a val))

                ((≡ n `(s . ,n-1))
                 (list-refᵒ d n-1 val)))))

(defrel (different-colorsᵒ c₁ c₂)
  (conde ((≡ c₁ 'red)   (≡ c₂ 'blue))
         ((≡ c₁ 'red)   (≡ c₂ 'green))
         ((≡ c₁ 'blue)  (≡ c₂ 'red))
         ((≡ c₁ 'blue)  (≡ c₂ 'green))
         ((≡ c₁ 'green) (≡ c₂ 'red))
         ((≡ c₁ 'green) (≡ c₂ 'blue))))

(defrel (<ₙᵒ n₁ n₂)
  (fresh (n₂-1)
    (≡ n₂ `(s . ,n₂-1))
    (conde ((≡ n₁ '()))
           ((fresh (n₁-1)
              (≡ n₁ `(s . ,n₁-1))
              (<ₙᵒ n₁-1 n₂-1))))))

(defrel (<ₚᵒ p₁ p₂)
  (fresh (l₁ r₁ l₂ r₂ α₁ α₂)
    (≡ p₁ `(,l₁ ,r₁))
    (≡ p₂ `(,l₂ ,r₂))
    
    (conde ((≡ `(,α₁ ,α₂) `(,l₁ ,l₂)))
           ((≡ l₁ l₂) (== `(,α₁ ,α₂) `(,r₁ ,r₂))))

    (<ₙᵒ α₁ α₂)))

