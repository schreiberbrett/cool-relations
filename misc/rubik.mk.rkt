#lang racket

(require "../../faster-minikanren/mk.rkt")

;; A (solved) Rubiks cube is represented as a list of its unfolded faces, a cube net

;;          r₁ r₂ r₃
;;          r₄ r₅ r₆
;;          r₇ r₈ r₉
;; b₁ b₂ b₃ w₁ w₂ w₃ g₁ g₂ g₃
;; b₄ b₅ b₆ w₄ w₅ w₆ g₄ g₅ g₆
;; b₇ b₈ b₉ w₇ w₈ w₉ g₇ g₈ g₉
;;          o₁ o₂ o₃
;;          o₄ o₅ o₆
;;          o₇ o₈ o₉
;;          y₁ y₂ y₃
;;          y₄ y₅ y₆
;;          y₇ y₈ y₉

(defrel (apply^o transformation input output)
  (fresh (r₁ r₂ r₃ r₄ r₅ r₆ r₇ r₈ r₉ b₁ b₂ b₃ w₁ w₂ w₃ g₁ g₂ g₃ b₄ b₅ b₆ w₄ w₅ w₆ g₄ g₅ g₆ b₇ b₈ b₉ w₇ w₈ w₉ g₇ g₈ g₉ o₁ o₂ o₃ o₄ o₅ o₆ o₇ o₈ o₉ y₁ y₂ y₃ y₄ y₅ y₆ y₇ y₈ y₉)
         (== input (list r₁ r₂ r₃ r₄ r₅ r₆ r₇ r₈ r₉ b₁ b₂ b₃ w₁ w₂ w₃ g₁ g₂ g₃ b₄ b₅ b₆ w₄ w₅ w₆ g₄ g₅ g₆ b₇ b₈ b₉ w₇ w₈ w₉ g₇ g₈ g₉ o₁ o₂ o₃ o₄ o₅ o₆ o₇ o₈ o₉ y₁ y₂ y₃ y₄ y₅ y₆ y₇ y₈ y₉))