#lang racket

(require "../../faster-minikanren/main.rkt")

(defrel (3coloro edges mapping)
  (conde ((== edges '()))
         ((fresh (u v-u rest-edges c1 c2 rest-mapping)
                 (== edges `((,u . ,v-u) .rest-edges))
                 (lookup1o u mapping rest-mapping c1)
                 (lookup2o v rest-mapping c2)
                 (=/=-color c1 c2)
                 (3coloro rest-edges mapping)))))


;; e.g., when {u, v} = {5, 7}
;; {5, 7} = ((s s s s s) .  (s s))
;; then m = ( _ _ _ _ _  c1  _ c2)
(defrel (lookup*o u v c1 c2 m)

(defrel (lookup1o u c m m-u)
  (fresh (u-1 m-1 m-u-1)
         (== m `(,a . ,d))
         (conde ((== u '()) (== c a) (== m-u d))
                ((== u `(s . ,u-1))
                 (== m-u `(, . ,m-u-1))
                 (lookup1o u-1 c m-1 m-u-1)