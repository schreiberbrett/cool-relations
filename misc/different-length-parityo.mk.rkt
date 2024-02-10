#lang racket

(require "../../faster-minikanren/mk.rkt")
(require "appendo.mk.rkt")
(require "eveno.mk.rkt")
(require "oddo.mk.rkt")
(require "parityo.mk.rkt")

;; (different-length-parityo l1 l2)
;; The length of l1 is a different parity of the length of l2

(defrel (different-length-parityo1 l1 l2)
  (conde ((eveno l1) (oddo l2))
         ((oddo l1) (eveno l2))))

(defrel (different-length-parityo2 l1 l2)
  (fresh (a b)
         (conde ((== l1 a) (== l2 b))
                ((== l1 b) (== l2 a)))
         (oddo a)
         (eveno b)))

(defrel (different-length-parityo3 l1 l2)
  (fresh (p1 p2)
         (conde ((== p1 'even) (== p2 'odd))
                ((== p1 'odd) (== p2 'even)))
         (parityo l1 p1)
         (parityo l2 p2)))

(defrel (different-length-parityo4 l1 l2)
  (fresh (l1++l2)
         (oddo l1++l2)
         (appendo l1 l2 l1++l2)))

(defrel (different-length-parityo5 l1 l2)
  (fresh (l1++l2)
         (append-parityo l1 l2 l1++l2 'odd 'even)))

;; l1++l2 is the appending of l1 and l2
;; l1++l2 has parity p
(defrel (append-parityo l1 l2 l1++l2 p not-p)
  (conde ((== l1 '()) (== l2 l1++l2) (parityo2 l1++l2 p not-p))
         ((fresh (a1 d1 d1++l2)
                 (== l1 `(,a1 . ,d1))
                 (== l1++l2 `(,a1 . ,d1++l2))
                 (append-parityo d1 l2 d1++l2 not-p p)))))

(defrel (parityo2 l p not-p)
  (conde ((== l '()) (== p 'even) (== not-p 'odd))
         ((fresh (a d)
                 (== l `(,a . ,d))
                 (parityo2 d not-p p)))))
                 
