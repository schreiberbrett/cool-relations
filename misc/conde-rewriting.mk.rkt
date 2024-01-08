#lang racket

(require "../../faster-minikanren/mk.rkt")

(defrel (even-lengtho l)
  (conde ((== l '()))
         ((fresh (a b c)
                 (== l `(,a ,b . ,c))
                 (even-lengtho c)))))

(defrel (odd-lengtho l)
  (conde ((fresh (e) (== l `(,e))))
         ((fresh (x y z)
                 (== l `(,x ,y . ,z))
                 (odd-lengtho z)))))

(defrel (all-bananao l)
  (conde ((== l '()))
         ((fresh (d)
                 (== l `(banana . ,d))
                 (all-bananao d)))))

(defrel (ទo sym arg)
  (fresh (a b c i)
         (conde ((== sym 'even-length)
                 (== arg '()))

                ((== sym 'odd-length)
                 (== arg `(,i)))
                             
                ((== arg `(,a ,b . ,c))
                 (ទo sym c)))))