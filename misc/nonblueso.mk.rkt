#lang racket

(require "../../faster-minikanren/mk.rkt")

;; Suppose I had a list of colors, 'red, 'green, and 'blue. And let's say I wanted all the ones that weren't blue.
;; That can be done using disequality constraints.

(defrel (nonblueso1 colors nonblues)
  (conde ((== colors '()) (== nonblues '()))
         ((fresh (a d rec)
                 (== colors `(,a . ,d))
                 (conde ((== a 'blue)
                         (== nonblues rec))
                        ((=/= a 'blue)
                         (== nonblues `(,a . ,rec))))
                 (nonblueso1 d rec)))))

;; Since we know that the only other options are 'red and 'green, this can be rewritten to avoid =/=.

(defrel (nonblueso2 colors nonblues)
  (conde ((== colors '()) (== nonblues '()))
         ((fresh (a d rec)
                 (== colors `(,a . ,d))
                 (conde ((== a 'blue)
                         (== nonblues rec))
                        ((conde ((== a 'red)) ((== a 'green)))
                         (== nonblues `(,a . ,rec))))
                 (nonblueso2 d rec)))))



(defrel (nonblueso3 colors nonblues)
  (conde ((== colors '()) (== nonblues '()))
         ((fresh (a d rec x y)
                 (== colors `(,a . ,d))
                 (conde ((== a '(0 0 1))
                         (== nonblues rec))
                        ((== a `(,x ,y 0))
                         (== nonblues `(,a . ,rec))))
                 (nonblueso3 d rec)))))