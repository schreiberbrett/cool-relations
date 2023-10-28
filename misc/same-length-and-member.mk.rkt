#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(defrel (has-appleo l)
  (fresh (a d)
         (== l `(,a . ,d))
         (conde ((== a 'apple))
                ((has-appleo d)))))

(defrel (all-orangeso l)
  (conde ((== l '()))
         ((fresh (a d)
                 (== l `(,a . ,d))
                 (== a 'orange)
                 (all-orangeso d)))))
(defrel (o1 l)
  (conde ((has-appleo l))
         ((all-orangeso l))))

(defrel (o2 l)
  (fresh (s)
         (conde ((== 'has-apple s))
                ((== 'all-oranges s)))
         (rho s l)))

(defrel (rho s l)
  (fresh (a d)
         (conde ((== s 'has-apple)
                 (== l `(,a . ,d))
                 (== a 'apple))

                ((== s 'all-oranges)
                 (== l '()))

                ((== l `(,a . ,d))
                 (conde ((== s 'has-apple))
                        ((== s 'all-oranges) (== a 'orange)))
                 (rho s d)))))
