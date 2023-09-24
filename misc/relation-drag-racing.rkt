#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-arith.scm")

(define (base-ten n)
  (match n
    ('() 0)
    (`(,head . ,tail) (+ head (* 2 (base-ten tail))))))

(defrel (multiple-of-3o-3n x)
  (fresh (n)
         (*o '(1 1) n x)))

(defrel (multiple-of-3o-n3 x)
  (fresh (n)
         (*o n '(1 1) x)))

(defrel (dfao l state)
  (conde ((== l '()) (== state 'q1))

         ((fresh (head tail next-state)
                 (== l `(,head . ,tail))

                 (conde ((== head 0)
                         (pairo tail)
                         (conde ((== state 'q1) (== next-state 'q1))
                                ((== state 'q2) (== next-state 'q3))
                                ((== state 'q3) (== next-state 'q2))))

                        ((== head 1)
                         (conde ((== state 'q1) (== next-state 'q2))
                                ((== state 'q2) (== next-state 'q1))
                                ((== state 'q3) (== next-state 'q3)))))

                 (dfao tail next-state)))))

(defrel (pairo l)
  (fresh (a d) (== l `(,a . ,d))))

(defrel (multiple-of-3o-dfa x)
  (dfao x 'q1))

(defrel (danger! x ground)
  (conde ((== x (build-num ground)))
         ((danger! x (+ 3 ground)))))

(defrel (multiple-of-3o-enum x)
  (danger! x 0))