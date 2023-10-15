#lang racket
(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(require "riffleo.mk.rkt")


(defrel (&o a b ab)
  (conde ((== a '+) (== b '+) (== ab '+))
         ((== a '+) (== b '-) (== ab 'o))
         ((== a '+) (== b 'o) (== ab '+))
         ((== a '-) (== b '+) (== ab 'o))
         ((== a '-) (== b '-) (== ab '-))
         ((== a '-) (== b 'o) (== ab '-))
         ((== a 'o) (== b '+) (== ab '+))
         ((== a 'o) (== b '-) (== ab '-))
         ((== a 'o) (== b 'o) (== ab 'o))))

(defrel (resolveo c1 c2 co)
  (conde ((== c1 '()) (== c2 '()) (== co '()))
         ((fresh (a1 a2 ao d1 d2 do)
                 (== c1 `(,a1 . ,d1))
                 (== c2 `(,a2 . ,d2))
                 (== co `(,ao . ,do))
                 (&o a1 a2 ao)
                 (resolveo d1 d2 do)))))

(defrel (empty-clauseo c)
  (conde ((== c '()))
         ((fresh (d)
                 (== c `(o . ,d))
                 (empty-clauseo d)))))

(defrel (tautologyo cnf proof)
  (fresh (a d x y others)
         (== proof `(,a . ,d))
         (riffleo `(,x ,y) others d)
         (resolveo x y a)
         (conde ((== proof cnf) (empty-clauseo a))
                ((tautologyo cnf d)))))


