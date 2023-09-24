#lang racket

(require "../../faster-minikanren/mk.rkt")

(defrel (pairo x)
  (fresh (a d) (== x `(,a . ,d))))

(defrel (riffleo l1 l2 lo)
  (fresh (h1 t1 h2 t2 ho to z0 z1)
         (conde ((== l1 '()) (== l2 '()) (== lo '()))
                ((pairo l1) (== l2 '()) (== lo l1))
                ((== l1 '()) (pairo l2) (== lo l2))

                ((== l1 `(,h1 . ,t1))
                 (== l2 `(,h2 . ,t2))
                 (== lo `(,ho . ,to))

                 ;; Avoid an extra recursive call by introducing z0 and z1
                 (conde ((== ho h1) (== z0 t1) (== z1 l2))
                        ((== ho h2) (== z0 l1) (== z1 t2)))
                   
                 (riffleo z0 z1 to)))))

(defrel (all-== x l)
  (conde ((== l '()))
         ((fresh (d)
                 (== l `(,x . ,d))
                 (all-== x d)))))

(defrel (all-=/= x l)
  (conde ((== l '()))
         ((fresh (a d)
                 (== l `(,a . ,d))
                 (=/= x a)
                 (all-=/= x d)))))

(defrel (<-lengtho l1 l2)
  (conde ((== l1 '()) (pairo l2))
         ((fresh (a1 d1 a2 d2)
                 (== l1 `(,a1 . ,d1))
                 (== l2 `(,a2 . ,d2))
                 (<-lengtho d1 d2)))))

#;(defrel (majorityo e l)
    (fresh (list-of-es list-of-not-es)
           (all-==  e list-of-es)
           (all-=/= e list-of-not-es)
           (<-lengtho list-of-not-es list-of-es)
           (riffleo list-of-es list-of-not-es l)))

; Combine first 3 lines
#;(defrel (conj3o e list-of-es list-of-not-es)
    (all-==  e list-of-es)
    (all-=/= e list-of-not-es)
    (<-lengtho list-of-not-es list-of-es))

#;(defrel (majorityo e l)
    (fresh (list-of-es list-of-not-es)
           (conj3o e list-of-es list-of-not-es)
           (riffleo list-of-es list-of-not-es l)))

#;(defrel (conj3o e list-of-es list-of-not-es)
    (all-==  e list-of-es)
    (all-=/= e list-of-not-es)
    (<-lengtho list-of-not-es list-of-es))

; Replace relations with definitions

(defrel (conj3o e list-of-es list-of-not-es)
  #;(all-==  e list-of-es)
  (conde ((== list-of-es '()))
         ((fresh (d)
                 (== list-of-es `(,e . ,d))
                 (all-== e d))))
  
  #;(all-=/= e list-of-not-es)
  (conde ((== list-of-not-es '()))
         ((fresh (a d)
                 (== list-of-not-es `(,a . ,d))
                 (=/= e a)
                 (all-=/= e d))))
  #;(<-lengtho list-of-not-es list-of-es)
  (conde ((== list-of-not-es '()) (pairo list-of-es))
         ((fresh (a1 d1 a2 d2)
                 (== list-of-not-es `(,a1 . ,d1))
                 (== list-of-es `(,a2 . ,d2))
                 (<-lengtho d1 d2)))))
