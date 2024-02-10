#lang racket

(require "../faster-minikanren/main.rkt")

(defrel (3sato cnf assignments)
  (conde ((== cnf '()))
         ((fresh (c1 c2 c3 rest var val)
            (== cnf `((,c1 ,c2 ,c3) . ,rest))
            (conde ((== c1 `(,var ,val)))
                   ((== c2 `(,var ,val)))
                   ((== c3 `(,var ,val))))
            (lookupo assignments var val)
            (3sato rest assignments)))))

(defrel (>0o l)
  (fresh (a d)
    (== l `(,a . ,d))))

(defrel (lookupo map k v)
  (fresh (l x r a d next)
    (== map `(,x ,l ,r))
    (conde ((== k '()) (== x v))
           ((== k `(,a . ,d))
            (conde ((== a 0) (>0o d) (== next l))
                   ((== a 1) (== next r)))
            (lookupo next d v)))))

