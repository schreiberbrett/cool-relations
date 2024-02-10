#lang racket

(defrel (>0o n)
  (fresh (a d) (== n `(,a . ,d))))

(defrel (elemo n s)
  (fresh (l m r)
    (== s `(,l ,m ,r))
    (conde ((== n '()) (== m #t))
           ((fresh (a d rec)
              (== n `(,a . ,d))
              (conde ((== a 0) (>0o d) (== rec l))
                     ((== a 1) (== rec r)))
              (elemo d rec))))))

