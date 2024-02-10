#lang racket

(require "../faster-minikanren/mk.rkt")

(defrel (majorityo x l)
  (fresh (difference)
    (>0o difference)
    (count-differenceo x l difference)))

(defrel (-1o x x-1)
  (+1o x-1 x))

(defrel (count-differenceo x l c)
  (conde ((== l '()) (== c '()))
         ((fresh (a d rec alpha1 alpha2)
            (== l `(,a . ,d))
            (conde ((== a x) (== `(,alpha1 ,alpha2) `(,rec ,c)))
                   ((== `(,alpha1 ,alpha2) `(,c ,rec))))
            (+1o alpha1 alpha2)
            (count-differenceo x d rec)))))

(defrel (>0o x)
  (fresh (_)
    (== x `(+ . ,_))))

(defrel (+1o x x+1)
  (conde ((== x   `(- . ,x+1)))
         ((== x+1 `(+ . ,x)))))

(defrel (+1o2 x x+1)
  (fresh (p z n l)
    (conde ((== x `((,p ,z 0) ,l)) (== x+1 `((1 0 0) (o . ,l))))
           ((== x `((0 0 1) (o . ,l))) (== x+1 `((0 ,z ,n) ,l))))))

