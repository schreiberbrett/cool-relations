#lang racket

(require "../../faster-minikanren/mk.rkt")

(defrel (membero x l b)
  (conde ((== l '()) (== b #f))
         ((fresh (a d)
                 (== l `(,a . ,d))
                 (conde ((== a x) (== b #t))
                        ((=/= a x) (membero x d b)))))))

(define balanced-parens-grammar
  '((S () (a S b))))

(defrel (ino l grammar)
  (