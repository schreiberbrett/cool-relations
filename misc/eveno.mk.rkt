#lang racket

(require "../../faster-minikanren/mk.rkt")

(provide eveno)

(defrel (eveno l)
  (conde ((== l '()))
         ((fresh (a1 a2 d)
                 (== l `(,a1 ,a2 . ,d))
                 (eveno d)))))
