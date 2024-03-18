#lang racket

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) gs ...)
     (lambda ()
       (apply (lambda (x ...) gs ...)
              (map (lambda (_) (gensym)) '(x ...)))))))