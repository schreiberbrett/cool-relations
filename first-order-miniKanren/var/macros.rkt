#lang racket

(require "exp.rkt")
(require "ednf.rkt")

(provide fresh conde defrel)

(define-syntax fresh
  (syntax-rules ()
    ((fresh (x ...) g ...)
     (let ((vars (map (lambda (sym) (gen-var sym)) '(x ...))))
       (Fresh vars (apply (lambda (x ...) (list g ...)) vars))))))

(define-syntax conde
  (syntax-rules ()
    ((conde (g ...) ...)
     (Conde (list (list g ...) ...)))))

(define-syntax defrel
  (syntax-rules ()
    ((defrel (r x ...) g ...)
     (define (r x ...)
       (Relation (quote r)
                 (list x ...)
                 (lambda () (list g ...)))))))

(defrel (appendo l r o)
  (conde ((== l '()) (== r o))
         ((fresh (a d rec)
                 (== l `(,a . ,d))
                 (== o `(,a . ,rec))
                 (appendo d r rec)))))