#lang typed/racket

(require "../misc/misc.rkt")

(provide replace-in-sexp occurs-in-sexp?)

(: replace-in-sexp (-> (Listof (Pairof Symbol Sexp)) Sexp Sexp))
(define (replace-in-sexp replacements sexp)
  (match sexp
    (`(,h . ,t) (match h
                  ('quote sexp)
                  ('quasiquote (cons h (qq-replace-in-sexp replacements t)))
                  (_ (cons (replace-in-sexp replacements h)
                           (replace-in-sexp replacements t)))))
        
    (_ (match (lookup sexp replacements)
         (#f sexp)
         (replacement replacement)))))

(: qq-replace-in-sexp (-> (Listof (Pairof Symbol Sexp)) Sexp Sexp))
(define (qq-replace-in-sexp replacements sexp)
  (match sexp
    (`(,h . ,t) (match h
                  ('unquote (cons h (replace-in-sexp replacements t)))
                  ('quote (cons h t))
                  (_ (cons (qq-replace-in-sexp replacements h)
                           (qq-replace-in-sexp replacements t)))))

    (_ sexp)))

(: occurs-in-sexp? (-> Symbol Sexp Boolean))
(define (occurs-in-sexp? sym sexp)
         (or (equal? sym sexp)
             (match sexp
               (`(,h . ,t) (match h
                             ('quasiquote (qq-occurs-in-sexp? sym t))
                             ('quote #f)
                             (_ (or (occurs-in-sexp? sym h)
                                    (occurs-in-sexp? sym t)))))
               (_ #f))))

(: qq-occurs-in-sexp? (-> Symbol Sexp Boolean))
(define (qq-occurs-in-sexp? sym sexp)
         (match sexp
           (`(,h . ,t) (match h
                         ('unquote (occurs-in-sexp? sym t))
                         ('quote #f)
                         (_ (or (qq-occurs-in-sexp? sym h)
                                (qq-occurs-in-sexp? sym t)))))
           (_ #f)))