#lang typed/racket

(require "../misc/misc.rkt")

(provide replace-in-sexp)

(: replace-in-sexp (-> (Listof (Pairof Symbol Sexp)) Sexp Sexp))
(define (replace-in-sexp replacements sexp)
        (match sexp
            (`(,h . ,t) (match h
                            ('quote sexp)
                            ('quasiquote (cons h (qq-replace-in-sexp replacements t)))
                            (_ (cons (replace-in-sexp replacements h)
                                    (replace-in-sexp replacements t)))))
        
        (_ (match (lookup sexp replacements)
            (`(just ,replacement) replacement)
            ('(nothing) sexp)))))

(: qq-replace-in-sexp (-> (Listof (Pairof Symbol Sexp)) Sexp Sexp))
(define (qq-replace-in-sexp replacements sexp)
         (match sexp
           (`(,h . ,t) (match h
                         ('unquote (cons h (replace-in-sexp replacements t)))
                         ('quote (cons h t))
                         (_ (cons (qq-replace-in-sexp replacements h)
                                  (qq-replace-in-sexp replacements t)))))

           (_ sexp)))