#lang typed/racket

(require "../s-expression-functions/s-expression-functions.rkt")
(require "../misc/misc.rkt")

(provide replace-in-exp MKexp Fresh Conde)

(struct Fresh ([vars : (Listof Symbol)] [exps : (Listof MKexp)]))
(struct Conde ([exps* : (Listof (Listof MKexp))]))

(define-type MKexp
  (U Fresh
     Conde
     (Pairof Symbol (Listof Sexp))))

(: replace-in-exp (-> (Listof (Pairof Symbol Sexp)) MKexp MKexp))
(define (replace-in-exp replacements exp)
  (match exp
    ((Conde exps*) (Conde (map* (lambda ([exp : MKexp])
                                  (replace-in-exp replacements exp)) exps*)))

    ((Fresh vars exps)
     (let ((gensyms (map (lambda (var) (gensym)) vars)))
       (Fresh gensyms (map (lambda ([exp : MKexp]) (replace-in-exp (append (zip vars gensyms) replacements) exp)) exps))))

    (`(,rel . ,args) `(,rel . ,(map (lambda ([arg : Sexp])
                                      (replace-in-sexp replacements arg)) args)))))