#lang typed/racket

(require "../s-expression-functions/s-expression-functions.rkt")
(require "../misc/misc.rkt")

(provide replace-in-exp MKexp Fresh Conde Relation)

(struct Fresh ([vars : (Listof Symbol)] [exps : (Listof MKexp)]))
(struct Conde ([exps* : (Listof (Listof MKexp))]))
(struct Relation ([name : Symbol] [args : (Listof Sexp)]))

(define-type MKexp
  (U Fresh
     Conde
     Relation))

(: replace-in-exp (-> (Listof (Pairof Symbol Sexp)) MKexp MKexp))
(define (replace-in-exp replacements exp)
  (match exp
    ((Conde exps*) (Conde (map* (lambda ([exp : MKexp])
                                  (replace-in-exp replacements exp)) exps*)))

    ((Fresh vars exps)
     (let ((gensyms (map (lambda (var) (gensym)) vars)))
       (Fresh gensyms (map (lambda ([exp : MKexp]) (replace-in-exp (append (zip vars gensyms) replacements) exp)) exps))))

    ((Relation name args) (Relation name (map (lambda ([arg : Sexp])
                                                (replace-in-sexp replacements arg)) args)))))