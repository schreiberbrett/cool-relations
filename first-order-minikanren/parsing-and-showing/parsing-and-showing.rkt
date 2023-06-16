#lang typed/racket

(require "../defrel-and-run/defrel-and-run.rkt")
(require "../mk-expression-functions/mk-expression-functions.rkt")
(require "../existential-disjunctive-normal-form/existential-disjunctive-normal-form.rkt")

(provide parse-defrel show-defrel
         parse-exp    show-exp)

(: parse-defrel (-> Sexp Defrel))
(define (parse-defrel x)
  (match x
    (`(defrel (,name . ,args) . ,exps) (Defrel (parse-symbol name) (parse-symbols args) (parse-exps exps)))))

(: parse-symbol (-> Sexp Symbol))
(define (parse-symbol x)
  (if (symbol? x) x 'parse-failure-should-never-see-this))

(: parse-symbols (-> Sexp (Listof Symbol)))
(define (parse-symbols xs)
  (match xs
    ('() '())
    (`(,h . ,t) (cons (parse-symbol h) (parse-symbols t)))))

(: parse-exp (-> Sexp MKexp))
(define (parse-exp x)
  (match x
    (`(conde . ,exps*) (Conde (parse-exps* exps*)))
    (`(fresh . (,vars . ,exps)) (Fresh (parse-symbols vars) (parse-exps exps)))
    (`(,rel . ,args) `(,(parse-symbol rel) . (parse-sexps ,args)))))

(: parse-exps (-> Sexp (Listof MKexp)))
(define (parse-exps x)
  (match x
    ('() '())
    (`(,h . ,t) (cons (parse-exp h) (parse-exps t)))))

(: parse-exps* (-> Sexp (Listof (Listof MKexp))))
(define (parse-exps* x)
  (match x
    ('() '())
    (`(,h . ,t) (cons (parse-exps h) (parse-exps* t)))))

(: parse-sexps (-> Sexp (Listof Sexp)))
(define (parse-sexps x)
  (match x
    ('() '())
    (`(,h . ,t) `(,h . ,(parse-sexps t)))))


(: show-defrel (-> Defrel Any))
(define (show-defrel x)
  (match x
    ((Defrel name params exps) `(defrel '(,name . ,params) . ,exps))))

(: show-exp (-> MKexp Any))
(define (show-exp x)
  (match x
    ((Conde exps*) `(conde . ,exps*))
    ((Fresh vars exps) `(fresh ,vars . ,exps))
    (`(,rel . ,args) '(,rel . ,args))))

(: show-ednf (-> EDNF Any))
(define (show-ednf x)
  (match x
    ((EDNF vars conjunctions) `(fresh ,vars (conde ,conjunctions)))))