#lang typed/racket

(require "../existential-disjunctive-normal-form/existential-disjunctive-normal-form.rkt")
(require "../mk-expression-functions/mk-expression-functions.rkt")
(require "../s-expression-functions/s-expression-functions.rkt")
(require "../misc/misc.rkt")

(provide replace-definitions Defrel ==)

(struct Defrel ([name : Symbol] [params : (Listof Symbol)] [exps : (Listof MKexp)]))

(: replace-definitions (-> (Listof Defrel) (Listof MKexp) (Listof MKexp)))
(define (replace-definitions defrels exps)
  (append-map
   (lambda ([exp : MKexp])
     (match exp
       ((Conde exps*)
        (list (Conde (map (lambda ([exps : (Listof MKexp)]) (replace-definitions defrels exps)) exps*))))

       ((Fresh vars exps)
        (list (Fresh vars (replace-definitions defrels exps))))

       (`(== ,u ,v) `((== ,u ,v)))

       (`(,rel . ,args)
        (match (lookup-defrel rel defrels)
          ((Defrel _name params exps)
           (map (lambda ([exp : MKexp]) (replace-in-exp (zip params args) exp)) exps))))))
   exps))

;; The defrel being looked up must exist! Otherwise, this function fails.
(: lookup-defrel (-> Symbol (Listof Defrel) Defrel))
(define (lookup-defrel rel defrels)
  (car (filter (lambda ([defrel : Defrel])
                 (match defrel
                   ((Defrel name params exps) (equal? rel name))))
               defrels)))


(struct == ([u : Sexp] [v : Sexp]))

(: simple-conjunctions (-> EDNF (Listof (Listof ==))))
(define (simple-conjunctions ednf)
  (match ednf
    ((EDNF vars conjunctions)
     
     (justs (map parse-simple-conjunction conjunctions)))))

(: parse-== (-> Relation (Option ==)))
(define (parse-== x)
  (match x
    ((Relation '== `(,u ,v)) (== u v))
    (_ #f)))


;; A more complex fold using Option
(: parse-simple-conjunction (-> (Listof Relation) (Option (Listof ==))))
(define (parse-simple-conjunction l)
  (match l
    ('() '())
    (`(,h . ,t) (let ([result-h (parse-== h)]
                      [result-t (parse-simple-conjunction t)])
                  (if (or (equal? result-h #f) (equal? result-t #f))
                      #f
                      (cons result-h result-t))))))



(: unify (-> (Listof Sexp)
             (Listof ==)
             (Option (Listof Sexp))))

(define (unify sexps equations)
  (match equations
    ('() sexps)
    (`(,(== u v) . ,rest) (cond ((equal? u v) (unify sexps rest))
                                ((symbol? u) (if (occurs-in-sexp? u v)
                                                 #f
                                                 (unify (map (lambda ([sexp : Sexp])
                                                               (replace-in-sexp `((,u . ,v)) sexp)) sexps) (map (lambda ([eqn : ==]) (match eqn
                                                                                                                                       ((== x y) (== (replace-in-sexp `((,u . ,v)) x) (replace-in-sexp `((,u . ,v)) y))))) rest))))
                                ((symbol? v) (if (occurs-in-sexp? v u)
                                                 #f
                                                 (unify (map (lambda ([sexp : Sexp])
                                                               (replace-in-sexp `((,v . ,u)) sexp)) sexps) (map (lambda ([eqn : ==]) (match eqn
                                                                                                                                       ((== x y) (== (replace-in-sexp `((,v . ,u)) x) (replace-in-sexp `((,v . ,u)) y))))) rest))))
                                ((and
                                  (pair? u)
                                  (pair? v)
                                  (not (equal? (car u) 'quote))
                                  (not (equal? (car u) 'quasiquote))
                                  (not (equal? (car v) 'quote))
                                  (not (equal? (car v) 'quasiquote))) #f)
                                 
                                (else #f)
                                ;; TODO: add Pair, Pair case
                                ))))


