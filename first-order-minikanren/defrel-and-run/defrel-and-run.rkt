#lang typed/racket

(require "../mk-expression-functions/mk-expression-functions.rkt")
(require "../misc/misc.rkt")

(provide replace-definitions Defrel)

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
          ((Defrel name params exps)
           (map (lambda ([exp : MKexp]) (replace-in-exp (zip params args) exp)) exps))))))
   exps))

;; The defrel being looked up must exist! Otherwise, this function fails.
(: lookup-defrel (-> Symbol (Listof Defrel) Defrel))
(define (lookup-defrel rel defrels)
  (car (filter (lambda ([defrel : Defrel])
                 (match defrel
                   ((Defrel name params exps) (equal? rel name))))
               defrels)))