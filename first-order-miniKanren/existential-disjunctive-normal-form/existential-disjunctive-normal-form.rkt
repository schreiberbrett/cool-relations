#lang typed/racket

(require "../mk-expression-functions/mk-expression-functions.rkt")

(provide exp-to-ednf EDNF)

(struct EDNF ([vars : (Listof Symbol)] [conjunctions : (Listof (Listof Relation))]))

(: exp-to-ednf (-> MKexp EDNF))
(define (exp-to-ednf exp)
  (match exp
    ((Fresh vars exps)
     (match (conjunction* (map exp-to-ednf exps))
       ((Fresh new-vars new-exps)
        (Fresh (append vars new-vars) new-exps))))

    ((Conde exps*) (disjunction* (map (lambda ([exps : (Listof MKexp)]) (conjunction* (map exp-to-ednf exps))) exps*)))

    ((Relation name args) (EDNF '() (list (list (Relation name args)))))))

(: conjunction (-> EDNF EDNF EDNF))
(define (conjunction a b)
  (match `(,a ,b)
    (`(,(EDNF a-vars a-conjunctions) ,(EDNF b-vars b-conjunctions))
     (EDNF (append a-vars b-vars) (cartesian-product-with-append a-conjunctions b-conjunctions)))))


(: disjunction (-> EDNF EDNF EDNF))
(define (disjunction ednf-a ednf-b)
  (match `(,ednf-a ,ednf-b)
    (`(,(EDNF a-vars a-conjunctions)
       ,(EDNF b-vars b-conjunctions))

     (EDNF (append a-vars b-vars) (append a-conjunctions b-conjunctions)))))

;; Simple folds

(: conjunction* (-> (Listof EDNF) EDNF))
(define (conjunction* ednfs)
         (match ednfs
           ('() (EDNF '() '(())))
           (`(,a . ,d) (conjunction a (conjunction* d)))))

(: disjunction* (-> (Listof EDNF) EDNF))
(define (disjunction* ednfs)
         (match ednfs
           ('() (EDNF '() '(())))
           (`(,a . ,d) (disjunction a (disjunction* d)))))



(: cartesian-product-with-append (All (a) (-> (Listof (Listof a)) (Listof (Listof a)) (Listof (Listof a)))))
(define (cartesian-product-with-append xs ys)
    (append-map (lambda ([x : (Listof a)])
        (map (lambda ([y : (Listof a)]) (append x y)) ys)) xs))
