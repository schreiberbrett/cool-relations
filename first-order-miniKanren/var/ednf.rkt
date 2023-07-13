#lang typed/racket

(require "exp.rkt")
(require "util.rkt")

(provide to-ednf to-exp)

(define-type BaseRelation (U Relation ==))

(struct EDNF ([vars : (Listof Var)] [dnf : (Listof (Listof BaseRelation))]) #:transparent)

(: to-exp (-> EDNF Exp))
(define (to-exp ednf)
  (match ednf
    ((EDNF vars dnf)
     (Fresh vars (list
                  (Conde (map replace-relations dnf)))))))

(: replace-relations (-> (Listof BaseRelation) (Listof Exp)))
(define (replace-relations l)
  (append-map (lambda ([x : BaseRelation])
                (match x
                  ((== u v) (list (== u v)))
                  ((Relation name args exps) (exps)))) l))

(: to-ednf (-> Exp EDNF))
(define (to-ednf exp)
  (match exp
    ((Fresh us exps) (match (to-ednf* exps)
                       ((EDNF vs dnf) (EDNF (append us vs) dnf))))

    ((Conde exps*) (ednf-or* (map to-ednf* exps*)))
    ((== u v) (EDNF '() `((,(== u v)))))
    ((Relation name args exps) (EDNF '() `((,(Relation name args exps)))))))

    

(: to-ednf* (-> (Listof Exp) EDNF))
(define (to-ednf* exps)
  (ednf-and* (map to-ednf exps)))



(: ednf-or (-> EDNF EDNF EDNF))
(define (ednf-or x y)
  (match `(,x ,y)
    (`(,(EDNF us x-dnf) ,(EDNF vs y-dnf))
     (EDNF (append us vs) (append x-dnf y-dnf)))))


(: ednf-and (-> EDNF EDNF EDNF))
(define (ednf-and x y)
  (match `(,x ,y)
    (`(,(EDNF us x-dnf) ,(EDNF vs y-dnf))
     (EDNF (append us vs)
           (cartesian-product-with (lambda ([x : (Listof BaseRelation)] [y : (Listof BaseRelation)])
                                     (append x y))
                                   x-dnf y-dnf)))))

(: ednf-or* (-> (Listof EDNF) EDNF))
(define (ednf-or* ednfs)
  (foldl ednf-or (EDNF '() '()) ednfs))

(: ednf-and* (-> (Listof EDNF) EDNF))
(define (ednf-and* ednfs)
  (foldl ednf-and (EDNF '() '(())) ednfs))
