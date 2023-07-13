#lang typed/racket

(require "util.rkt")
(provide Exp L1 gen-var Var Relation Fresh Conde == get-goals replace-relation replace-relations to-sexp l1-equal? occurs-in?)



(struct Fresh ([vars : (Listof Var)] [exps : (Listof Exp)]) #:transparent)
(struct Conde ([exps* : (Listof (Listof Exp))]) #:transparent)
(struct == ([u : L1] [v : L1]) #:transparent)
(struct Relation ([name : Symbol] [args : (Listof L1)]
                                  [exps : (-> (Listof Exp))]) #:transparent)

(define-type Exp (U Fresh
                    Conde
                    ==
                    Relation))

(define-type L1 (U Number
                   Boolean
                   Null
                   Symbol
                   (Pairof L1 L1)
                   Var))

(struct Var ([name : Symbol] [n : Integer]) #:transparent)


(: gen-var (-> Symbol Var))
(define _n -1)
(define (gen-var s)
  (set! _n (+ 1 _n))
  (Var s _n))


(: get-goals (-> Relation (Listof Exp)))
(define (get-goals d)
  (match d
    ((Relation _name _args goals) (goals))))

(: replace-relations (-> (Listof Exp) (Listof Exp)))
(define (replace-relations l) (append-map replace-relation l))

(: replace-relation (-> Exp (Listof Exp)))
(define (replace-relation x)
  (match x
    ((Fresh vars exps) (list (Fresh vars (replace-relations exps))))
    ((Conde exps*) (list (Conde (map replace-relations exps*))))
    ((== u v) (list (== u v)))
    ((Relation _name _args exps) (exps))))



(: to-sexp (-> Exp Any))
(define (to-sexp exp)
  (match exp
    ((Fresh vars exps) `(fresh ,(map l1-to-sexp vars) . ,(map to-sexp exps)))
    ((Conde exps*) `(conde . ,(map* to-sexp exps*)))
    ((== u v) `(== ,(l1-to-sexp u) ,(l1-to-sexp v)))
    ((Relation name args exps) `(,name . ,(map l1-to-sexp args)))))


;; L1 functions

(: l1-equal? (-> L1 L1 Boolean))
(define (l1-equal? x y)
  (match `(,x ,y)
    (`(,(Var s1 n1) ,(Var s2 n2)) (and (equal? s1 s2) (equal? n1 n2)))
    (`((,h1 . ,t1) (,h2 . ,t2)) (and (l1-equal? h1 h2) (l1-equal? t1 t2)))
    (`((,h1 . ,t1) ,y) #f)
    (`(,x (,h2 . ,t2)) #f)
    (`(,x ,y) (equal? x y))))

(: occurs-in? (-> L1 L1 Boolean))
(define (occurs-in? x y)
  (or (l1-equal? x y)
      (match y
        (`(,h . ,t) (or (occurs-in? x h) (occurs-in? x t)))
        (y #f))))

(: l1-to-sexp (-> L1 Any))
(define (l1-to-sexp l1)
  (match l1
    (`(,a . ,d) `(,(l1-to-sexp a) . ,(l1-to-sexp d)))
    ((Var sym num) (list sym num))
    (x x)))

