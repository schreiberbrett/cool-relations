#lang racket

(define (map-exp exp f)
  (match exp
    (`(fresh ,vars . ,exps) (f `(fresh ,vars . ,(map (lambda (exp) (map-exp exp f)) exps))))
    (`(conde . ,exps*) (f `(conde . ,(map (lambda (exps) (map (lambda (exp) (map-exp exp f)) exps)) exps*))))
    (`(,rel . ,args) (f `(,rel . ,args)))))

(define (extract-common-condes exp)
  (map-exp exp
           (lambda (exp)
             (match exp
               (`(fresh ,vars . ,exps)