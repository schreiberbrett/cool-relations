#lang racket

(require racket/match)

(provide
    any
    all
    append-map
    map*
    lookup)

(define (any p? l)
    (match l
        ('() #f)
        (`(,h . ,t) (or (p? h) (any p? t)))))

(define (all p? l)
    (match l
        ('() #t)
        (`(,h . ,t) (and (p? h) (all p? t)))))

(define (append-map f l)
    (match l
        ('() '())
        (`(,h . ,t) (append (f h) (append-map f t)))))

;; (a -> b) -> (List (List a)) -> (List (List b))
(define (map* f list-of-lists)
    (map (lambda (l) (map f l)) list-of-lists)) 

;; (List a) -> (List b) -> (List (Pair a b))
;; This fails its pattern match when the lists are different lengths.
;; Only use this when you are sure both lists are the same length!
(define (zip l r)
    (match `(,l ,r)
        ('(() ()) '())
        (`((,lh . ,lt) (,rh . ,rt)) `((,lh . ,rh) . ,(zip lt rt)))))

;; Key x (List (Pair Key Value)) -> Maybe Value
(define (lookup x l)
    (match l
        (`((,h1 . ,h2) . ,t) (if (equal? x h1) `(just ,h2) (lookup x t)))
        ('() '(nothing))))
