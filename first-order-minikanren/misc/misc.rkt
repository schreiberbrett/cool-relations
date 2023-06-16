#lang typed/racket

(provide
    any
    all
    append-map
    map*
    zip
    lookup)

(: any (All (a) (-> (-> a Boolean) (Listof a) Boolean)))
(define (any p? l)
    (match l
        ('() #f)
        (`(,h . ,t) (or (p? h) (any p? t)))))

(: all (All (a) (-> (-> a Boolean) (Listof a) Boolean)))
(define (all p? l)
    (match l
        ('() #t)
        (`(,h . ,t) (and (p? h) (all p? t)))))

(: append-map (All (a b) (-> (-> a (Listof b)) (Listof a) (Listof b))))
(define (append-map f l)
    (match l
        ('() '())
        (`(,h . ,t) (append (f h) (append-map f t)))))

(: map* (All (a b) (->
    (-> a b)
    (Listof (Listof a))
    (Listof (Listof b)))))
(define (map* f list-of-lists)
    (map (lambda ([l : (Listof a)]) (map f l)) list-of-lists))

;; (List a) -> (List b) -> (List (Pair a b))
;; This fails its pattern match when the lists are different lengths.
;; Only use this when you are sure both lists are the same length!
(: zip (All (a b) (-> (Listof a) (Listof b) (Listof (Pairof a b)))))
(define (zip l r)
    (match `(,l ,r)
        ('(() ()) '())
        (`((,lh . ,lt) (,rh . ,rt)) `((,lh . ,rh) . ,(zip lt rt)))))

;; Key x (List (Pair Key Value)) -> Maybe Value
(: lookup (All (k v) (-> k (Listof (Pairof k v)) (Option v))))
(define (lookup x l)
    (match l
        (`((,h1 . ,h2) . ,t) (if (equal? x h1) h2  (lookup x t)))
        ('() #f)))
