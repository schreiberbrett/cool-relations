#lang typed/racket

(provide append-map map* cartesian-product-with)

(: append-map (All (a b) (-> (-> a (Listof b)) (Listof a) (Listof b))))
(define (append-map f l)
    (match l
        ('() '())
        (`(,h . ,t) (append (f h) (append-map f t)))))

(: map* (All (a b) (-> (-> a b) (Listof (Listof a)) (Listof (Listof b)))))
(define (map* f list-of-lists)
    (map (lambda ([l : (Listof a)]) (map f l)) list-of-lists))


(: cartesian-product-with (All (a b c) (-> (-> a b c) (Listof a) (Listof b) (Listof c))))
(define (cartesian-product-with f x y)
  (append-map (lambda ([x1 : a])
                (append-map (lambda ([y1 : b])
                              `(,(f x1 y1)))
                            y))
              x))