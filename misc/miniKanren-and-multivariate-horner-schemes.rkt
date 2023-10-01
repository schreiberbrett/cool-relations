#lang racket

(define (rember x l)
  (cond
    ((null? l) '())
    ((equal? x (car l)) (cdr l))
    (else (cons (car l) (rember x (cdr l))))))

(define (horner-step x M)
  (let* ((Mᵢ (filter (λ (Mₖ) (member x Mₖ)) M))
         (Mₒ (filter (λ (Mₖ) (not (member x Mₖ))) M))
         (Mᵣ (map (λ (Mₖ) (rember x Mₖ)) Mᵢ)))
    `(+ (* ,x (+ . ,Mᵣ))
        (+ . ,Mₒ))))

(define (all-variables P)
  (match P
    ('() '())
    (`(() . ,t) (all-variables t))
    (`((,a . ,d) . ,t) (let ((rec (all-variables `(,d . ,t))))
                         (if (member a rec) rec `(,a . ,rec))))))

(define (most-frequent-variable P)
  (let* ((P (filter (lambda (M) (not (null? M))) P))
         (vars (all-variables P)))
    (max-by (lambda (var) (length (filter (lambda (M) (member var M)) P))) vars)))

(define (max-by f l)
  (match l
    ('() #f)
    (`(,a . ,d) (let ((x (max-by f d)))
                  (if (and x (> (f x) (f a)))
                      x
                      a)))))