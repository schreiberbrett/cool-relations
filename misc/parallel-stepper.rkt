#lang racket

(require pict/code
         pict)

(define-syntax emph
  (make-code-transformer
   (syntax-rules ()
     [(_ x)
      (let ()
        (define the-code (code x))
        (define width (pict-width the-code))
        (define height (pict-height the-code))
        (lbl-superimpose
         (colorize (filled-rectangle width height) "green")
         the-code))])))


#;(code
   (this highlights the (emph (+ 1 2 3)) and (emph second) arg but not the last))

; 
(define (zip-with-substitute l1 sub1 l2 sub2 f)
  (match `(,l1 ,l2)
    ('(() ()) '())
    (`((,a1 . ,d1) ()) (cons (f a1 sub2) (zip-with-substitute d1 sub1 '() sub2 f)))
    (`(() (,a2 . ,d2)) (cons (f sub1 a2) (zip-with-substitute '() sub1 d2 sub2 f)))
    (`((,a1 . ,d1) (,a2 . ,d2)) (cons (f a1 a2) (zip-with-substitute d1 sub1 d2 sub2 f)))))

; a stepper is a pair: a list of sexps and a pointer to its last element.

(define (quicksort-stepper l)
  (match l
    ('() '(((quicksort ())
            ())
           
           ()))

    (`(,x) `(((quicksort (list ,x))
              (list ,x))

             (,x)))
    
    (`(,pivot . ,rest) (match-let* ((lt (filter (lambda (x) (< x pivot)) rest))
                                    (geq (filter (lambda (x) (>= x pivot)) rest))

                                    (`(,ltsteps ,ltlast) (quicksort-stepper lt))

                                    (`(,geqsteps ,geqlast) (quicksort-stepper geq))

                                    (mylast (append ltlast (cons pivot geqlast)))

                                    (result `(,(append* `(((quicksort (cons (emph ,pivot) ,rest))
                                                           (append (quicksort ,lt) (cons ,pivot (quicksort ,geq))))
                                  
                                                          ,(zip-with-substitute ltsteps ltlast geqsteps geqlast
                                                                                (lambda (ltstep geqstep)
                                                                                  `(append ,ltstep (cons ,pivot ,geqstep))))

                                                          (,mylast)))

                                              ,mylast)))

                         #;(displayln result)
                         result))))

                                    
(quicksort-stepper '(2 1 3 4))

(code ((quicksort (cons (emph 2) (1 3 4)))
       (append (quicksort (1)) (cons 2 (quicksort (3 4))))
       (append (quicksort (list 1)) (cons 2 (quicksort (cons (emph 3) (4)))))
       (append (list 1) (cons 2 (append (quicksort ()) (cons 3 (quicksort (4))))))
       (append (1) (cons 2 (append (quicksort ()) (cons 3 (quicksort (list 4))))))
       (append (1) (cons 2 (append () (cons 3 (list 4)))))
       (append (1) (cons 2 (3 4)))
       (1 2 3 4)))
                           