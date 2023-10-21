#lang scribble/manual

@(require scribble-math)
@(require scribble-math/dollar)

@title[#:date "2023-09-26"]{miniKanren and Multivariate Horner Schemes}
@author{Brett Schreiber}

@bold{Bill chops doors in inns.} This is a nonsense sentence, but it has the interesting property that all its words are in dictionary order, and all the letters within each word are in alphabetical order. So it is possible to compress the sentence into a smaller representation by treating the sentence like a sum of products and factoring out common variables:

@($$ "bill + chops + doors + in + inns = i(bll + n(1 + ns)) + os(chp + dor)")

There is a second way to factor out common variables:

@($$ "= i(bll + n) + s(o(chp + dor) + inn)")



@url{https://scholarworks.utep.edu/cgi/viewcontent.cgi?article=1402&context=cs_techrep}

@($$ "f(x_1, \\ldots, x_n) = \\sum_k M_k")

@($$ "f(x_1, \\ldots, x_n) = x_i \\cdot \\left(\\sum_{k:x_i \\in M_k} M_k^\\prime \\right) + \\sum_{k:x_i \\notin M_k} M_k")

Recall the Racket function @racket[rember], which removes the first matching element @racket[x] from a list @racket[l]:

@racketblock[(define (rember x l)
               (cond
                 ((null? l) '())
                 ((equal? x (car l)) (cdr l))
                 (else (cons (car l) (rember x (cdr l))))))]

Now I can translate the formula into Racket, which I will call @racket[horner-step].

@racketblock[(define (horner-step x M)
               (let* ((Mᵢ (filter (λ (Mₖ) (member x Mₖ)) M))
                      (Mₒ (filter (λ (Mₖ) (not (member x Mₖ))) M))
                      (Mᵣ (map (λ (Mₖ) (rember x Mₖ)) Mᵢ)))
                 `(+ (* ,x (+ . ,Mᵣ))
                     (+ . ,Mₒ))))]

Suppose we had a function that, given a multivariate polynomial, returned all the variables within.

@racketblock[(define (all-variables P)
               (match P
                 ('() '())
                 (`(() . ,t) (all-variables t))
                 (`((,a . ,d) . ,t) (let ((rec (all-variables `(,d . ,t))))
                                      (if (member a rec) rec `(,a . ,rec))))))]

@racketblock[(define (most-frequent-variable P)
               (max-by
                (lambda (var) (length (filter (lambda (M)
                                                (member var M)) P)))
                (all-variables P)))]



@racketblock[(define (max-by f l)
               (match l
                 ('() #f)
                 (`(,a . ,d) (let ((x (max-by f d)))
                               (if (and x (> (f x) (f a)))
                                   x
                                   a)))))]