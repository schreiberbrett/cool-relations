#lang scribble/manual

@(require scribble-math)
@(require scribble-math/dollar)

@title[#:date "2023-09-26"]{miniKanren and Multivariate Horner Schemes}
@author{Brett Schreiber}

Suppose we had a multivariate polynomial that we wanted to optimize to use as few operations as possible. Here is an example which starts with 6 multiplications and 5 additions, gets transformed by a greedy algorithm, and ends with 2 multiplications and 5 additions.

@tabular[#:sep @hspace[1]
         (list (list @${x^2 + xy + xz + xw + yw + zw}
                     "")
               (list @${= x(x + y + z + w) + yw + zw}
                     @elem{Factor out @${x} from 4 of the monomials.})
               (list @${= x(x + y + z + w) + w(y + z)}
                     @elem{Factor out @${w} from 2 of the monomials.}))]

This transformation results in a @bold{multivariate Horner scheme}. Although useful in reducing multiplications, multivariate Horner schemes are not optimal. Here is a different transformation which results in 1 multiplication and 3 additions.

@tabular[#:sep @hspace[1]
         (list (list @${x^2 + xy + xz + xw + yw + zw})
               (list @${= (x + y + z)(x + w)}))]

@section{The greedy algorithm}
@hyperlink["https://scholarworks.utep.edu/cgi/viewcontent.cgi?article=1402&context=cs_techrep"]{Ceberio and Kreinovich (2003)} describe a greedy algorithm for producing a multivariate Horner scheme, given a multivariate polynomial:

@itemlist[@item{Find a variable that appears in the most monomials. Halt if no variable appears more than once.}
          @item{Factor out that variable from the monomials it appears in.}
          @item{Recur twice: once on the newly-factored monomials, and once on the monomials which were left alone.}]


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