#lang scribble/manual

@(require scribble-math)
@(require scribble-math/dollar)

@title[#:date "2023-09-26"]{miniKanren and Multivariate Horner Schemes}
@author{Brett Schreiber}


Suppose we had the following function that you wanted to evaluate on certain inputs.

@($$ "f(x, y, z) = x^2yz + xy + y^2z + yz")

This type of function is called a multivariate polynomial, because it is a sum of products over multiple variables. Here is a version of @${f} with explicit multiplication.

@$${f(x, y, z) = x*x*y*z + x*y + y*y*z + y*z}

There are 7 multiplications and 3 additions. Here is a version with one @${y} factored out of each of the products (hereafter called monomials).

@$${f(x, y, z) = y(x^2z + x + yz + z)}

Now @${z} is common among 3 of the inner monomials. It can be factored out without @${x}.

@$${f(x, y, z) = y(z(x^2 + y + 1) + x) = y*(z*(x*x + y + 1) + x)}

This version has 3 multiplications and the same 3 additions. In general, factoring out a variable decreases the number of multiplications, but preserves the number of additions.

In general, you can reduce the number of multiplications in a multivariate polynomial by pulling out common factors. This method of reducing multipliactions in a multivariate polynomial is called the Multivariate Horner Scheme. Here is a greedy algorithm that 

@itemlist[#:style 'ordered
          @item{Find the factor that appears in the most monomials. If none exists, halt. If more than one exist, use the first. Let @${x} be that factor.}
          @item{Let @${M} be the monomials @${x} appears in and @${N} be the ones it does not. Let @${O} be the monomial resulting from factoring out @${x} from @${M}.}]

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