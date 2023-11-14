#lang scribble/manual


@title[#:date "2023-11-03"]{The Majority Relation}
@author{Brett Schreiber}

Consider the list @racket['(a b a c a)]. Here, @racket['a] is the majority element. Now consider the list @racket['(x x y z)]. Although @racket[x] is the most frequent element, it is not the majority. Notice that a list can have zero or one majority elements.

Let's define a relation @racket[majorityo] in miniKanren. It takes an element @racket[e] and a list @racket[l] and asserts that @racket[e] is the majority element in @racket[l]. For example, the following miniKanren statement should succeed. @racket[(majorityo 'x '(a x b x a x x c x))]. I always like to start by drawing a picture and adding annotations.

@image["img/majority.png"]

A few takeaways:
@itemlist[@item{@racket[l] can be described as a disjoint union, or riffle, of two lists @racket[l₁] and @racket[l₂].}
          @item{@racket[l₁] is longer than @racket[l₂], that is, it has a greater length.}
          @item{@racket[l₁] consists entirely of @racket[e]s. It is @racket[e], repeated.}]

Writing this out in miniKanren:

@racketblock[(defrel (majorityo e l)
               (fresh (l₁ l₂ l₁⊔l₂)
                      (== l l₁⊔l₂)
                      (repetitiono e l₁)
                      (longero l₁ l₂)
                      (riffleo l₁ l₂ l₁⊔l₂)))]

I'll define the helper relations using the same names:



