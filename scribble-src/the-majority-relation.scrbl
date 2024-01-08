#lang scribble/manual


@title[#:date "2023-11-03"]{The Majority Relation}
@author{Brett Schreiber}

Consider the list @racket['(a b a c a)]. Here, @racket['a] is the majority element. Now consider the list @racket['(x x y z)]. Although @racket[x] is the most frequent element, it is not the majority. Notice that a list can have zero or one majority elements.

Let's define a relation @racket[(majorityo e l)] in miniKanren.

Given:
@itemlist[@item{A value @racket[e]}
          @item{A list @racket[l]}]
Asserts:
@itemlist[@item{Most of the elements in @racket[l] are equal to @racket[e]}]

Example:
@racketblock[(majorityo 'x '(a x b x a x x c x))]

I always like to start by drawing a picture and adding annotations.

@image["img/majority.png"]

A few takeaways:
@itemlist[@item{@racket[l] is a disjoint union, or riffle, of two lists @racket[l₁] and @racket[l₂].}
          @item{@racket[l₁] is longer than @racket[l₂].}
          @item{@racket[l₁] is @racket[e], repeated.}]

Here is my first attempt: @racket[majorityo-naive].

@racketblock[(defrel (majorityo-naive e l)
               (fresh (l₁ l₂ l₁⊔l₂)
                      (== l l₁⊔l₂)
                      (==* e l₁)
                      (longero l₁ l₂)
                      (riffleo l₁ l₂ l₁⊔l₂)))]

Corresponding helper relations:

@racketblock[(defrel (==* e l)
               (conde ((== l '()))
                      ((fresh (d)
                              (== l `(,e . ,d))
                              (==* e d)))))

             (defrel (longero l₁ l₂)
               (fresh (a₁ d₁ a₂ d₂)
                      (== l₁ `(,a₁ . ,d₁))
                      (conde ((== l₂ '()))
                             ((== l₂ `(,a₂ . ,d₂))
                              (longero d₁ d₂)))))

             (defrel (riffleo l₁ l₂ l₁⊔l₂)
               (fresh (a₁ d₁ a₂ d₂ d₁⊔l₂ l₁⊔d₂)
                      (conde ((== l₁ '()) (== l₂ '()) (== l₁⊔l₂ '()))
                
                             ((== l₁ `(,a₁ . ,d₁)) (== l₂ '()) (== l₁⊔l₂ l₁))
                             ((== l₁ '()) (== l₂ `(,a₂ . ,d₂)) (== l₁⊔l₂ l₂))

                             ((== l₁ `(,a₁ . ,d₁))
                              (== l₂ `(,a₂ . ,d₂))

                              (fresh (α₁ α₂ α₃)
                                     (conde ((== l₁⊔l₂ `(,a₁ . ,d₁⊔l₂)) (== `(,α₁ ,α₂ ,α₃) `(,d₁ ,l₂ ,d₁⊔l₂)))
                                            ((== l₁⊔l₂ `(,a₂ . ,l₁⊔d₂)) (== `(,α₁ ,α₂ ,α₃) `(,l₁ ,d₂ ,l₁⊔d₂))))
                                     (riffleo α₁ α₂ α₃))))))]
                              
@racket[==*] and @racket[longero] both recur on @racket[l₁] in the definition of @racket[majorityo-naive]. Below is a relation expressing their conjunction.

@margin-note{This technique also leads to the singly-recursive relation @racket[(==*-and-riffleo e l₁ l₂)]. I was not able to do the same for @racket[longero] and @racket[riffleo].}

@racketblock[(defrel (==*-and-longero e l₁ l₂)
               (fresh (d₁)
                      (== l₁ `(,e . ,d₁))
                      (conde ((== l₂ '()))
                             ((== l₂ `(,a₂ . ,d₂))
                              (==*-and-longero e d₁ d₂)))))]

Also, the fresh variable @racket[l₁⊔l₂], in @racket[majorityo-naive] equals @racket[l]. Instead, I can use @racket[l] everywhere.

The below implementation, @racket[majorityo-cpt], applies these findings. The suffix @racket[cpt] indicates that it is a @bold{c}orrectness-@bold{p}reserving @bold{t}ransformation of @racket[majorityo-naive].

@racketblock[(defrel (majorityo-cpt e l)
               (fresh (l₁ l₂)
                      (==*-and-longero e l₁ l₂)
                      (riffleo l₁ l₂ l)))]

@section{A different approach}

I thought about how to express write this as a Racket predicate, @racket[majority?]. I would use a helper function @racket[margin].

@racketblock[(define (majority? x l)
               (positive? (margin x l)))

             (define (margin x l)
               (cond ((null? l) 0)
                     ((equal? (car l) x) (+ 1 (margin x (cdr l))))
                     (else (- 1 (margin x (cdr l))))))]

Turning these functions into relations is difficult, because they involve numbers. Relations that involve numbers have the potential to run forever. There is always a higher number to consider.

Moreover, @racket[margin] returns negative numbers, e.g. @racket[(margin 'a '(b b b))] evaluates to @racket[-3]. So I can't make use of @hyperlink["https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd/blob/master/trs2-arith.scm"]{the arithmetic system from 'The Reasoned Schemer, Second Edition'} because its relations only work on natural numbers, not integers.

So I'll have to come up with my own implementation of integers, but luckily, it only requires 4 relations:
@itemlist[@item{@racket[positiveo]}
          @item{@racket[=0o]}
          @item{@racket[+1o]}
          @item{@racket[-1o]}]

Then the code for @racket[majoritys] would be as follows.

@racketblock[(defrel (majorityo x l)
               (fresh (n)
                      (positiveo n)
                      (deficito x l n)))

             (defrel (margino x l n)
               (conde ((== l '()) (=0o n))
                      ((fresh (a d m)
                              (== l `(,a . ,d))
                              (conde ((== x a) (+1o m n))
                                     ((-1o m n)))
                              (margino x d m)))))]

But @bold{miniKanren is bidirectional}, so @racket[(-1o m n)] can be expressed as @racket[(+1o n m)] instead.

@racketblock[(defrel (margino x l n)
               (conde ((== l '()) (=0o n))
                      ((fresh (a d m)
                              (== l `(,a . ,d))
                              (conde ((== x a) (+1o m n))
                                     ((+1o n m)))
                              (margino x d m)))))]

Since this only needs to support @racket[+1o], @racket[=0o], and @racket[positiveo], I'm thinking about Peano numbers. But I need to somehow incorporate negative and positive numbers. Here is a proposed encoding, which I will call @bold{list integers}, drawn on the number line. I annotated some interesting relationships in green.

@image["img/list-integers.png"]

When dealing with list-integers, zero is @racket['()], positive numbers are nonempty lists of @racket[`+]s, and negative numbers are nonempty lists of @racket['-]s. From this encoding, the definitions of @racket[=0o], @racket[positiveo], and @racket[+1o] are:

@racketblock[(defrel (=0o x)
               (== x '()))

             (defrel (positiveo x)
               (fresh (y)
                      (== x `(+ . ,y))))

             (defrel (+1o x x+1)
               (conde ((== x `(- . ,x+1)))
                      ((== `(+ . ,x) x+1))))]

These relations are @bold{golden!} They never diverge, because they leave parts of their arguments fresh. But that means things like @racket[(positiveo '(+ -))] succeed, even though @racket['(+ -)] is not a valid list integer. Thankfully, I don't have to worry about these false positives, because @racket[majorityo] and @racket[margino] never construct invalid list integers.



