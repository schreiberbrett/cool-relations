#lang scribble/lp2

@title{Cool Relations}

@chunk[<*>
       <load>
       <nonemptyo>
       <appendo>
       <riffleo>
       <picko>
       <appendo*>
       <nonempty-appendo*>]

@section{Code from The Reasoned Schemer 2nd Edition}
@chunk[<load>
       ; (require scribble-math)
       ; (require scribble-math/dollar)
       (require racket/include)
       (include "../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")]

@chunk[<appendo>
       (defrel (appendo l r o)
         (conde
          ((== l '()) (== r o))
          ((fresh (h t rec)
                  (== l `(,h . ,t))
                  (== o `(,h . ,rec))
                  (appendo t r rec)))))]

@section{Asserting that a list is non-empty}
@chunk[<nonemptyo>
       (defrel (nonemptyo l)
         (fresh (h t)
                (== l `(,h . ,t))))]

@section{The Riffle Relation}

Elsewhere, this relation is sometimes called `shuffle`

@chunk[<riffleo>
       (defrel (riffleo l r o)
         (fresh (h t rec)
                (conde
                 ((== l '()) (== r '()) (== o '()))
                 ((== l `(,h . ,t)) (== o `(,h . ,rec)) (riffleo t r rec))
                 ((== r `(,h . ,t)) (== o `(,h . ,rec)) (riffleo l t rec)))))]


If we consider @racket[riffle] to be the nondeterministic functional version of @racket[riffleo], and @racket[append] to be the deterministic functional version of append, then we have some striking similarities.

@racket[append] and @racket[riffle] are both linear (they use each element from each input list exactly once in the output).

@racketblock[(forall (a b)
                (= (+ (length a) (length b)) (length (append a b)))
                (= (+ (length a) (length b)) (length (riffle a b))))]


@racket[append] and @racket[riffle] are both associative:

@racketblock[(forall (a b c)
                (=
                 (append a (append b c))
                 (append (append a b) c)
                 (append a b c))
                
                (=
                 (riffle a (riffle b c))
                 (riffle (riffle a b) c)
                 (riffle a b c)))]


@racket[riffle] and @racket[append] also share the property that, if the output list is both sorted, then both of the inputs must have been sorted as well. In general, order within the sublists is preserved.
@racketblock[(forall (a b)
                     (-> (or
                          (sorted? (append a b))
                          (sorted? (riffle a b)))

                         (and
                          (sorted? a)
                          (sorted? b))))]


However, there is one property of @racket[riffleo] not shared by @racket[appendo]: the act of riffling is commutative. When a dealer shuffles cards, it doesn't matter which deck is in his right hand, and which is in his left.
@racketblock[(forall (a b)
                     (==
                      (run* (x) (riffleo a b x))
                      (run* (x) (riffleo b a x))))]




Suppose @racket[l] is a finite list. How many results does @racketblock[(run* (x1 x2 x3 rest) (riffleo `(,x1 ,x2 ,x3) rest l))] produce? This is like  nondeterministically choosing three elements from a list in a fixed order. So the exact number of results is known:

@racketblock[(forall (l)
                     (=
                      (length (run* (x1 x2 x3 rest) (riffleo `(,x1 ,x2 ,x3) rest l)))
                      (choose (length l) 3)))]

Where @racket[(choose n k)] is mathematical combination. This example is equivalent to deterministically iterating over the list with three loops in cubic time. In general:

@racketblock[(forall (l)
                     (=
                      (length (run* (chosen not-chosen) (riffleo chosen not-chosen l)))
                      (choose (length l) (length chosen))
                      (choose (length l) (length not-chosen))))]



@subsection{Picking unordered}
Sometimes you want to explicitly pick items from an unordered list. For that, you can use @racket[picko], which is defined in terms of @racket[riffleo].

@chunk[<picko>
       (defrel (pick-oneo x not-picked l)
         (riffleo `(,x) not-picked l))

       (defrel (picko picked not-picked l)
         (conde
          ((== picked '()) (== not-picked l))
          ((fresh (first not-first rest)
                  (== picked `(,first . ,rest))
                  (pick-oneo first not-first l)
                  (picko rest not-picked not-first)))))]

@section{Appending @racket[n] lists together}

Note that, in this naive definition, there could be an infinite amount of empty lists.

@chunk[<appendo*>
       (defrel (appendo* l o)
         (conde
          ((== l '()) (== o '()))
          ((fresh (h t rec)
                  (== l `(,h . ,t))
                  (appendo h rec o)
                  (appendo* t rec)))))]


So we need a new version which only allows nonempty lists to be appended together.
@chunk[<nonempty-appendo*>
       (defrel (nonempty-appendo* l o)
         (conde
          ((== l '()) (== o '()))
          ((fresh (h t rec)
                  (== l `(,h . ,t))
                  (nonemptyo h)
                  (appendo h rec o)
                  (nonempty-appendo* t rec)))))]

Given some finite list @racket[l], how many results does @racket[(run* (x) (nonempty-appendo* x l))] produce?

Starting with an easy one, say, a list with 4 elements.
@racketblock[> (run* (x) (nonempty-appendo* x '(e1 e2 e3 e4)))]
@racketblock['((((e1   e2   e3   e4)))
               (((e1) (e2   e3   e4)))
               (((e1   e2) (e3   e4)))
               (((e1) (e2) (e3   e4)))
               (((e1   e2   e3) (e4)))
               (((e1) (e2   e3) (e4)))
               (((e1   e2) (e3) (e4)))
               (((e1) (e2) (e3) (e4))))]


A few tests for different values of @racket[l]:
@racketblock[> (map
                (lambda (l)
                  (length (run* (x) (nonempty-appendo* x l))))
                '(()
                  (a)
                  (a a)
                  (a a a)
                  (a a a a)
                  (a a a a a)
                  (a a a a a a)))]
@racketblock['(1 1 2 4 8 16 32)]

These are powers of two. Let's see how this can be formally proved.


@racketblock[(forall (l)
                      (=
                       (length (run* (x) (nonempty-appendo* x l)))
                       (expt 2 (length l))))]


@section{Actor Association Game}
The actor association game, known also as "The Six Degrees of Kevin Bacon" is a way to get from one actor to another by listing a chain of movie collaborations.
@chunk[<actor-association-game>
       (defrel (bacono movies x y proof)
         (fresh (movie actors rest-movies ^1)
                (picko `((,movie . ,actors)) rest-movies movies)
                (conde
                 ((== proof `((,movie ,x ,y)))
                  (picko `(,x ,y) ^1 actors))
        
                 ((fresh (next rest-proof)
                         (== proof `((,movie ,x ,next) . ,rest-proof))
                         (picko `(,x ,next) ^1 actors)
                         (bacono rest-movies next y rest-proof))))))


       (define all-movies '((glass-onion           daniel-craig edward-norton)
                            (avengers              robert-downey-jr scarlett-johannson)
                            (casino-royale         daniel-craig eva-green)
                            (spider-man-homecoming tom-holland robert-downey-jr)
                            (uncharted             tom-holland mark-wahlberg)))]
         