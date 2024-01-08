#lang scribble/manual

@(require scribble-math)
@(require scribble-math/dollar)

@title[#:date "2023-12-30"]{A @racket[conde] with two recursive calls}
@author{Brett Schreiber}

Consider two miniKanren relations, @racket[even-lengtho] and @racket[odd-lengtho].

@racketblock[(defrel (even-lengtho e)
               (conde ((== e '()))
                      ((fresh (a b c)
                              (== e `(,a ,b . ,c))
                              (even-lengtho c)))))

             (defrel (odd-lengtho o)
               (conde ((fresh (i) (== o `(,i))))
                      ((fresh (x y z)
                              (== o `(,x ,y . ,z))
                              (odd-lengtho z)))))]

They produce correct results.

@racketblock[> (run 5 (l) (even-lengtho l))
             '((())
               ((_0 _1))
               ((_0 _1 _2 _3))
               ((_0 _1 _2 _3 _4 _5))
               ((_0 _1 _2 _3 _4 _5 _6 _7)))
             > (run 5 (l) (odd-lengtho l))
             '(((_0))
               ((_0 _1 _2))
               ((_0 _1 _2 _3 _4))
               ((_0 _1 _2 _3 _4 _5 _6))
               ((_0 _1 _2 _3 _4 _5 _6 _7 _8)))]


Here is a contrived example.
@racketblock[> (run 9 (x l) (conde ((== x 'steak) (even-lengtho l))
                                   ((== x 'salad) (odd-lengtho l))))
             '((steak ())
               (salad (_0))
               (steak (_0 _1))
               (salad (_0 _1 _2))
               (steak (_0 _1 _2 _3))
               (salad (_0 _1 _2 _3 _4))
               (steak (_0 _1 _2 _3 _4 _5))
               (salad (_0 _1 _2 _3 _4 _5 _6))
               (steak (_0 _1 _2 _3 _4 _5 _6 _7)))]


Ok, now to ask a dumb question. Is it possible for @racket[x] to be @racket['steak] but for the list to be odd?

@racketblock[> (run 1 (q) (fresh (x l)
                                 (conde ((== x 'steak) (even-lengtho l))
                                        ((== x 'salad) (odd-lengtho l)))
                                 (== x 'steak)
                                 (odd-lengtho l)))
               ...]

The above query runs forever without ever telling us !no", even though the contradiction is evident. Maybe rearranging the query would help.

@racketblock[> (run 1 (q) (fresh (x l)
                                 (== x 'steak)
                                 (conde ((== x 'steak) (even-lengtho l))
                                        ((== x 'salad) (odd-lengtho l)))
                                 (odd-lengtho l)))
               ...]

Still diverges.

@section{A solution}

Suppose I was only I was restricted to use @racket[defrel] only once in my program. I could still have the behavior of two separate @racket[even-lengtho] and @racket[odd-lengtho] relations as follows:

@margin-note{I like to use @racket[ទo] to represent a throwaway relation. It comes from the first letter of the Khmer word @hyperlink["https://en.wiktionary.org/wiki/ទំនាក់ទំនង"]{ទំនាក់ទំនង}, meaning "relation". It is easy when handwriting relations.}

@itemlist[@item{Define a two-place relation. Here I use @racket[(defrel (ទo sym x) ...)]. @racket[sym] will be one of @racket['even-length] or @racket['odd-length].}
          @item{Create a @racket[conde] clause to dispatch on @racket[sym]. The body of @racket[even-lengtho] and @racket[odd-lengtho] can go in here.}
          @item{Replace an calls to @racket[(even-lengtho ...)] with a call to @racket[(ទo 'even-length ...)]. Similarly for @racket[odd-lengtho].}]

@racketblock[(defrel (ទo sym arg)
               (conde ((== sym 'even-length)
                       (fresh (e)
                              (== arg e)
                              (conde ((== e '()))
                                     ((fresh (a b c)
                                             (== e `(,a ,b . ,c))
                                             (ទo 'even-length c))))))

                      ((== sym 'odd-length)
                       (fresh (o)
                              (== arg o)
                              (conde ((fresh (i) (== o `(,i))))
                                     ((fresh (x y z)
                                             (== o `(,x ,y . ,z))
                                             (ទo 'odd-length z))))))))]
                       
Same results for the first 3 tests.

@racketblock[> (run 5 (l) (ទo 'even-length l))
             '((())
               ((_0 _1))
               ((_0 _1 _2 _3))
               ((_0 _1 _2 _3 _4 _5))
               ((_0 _1 _2 _3 _4 _5 _6 _7)))
             > (run 5 (l) (ទo 'odd-length l))
             '(((_0))
               ((_0 _1 _2))
               ((_0 _1 _2 _3 _4))
               ((_0 _1 _2 _3 _4 _5 _6))
               ((_0 _1 _2 _3 _4 _5 _6 _7 _8)))

             > (run 9 (x l) (conde ((== x 'steak) (ទo 'even-length l))
                                   ((== x 'salad) (ទo 'odd-length l))))
             '((steak ())
               (salad (_0))
               (steak (_0 _1))
               (salad (_0 _1 _2))
               (steak (_0 _1 _2 _3))
               (salad (_0 _1 _2 _3 _4))
               (steak (_0 _1 _2 _3 _4 _5))
               (salad (_0 _1 _2 _3 _4 _5 _6))
               (steak (_0 _1 _2 _3 _4 _5 _6 _7)))]

Let's see how it does for the dumb question.

@racketblock[> (run 1 (q) (fresh (x l)
                                 (== x 'steak)
                                 (conde ((== x 'steak)  (ទo 'even-length l))
                                        ((== x 'salad) (ទo 'odd-length l)))
                                 (ទo 'odd-length l)))
             ...]

Still diverges. But look! Now we have two calls the same relation that differ only by their first arguments. By introducing a fresh variable @racket[α], meaning "argument", the call to @racket[ទo] can be factored out. Does that change anything?

@racketblock[> (run 1 (q) (fresh (x l α)
                                 (== x 'steak)
                                 (conde ((== x 'steak) (== α 'even-length))
                                        ((== x 'salad) (== α 'odd-length)))
                                 (ទo α l)
                                 (ទo 'odd-length l)))
             ...]

Not yet. Let's do some program transformations.

First, bubble up all @racket[fresh] declarations into one. Here I can do it unconditionally, because there are no fresh variables with the same name. In general, an algorithm doing this step would have to avoid naming collisions by using something like  @racket[(gensym)].

@racketblock[(defrel (ទo sym arg)
               (fresh (e a b c o i x y z)
                      (conde ((== sym 'even-length)
                              (== arg e)
                              (conde ((== e '()))
                                     ((== e `(,a ,b . ,c))
                                      (ទo 'even-length c)))
                      
                              ((== sym 'odd-length)
                               (== arg o)
                               (conde ((== o `(,i)))
                                      ((== o `(,x ,y . ,z))
                                       (ទo 'odd-length z))))))))]

Next, notice that @racket[e] and @racket[o] are unnecessary, since they unify with @racket[arg] in the clauses which they appear. They can be replaced with @racket[arg] everywhere.

@racketblock[(defrel (ទo sym arg)
               (fresh (a b c i x y z)
                      (conde ((== sym 'even-length)
                              (conde ((== arg '()))
                                     ((== arg `(,a ,b . ,c))
                                      (ទo 'even-length c)))
                      
                              ((== sym 'odd-length)
                               (conde ((== arg `(,i)))
                                      ((== arg `(,x ,y . ,z))
                                       (ទo 'odd-length z))))))))]

Next, distribute down @racket[(== sym 'even-length)] into its two @racket[conde] branches. Do the same for @racket[(== sym 'odd-length)]. This will decrease the "circuit depth" since disjunction is associative.  The following logical inference demonstrates this:

@$${\frac
 {(l \land (m \lor n)) \lor (o \land (p \lor q))}
 {(l \land m) \lor (l \land n) \lor (o \land p) \lor (o \land q)}}


In miniKanren, this allows there to only be one layer of @racket[conde] clauses.

@racketblock[(defrel (ទo sym arg)
               (fresh (a b c i x y z)
                      (conde ((== sym 'even-length)
                              (== arg '()))
                             
                             ((== sym 'even-length)
                              (== arg `(,a ,b . ,c))
                              (ទo 'even-length c))
                             
                             ((== sym 'odd-length)
                              (== arg `(,i)))
                             
                             ((== sym 'odd-length)
                              (== arg `(,x ,y . ,z))
                              (ទo 'odd-length z)))))]

Notice that @racket[(== sym 'even-length)] gets repeated now, as does @racket[(== sym 'odd-length)]. That's okay! Unification where one of the arguments is a ground symbol, like here, is nearly free in miniKanren. However, the two recursive calls to @racket[ទo] are not. Thankfully, the recursion can be factored out as before. Notice that we could say @racket[sym] instead of hardcoding @racket['even-length] and @racket['odd-length] in the recursive calls.

@racketblock[(defrel (ទo sym arg)
               (fresh (a b c i x y z)
                      (conde ((== sym 'even-length)
                              (== arg '()))

                             ((== sym 'odd-length)
                              (== arg `(,i)))
                             
                             ((== sym 'even-length)
                              (== arg `(,a ,b . ,c))
                              (ទo sym c))
                             
                             ((== sym 'odd-length)
                              (== arg `(,x ,y . ,z))
                              (ទo sym z)))))]

This next part is subtle. The last two clauses almost say the same thing. They are both clauses where @racket[sym] is either @racket['even-length] or @racket['odd-length], and @racket[arg] has at least two elements, then make a recursive call to everything othre than the first two elements of @racket[arg]. The difficulty here is to notice that the fresh variables @racket[a], @racket[b], @racket[c] and @racket[x], @racket[y], @racket[z] perform the same role and remain fresh, so they are interchangeable.

Here is a demonstration of this in first order logic:

@$${\frac
 {(x \land a \land b) \lor (y \land a \land b)}
 {(x \lor y) \land a \land b}}

Here is the result of that transformation.

@racketblock[(defrel (ទo sym arg)
               (fresh (a b c i)
                      (conde ((== sym 'even-length)
                              (== arg '()))

                             ((== sym 'odd-length)
                              (== arg `(,i)))
                             
                             ((conde ((== sym 'even-length)) ((== sym 'odd-length)))
                              (== arg `(,a ,b . ,c))
                              (ទo sym c)))))]

Finally, we can observe that @racket[(conde ((== sym 'even-length)) ((== sym 'odd-length)))] is always true, because it eventually gets ground to @racket['even-length] or @racket['odd-length] once the recursion reaches a base case. It can be removed because it's a tautology. It should be removed because leaving variables fresh is often beneficial for halting.

I'm not sure if it's possible for an algorithm to recognize the above transformation, but here is the code after applying it.

@racketblock[(defrel (ទo sym arg)
               (fresh (a b c i)
                      (conde ((== sym 'even-length)
                              (== arg '()))

                             ((== sym 'odd-length)
                              (== arg `(,i)))
                             
                             ((== arg `(,a ,b . ,c))
                              (ទo sym c)))))]

So, how does this fare with the query?