#lang scribble/manual

@(require scribble-math)
@(require scribble-math/dollar)

@title[#:date "2024-01-04"]{A technique to minimize recursive calls in miniKanren}
@author{Brett Schreiber}

Suppose I had two relations @racket[po] and @racket[qo] Consider the following code snippet:

@racketblock[(run 100 (x y)
                  (conde ((po x) (po y))
                         ((po x) (qo y))
                         ((qo x) (po y))
                         ((qo x) (qo y))))]


This seems wasteful. For example, @racket[(po x)] gets calculated twice, once in the first @racket[conde] clause, and once in the second. In fact, each invocation of @racket[po] and @racket[qo] on the variables @racket[x] and @racket[y] happens twice. Remember that @racket[conde] clauses are not like @racket[cond] statements: other @racket[conde] clauses may still get executed, even if the first clause succeeds!

Thankfully, there is a way to avoid recomputation. Notice that every combination of @racket[po], @racket[qo], @racket[x] and @racket[y] is represented:

@racketblock[(run 100 (x y)
                  (conde ((po x))
                         ((qo x)))
                  (conde ((po y))
                         ((qo y))))]

Crisis averted! This is closely related to factoring in algebra:

@$${p^2 + 2pq + q^2 = (p + q)(p + q)}

But now imagine a different situation, where there is no longer the case @racket[(qo x) (po y)].

@racketblock[(run 100 (x y)
                  (conde ((po x) (po y))
                         ((po x) (qo y))
                         ((qo x) (qo y))))]

I can somewhat do the same algebraic factoring, if I include a subtraction.

@$${p^2 + pq + q^2 = (p + q)(p + q) - pq}

This gives me a hint about how to express a factored version in miniKanren. Essentially, the subtraction in the algebra corresponds to stating that @racket[(qo x) (po y)] is false. This can be achieved by introducing a symbol @racket[s] which tracks the cases.

@racketblock[(run 100 (x y)
                  (fresh (s)
                         (conde ((== s 'pp))
                                ((== s 'pq))
                                ((== s 'qq)))

                         (conde ((conde ((== s 'pp)) ((== s 'pq))) (po x))
                                ((== s 'qq) (qo x)))
                         
                         (conde ((== s 'pp) (po y))
                                ((conde ((== s 'pq)) ((== s 'qq))) (qo y)))))]

Therefore, unlike in regular algebra, we can exploit idempotence when optimizing miniKanren programs:

@$${x + x = x}

This optimization should be used as a last resort. Ideally, there would more structure known about @racket[po] and @racket[qo] which would lead to a clever way to express their conjunction more efficiently.

@section{Benchmarks}

For the benchmarks, I will use the relations @racket[all-appleo] and @racket[all-bananao], implemented below.

@racketblock[(defrel (all-appleo l)
               (conde ((== l '()))
                      ((fresh (d)
                              (== l `(apple . ,d))
                              (all-appleo d)))))

             (defrel (all-bananao l)
               (conde ((== l '()))
                      ((fresh (d)
                              (== l `(banana . ,d))
                              (all-bananao d)))))]

And here is a rough benchmark

@racketblock[(time (run 15 (x y)
                        (conde ((all-appleo x) (all-appleo y))
                               ((all-appleo x) (all-bananao y))
                               ((all-bananao x) (all-appleo y))
                               ((all-bananao x) (all-bananao y))))
                   (void))]