#lang scribble/lp2
@;;(require scribble/lp-include)

@title{Cool Relations}

@section{Code from The Reasoned Schemer 2nd Edition}
@chunk[<load>
       ; (require scribble-math)
       ; (require scribble-math/dollar)
       (require racket/match)
       (require racket/include)
       (include "../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")]

@chunk[<appendo>
       (defrel (appendo l r o)
         (conde ((== l '()) (== r o))
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
                (conde ((== l '()) (== r '()) (== o '()))
                       ((== l `(,h . ,t)) (== o `(,h . ,rec)) (riffleo t r rec))
                       ((== r `(,h . ,t)) (== o `(,h . ,rec)) (riffleo l t rec)))))]


If we consider @racket[riffle] to be the nondeterministic functional version of @racket[riffleo], and @racket[append] to be the deterministic functional version of append, then we have some striking similarities.

@racket[append] and @racket[riffle] are both linear (they use each element from each input list exactly once in the output).

@racketblock[(forall (a b)
                (= (+ (length a) (length b)) (length (append a b)))
                (= (+ (length a) (length b)) (length (riffle a b))))]


@racket[append] and @racket[riffle] are both associative:

@racketblock[(forall (a b c)
                     (= (append a (append b c))
                        (append (append a b) c)
                        (append a b c))
                
                     (= (riffle a (riffle b c))
                        (riffle (riffle a b) c)
                        (riffle a b c)))]


@racket[riffle] and @racket[append] also share the property that, if the output list is both sorted, then both of the inputs must have been sorted as well. In general, order within the sublists is preserved.
@racketblock[(forall (a b)
                     (-> (or (sorted? (append a b))
                             (sorted? (riffle a b)))

                         (and(sorted? a)
                             (sorted? b))))]


However, there is one property of @racket[riffleo] not shared by @racket[appendo]: the act of riffling is commutative. When a dealer shuffles cards, it doesn't matter which deck is in his right hand, and which is in his left.
@racketblock[(forall (a b)
                     (== (run* (x) (riffleo a b x))
                         (run* (x) (riffleo b a x))))]




Suppose @racket[l] is a finite list. How many results does @racketblock[(run* (x1 x2 x3 rest) (riffleo `(,x1 ,x2 ,x3) rest l))] produce? This is like  nondeterministically choosing three elements from a list in a fixed order. So the exact number of results is known:

@racketblock[(forall (l)
                     (= (length (run* (x1 x2 x3 rest) (riffleo `(,x1 ,x2 ,x3) rest l)))
                        (choose (length l) 3)))]

Where @racket[(choose n k)] is mathematical combination. This example is equivalent to deterministically iterating over the list with three loops in cubic time. In general:

@racketblock[(forall (l)
                     (= (length (run* (chosen not-chosen) (riffleo chosen not-chosen l)))
                        (choose (length l) (length chosen))
                        (choose (length l) (length not-chosen))))]



@subsection{Picking unordered}
Sometimes you want to explicitly pick items from an unordered list. For that, you can use @racket[picko], which is defined in terms of @racket[riffleo].

@chunk[<picko>
       (defrel (pick-oneo x not-picked l)
         (riffleo `(,x) not-picked l))

       (defrel (picko picked not-picked l)
         (fresh (first not-first rest)
                (conde ((== picked '()) (== not-picked l))
                       ((== picked `(,first . ,rest))
                        (pick-oneo first not-first l)
                        (picko rest not-picked not-first)))))]

@section{Appending @racket[n] lists together}

Note that, in this naive definition, there could be an infinite amount of empty lists.

@chunk[<appendo*>
       (defrel (appendo* l o)
         (fresh (h t rec)
                (conde ((== l '()) (== o '()))
                       ((== l `(,h . ,t))
                        (appendo h rec o)
                        (appendo* t rec)))))]

So we need a new version which only allows nonempty lists to be appended together.
@chunk[<nonempty-appendo*>
       (defrel (nonempty-appendo* l o)
         (fresh (h t rec)
                (conde ((== l '()) (== o '()))
                       ((== l `(,h . ,t))
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

@subsection{Equivalence of relations and a new version of @racket[nonempty-appendo*]}
Two mathematical relations are considered equal if the set of satisfying tuples is the same. Therefore, two miniKanren relations can be considered equivalent if you ignore duplicates and the order they come back in.

But how does this hold up in the presence of fresh variables? Fresh variables can be used to encode an infinite amount of tuples in the relation. For instance, in @racket[appendo]:

@racketblock[> (run 7 (a b c) (appendo a b c))
             '((() _0 _0)
               ((_0) _1 (_0 . _1))
               ((_0 _1) _2 (_0 _1 . _2))
               ((_0 _1 _2) _3 (_0 _1 _2 . _3))
               ((_0 _1 _2 _3) _4 (_0 _1 _2 _3 . _4))
               ((_0 _1 _2 _3 _4) _5 (_0 _1 _2 _3 _4 . _5))
               ((_0 _1 _2 _3 _4 _5) _6 (_0 _1 _2 _3 _4 _5 . _6)))]

Notice how the right hand side always remains fresh. This encodes an infinite number of solutions. The first result says "Any list left-appended by the empty list is itself."

@subsubsection{Reversing a list and palindromes}
Suppose we have defined a binary relation @racket[(reverseo a b)] where @racket[b] is the reverse of @racket[a]. Notice that this is a symmetric relation: if @racket[(reverseo x y)] holds, then so does @racket[(reverseo y x)].

@subsection{Nonempty riffling and Bell numbers}
@chunk[<nonempty-riffleo*>
       (defrel (nonempty-riffleo* l o)
         (fresh (h t rec)
                (conde ((== l '()) (== o '()))
                       ((== l `(,h . ,t))
                        (nonemptyo h)
                        (riffleo h rec o)
                        (nonempty-riffleo* t rec)))))]



@section{Smaller relations}
@chunk[<smaller-relations>
       <palindromeo>]

@chunk[<palindromeo>
       (defrel (palindromeo l)
         (conde ((== l '()))
                ((fresh (x y)
                        (appendo `(,x . ,y) `(,x) l)
                        (palindromeo y)))))]


@section{Actor Association Game}
The actor association game, known also as "The Six Degrees of Kevin Bacon" is a way to get from one actor to another by listing a chain of movie collaborations.
@chunk[<actor-association-game>
       (defrel (bacono movies x y proof)
         (fresh (movie actors rest-movies next rest-proof ^1)
                (picko `((,movie . ,actors)) rest-movies movies)
                (conde ((== proof `((,movie ,x ,y)))
                        (picko `(,x ,y) ^1 actors))
                       ((== proof `((,movie ,x ,next) . ,rest-proof))
                        (picko `(,x ,next) ^1 actors)
                        (bacono rest-movies next y rest-proof)))))


       (define all-movies
         '((glass-onion           daniel-craig edward-norton)
           (avengers              robert-downey-jr scarlett-johannson)
           (casino-royale         daniel-craig eva-green)
           (the-departed          mark-wahlberg leonardo-dicaprio)
           (spider-man-homecoming tom-holland robert-downey-jr)
           (uncharted             tom-holland mark-wahlberg)))]


@section{Finite relationships using @racket[ino]}

@chunk[<ino>
       (defrel (ino x l)
         (fresh (h t)
                (== l `(,h . ,t))
                (conde ((== x h))
                       ((ino x t)))))]

@racket[ino] can also be defined in terms of @racket[riffleo].

@subsection{A sample data base (taken from SICP}
@chunk[<a-sample-databaseo>
       (defrel (a-sample-databaseo query)
         (ino query
              '((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
                (job (Bitdiddle Ben) (computer wizard))
                (salary (Bitdiddle Ben) 60000)


                (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
                (job (Hacker Alyssa P) (computer programmer))
                (salary (Hacker Alyssa P) 40000)
                (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

                (address (Fect Cy D) (Cambridge (Ames Street) 3))
                (job (Fect Cy D) (computer programmer))
                (salary (Fect Cy D) 35000)
                (supervisor (Fect Cy D) (Bitdiddle Ben))

                (address (Tweakit Lem E) (Boston (Bay State Road) 22))
                (job (Tweakit Lem E) (computer technician))
                (salary (Tweakit Lem E) 25000)
                (supervisor (Tweakit Lem E) (Bitdiddle Ben))              

                (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
                (job (Reasoner Louis) (computer programmer trainee))
                (salary (Reasoner Louis) 30000)
                (supervisor (Reasoner Louis) (Hacker Alyssa P))

                (supervisor (Bitdiddle Ben) (Warbucks Oliver))
                (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
                (job (Warbucks Oliver) (administration big wheel))
                (salary (Warbucks Oliver) 150000)
                      
                (address (Scrooge Eben) (Weston (Shady Lane) 10))
                (job (Scrooge Eben) (accounting chief accountant))
                (salary (Scrooge Eben) 75000)
                (supervisor (Scrooge Eben) (Warbucks Oliver))

                (address (Cratchet Robert) (Allston (N Harvard Street) 16))
                (job (Cratchet Robert) (accounting scrivener))
                (salary (Cratchet Robert) 18000)
                (supervisor (Cratchet Robert) (Scrooge Eben))

                (address (Aull DeWitt) (Slumerville (Onion Square) 5))
                (job (Aull DeWitt) (administration secretary))
                (salary (Aull DeWitt) 25000)
                (supervisor (Aull DeWitt) (Warbucks Oliver))

                (can-do-job (computer wizard) (computer programmer))
                (can-do-job (computer wizard) (computer technician))

                (can-do-job (computer programmer)
                            (computer programmer trainee))

                (can-do-job (administration secretary)
                            (administration big wheel)))))]

@racketblock[
 > (run 1 (x) (a-sample-databaseo `(job ,x (computer programmer))))
 '(((Hacker Alyssa P)))]
                      
@section{A worked example: Decoding MIPS assembly instructions}
There is a difference between the actual zeroes and ones that a MIPS CPU executes, and the assembly instructions that they represent. miniKanren can bridge this divide.

I'm going to try to delegate work to other relations where possible.

Adapted from: https://inst.eecs.berkeley.edu/~cs61c/resources/MIPS_help.html

@chunk[<mipso>
       (defrel (mips)
         (fresh (opcode rs rt rd shift func instruction IMM pseudo-address)
                (conde ((appendo* `(,opcode ,rs ,rt ,rd ,shift ,func) instruction) (r-formato ))
                       ((appendo* `(,opcode ,rs ,rt ,IMM) instruction))
                       ((appendo* `(,opcode ,pseudo-address) instruction)))))




       (defrel (r-formato opcode $rs $rt $rd shamt funct)
         (fresh (x) (== x x)))]

@section{miniKanren and formal languages}

@subsection{miniKanren >= Regular}

miniKanren can recognize regular languages.

@chunk[<regularo>
       (defrel (regularo regex str)
         (fresh (e1 e2 s1 s2)
                (conde ((== regex 0) (== str '(0)))
                       ((== regex 1) (== str '(1)))

                       ((== regex `(+ ,e1 ,e2))
                        (appendo s1 s2 str)
                        (regularo e1 s1)
                        (regularo e2 s2))

                       ((== regex `(or ,e1 ,e2))
                        (conde ((regularo e1 str))
                               ((regularo e2 str))))

                       ((== regex `(* ,e1))
                        (conde ((== str '()))
                               ((regularo `(+ ,e1 ,regex) str)))))))]



@subsection{miniKanren >= Context-Free}
@subsubsection{Recognizing the nonregular language @racket[a^nb^n]}
@chunk[<anbno>
       (defrel (anbno str)
         (conde ((== str '()))
                ((fresh (middle)
                        (appendo `(a . ,middle) '(b) str)
                        (anbno middle)))))]

@chunk[<context-freeo>
       (defrel (context-freeo terminal-symbols production-rules string parse-tree)
         'TODO)]
         

@subsection{miniKanren >= P via Circuit-Value-Problem}

@subsection{miniKanren >= NP}
@subsubsection{via Satisfiability}
@subsubsection{via 3-Color}
Given:
- I want to three-color the overall graph.
- u and v cannot be the same color
Assuming:
- The three colors are @racket['red], @racket['green], and @racket['blue].
Then I can conclude:
@chunk[<color-=/=>
       (defrel (color-=/= u v)
         (ino `(,u ,v)
              '((red   green)
                (red   blue)
                (green red)
                (green blue)
                (blue  red)
                (blue  green))))]


@chunk[<three-colorableo>
       (defrel (three-colorableo g coloring)
         (conde
          ((== g '()))
          ((fresh (u v color-of-u color-of-v rest-edges ^1)
                  (== g `((,u ,v) . ,rest-edges))
                  (riffleo `((,u ,color-of-u) (,v ,color-of-v)) ^1 coloring)
                  (color-=/= color-of-u color-of-v)
                  (three-colorableo rest-edges coloring)))))]

@subsection{miniKanren >= coNP via recursive Hajos enumeration}
@chunk[<hajoso>
       (defrel (has-edgeo u v rest g)
         (fresh (u^ v^)
                (riffleo `(,u) `(,v) `(,u^ ,v^))
                (riffleo `(,u^ ,v^) rest g)))
                
       (defrel (hajoso g)
         (conde
          ((fresh (v1 v2 v3 v4 ^1 ^2 ^3 ^4)
                  ;; g == K4, ordered
                  (== g `((,v1 ,v2)
                          (,v1 ,v3)
                          (,v1 ,v4)
                          (,v2 ,v3)
                          (,v2 ,v4)
                          (,v3 ,v4)))))
          ((fresh (n1 n2 shared rest1 rest2 rest g1 g2)
                  (has-edgeo shared n1 rest1 g1)
                  (has-edgeo shared n2 rest2 g2)
                  ;(has-edgeo n1 n2 rest g)
                  ;(appendo rest1 rest2 rest) ;; assume subgraphs are ordered
                  (hajoso g1)
                  (hajoso g2)))))]



@subsection{miniKanren >= PSPACE}
@subsubsection{via Geography Game (??)}
@subsubsection{via miniKanren >= Context-Sensitive}

@subsection{miniKanren == RE}
@subsubsection{via Turing-completeness}
TODO. Incomplete.
@chunk[<evalo>
       (defrel (evalo transitions final-states current-state l current-symbol r result)
         (fresh (next-state new-symbol direction h t)
                (conde
                 ((ino current-state final-states) (== result `(,l ,current-symbol ,r)))
                 ((ino `(,current-state ,current-symbol ,next-state ,new-symbol ,direction) transitions)
                  (conde
                   ((== direction 'l) (== l `(,h . ,t)) (evalo transitions next-state t h `(,new-symbol . ,r)))
                   (== direction 'r) (== r `(,h . ,t)) (evalo transitions next-state `(,new-symbol . ,l) h t ))))))]
           

@subsubsection{via Post's Correspondence Problem}
@chunk[<post-correspondenceo>
       (defrel (domino-appendo dominoes l1 l2 o1 o2)
         (conde
          ((== l1 '()) (== l2 '()) (== o1 '()) (== o2 '()))
          ((fresh (h1 t1 h2 t2 rec1 rec2)
                  (== l1 `(,h1 . ,t1))
                  (== l2 `(,h2 . ,t2))
                  (ino `(,h1 ,h2) dominoes)
                  (appendo h1 rec1 o1)
                  (appendo h2 rec2 o2)
                  (domino-appendo dominoes t1 t2 rec1 rec2)))))


       
       (defrel (post-correspondenceo dominoes top bottom)
         (fresh (str)
                (nonemptyo str)
                (domino-appendo dominoes top bottom str str)))]

@racketblock[
 > (run 1 (top bottom) (post-correspondenceo

               '(((a)   (b a a))
                 ((a b)   (a a))
                 ((b b a) (b b)))

               top bottom))

   '((((b b a) (a b) (b b a) (a))
      ((b b) (a a) (b b) (b a a))))]


@section{miniKanren and the free monoid}
@subsection{Levi's lemma}

Per @hyperlink["https://en.wikipedia.org/wiki/Levi%27s_lemma" "Wikipedia"]:
``For all strings u, v, x, and y, if uv = xy, then there exists a string w such that either
uw = x and v = wy (if |u| <= |x|)
or
u = xw and wv = y (if |u| >= |x|)
''

@chunk[<levis-lemmao>
       #| thinking naively |#
       (defrel (levis-lemmao-v1 u v x y w)
         (fresh (s)
                (appendo u v s)
                (appendo x y s)
                (conde ((appendo u w x) (appendo w y v))
                       ((appendo x w u) (appendo w v y)))))


       #| encoding an infinite number of solutions using a fresh variable |#
       (defrel (levis-lemmao u v s)
         (fresh (x y)
                (appendo x s u)
                (appendo s y v)))]

@section{First-Order miniKanren}
This section describes miniKanren in terms of its representation.

A  function for iteratively repeating an operation until the output stops changing.
@chunk[<apply-until-fixpoint>
       (define (apply-until-fixpoint f x)
         (let ((y (f x)))
           (if (equal? y x)
               x
               (apply-until-fixpoint f y))))]

Here are the definitions for two types of occurrence checks: @racket[occurs-in-exp?] and @racket[occurs-in-sexp?], as well as two replacement checks: @racket[replace-in-exp] and @racket[replace-in-sexp].

Their plural counterparts, @racket[occurs-in-exps] and @racket[occurs-in-sexps], are trivially implemented using @racket[any], so they are defined in the appendix.

@chunk[<first-order-minikanren>
       <exp-operations>
       <sexp-operations>
       <quasiquotation>
       <rest-first-order-minikanren>]

@chunk[<exp-operations>
       (define (occurs-in-exp? x exp)
         (match exp
           (`(conde . ,exps*)
            (any (lambda (exps) (occurs-in-exps? x exps)) exps*))

           (`(fresh ,vars . ,exps)
            (and (not (any-occurs-in-sexp? vars x))
                 (occurs-in-exps? x exps)))

           (`(,rel . ,sexps)
            (occurs-in-sexps? x sexps))))

       (define (replace-in-exp x y exp)
         (match exp
           (`(conde . ,exps*)
            `(conde . ,(map (lambda (exps) (replace-in-exps x y exps)) exps*)))

           (`(fresh ,vars . ,exps)
            `(fresh ,vars . ,(if (or (any-occurs-in-sexp? vars x)
                                     (any-occurs-in-sexp? vars y))
                                 exps
                                 (replace-in-exps x y exps))))

           (`(,rel . ,sexps)
            `(,rel . ,(replace-in-sexps x y sexps)))))]

@chunk[<sexp-operations>
       (define (occurs-in-sexp? x sexp)
         (or (equal? x sexp)
             (match sexp
               (`(,h . ,t) (match h
                             ('quasiquote (qq-occurs-in-sexp? x t))
                             ('quote #f)
                             (_ (or (occurs-in-sexp? x h)
                                    (occurs-in-sexp? x t)))))
               (_ #f))))

       (define (replace-in-sexp x y sexp)
         (if (equal? x sexp) y 
             (match sexp
               (`(,h . ,t) (match h
                             ('quasiquote (cons h (qq-replace-in-sexp x y t)))
                             ('quote (cons h t))
                             (_ (cons (replace-in-sexp x y h)
                                      (replace-in-sexp x y t)))))
               (_ sexp))))]

@subsection{The problem with quasiquote}
Quasiquote means that I need to introduce mutual recursion. You can think of it like recurring with a boolean flag signaling that the s-expression in question is part of a larger, quasiquoted s-expression, so the code should deal with the case of @racket[unquote].

@chunk[<quasiquotation>
       (define (qq-occurs-in-sexp? x sexp)
         (match sexp
           (`(,h . ,t) (match h
                         ('unquote (occurs-in-sexp? x t))
                         ('quote #f)
                         (_ (or (qq-occurs-in-sexp? x h)
                                (qq-occurs-in-sexp? x t)))))
           (_ #f)))

       (define (qq-replace-in-sexp x y sexp)
         (match sexp
           (`(,h . ,t) (match h
                         ('unquote (cons h (replace-in-sexp x y t)))
                         ('quote (cons h t))
                         (_ (cons (qq-replace-in-sexp x y h)
                                  (qq-replace-in-sexp x y t)))))
           (_ sexp)))]

@chunk[<rest-first-order-minikanren>       
       (define (unify-exps exps)
         (match exps
           ('() '())
           (`((== (,l1 . ,r1) (,l2 . ,r2)) . ,rest) (unify-exps `((== ,l1 ,l2)
                                                                  (== ,r1 ,r2) . ,rest)))
           (`((== ,u ,v) . ,rest) (match `(,(occurs-in-sexp? u v) ,(occurs-in-sexp? v u))
                                    ('(#t #t) (unify-exps rest))
                                    ('(#t #f) #f)
                                    ('(#f #t) #f)
                                    ('(#f #f) (unify-exps (match `(,(occurs-in-exps? u rest) ,(occurs-in-exps? v rest))
                                                            ('(#t #t) (replace-in-exps u v rest)) ; replace u with v in rest
                                                            ('(#t #f) (replace-in-exps u v rest))
                                                            ('(#f #t) (replace-in-exps v u rest))
                                                            ('(#f #f) rest))))))
           (`((fresh ,vars . ,exps) . ,rest)
            `((fresh ,vars . ,(unify-exps exps)) . ,(unify-exps rest)))

           (`((conde . ,exps*) . ,rest)
            `((conde . ,(map (lambda (exps) (unify-exps exps)) exps*)) . ,(unify-exps rest)))

           (`((,relation . ,args) . ,rest) `((,relation . ,args) . ,(unify-exps rest)))))]

@section{Relational NatMaps}
It should be easy enough to type this and have it work. But for now I have to convert it manually.
@chunk[<relational-binary-natmaps>
       (defrel (natmapov1 t)
         (fresh (A B value)
                (ino t `((T1)
                         (T2 ,value)
                         (T3 ,A)
                         (T4 ,value ,A)
                         (T5 ,B)
                         (T6 ,value ,B)
                         (T7 ,A ,B)
                         (T8 ,value ,A ,B)))
                (Ao A)
                (Bo B)))

       (defrel (natmapo t)
         (fresh (A B value)
                (conde ((== t `(T1)))
                       ((== t `(T2 ,value)))
                       ((== t `(T3 ,A)) (Ao A))
                       ((== t `(T4 ,value ,A)) (Ao A))
                       ((== t `(T5 ,B)) (Bo B))
                       ((== t `(T6 ,value ,B)) (Bo B))
                       ((== t `(T7 ,A ,B)) (Ao A) (Bo B))
                       ((== t `(T8 ,value ,A ,B)) (Ao A) (Bo B)))))

       (defrel (Aov1 x)
         (fresh (A B)
                (ino x `((A1 ,A)
                         (A2 ,B)
                         (A3 ,A ,B)))
                (Ao A)
                (Bo B)))

       (defrel (Ao x)
         (fresh (A B)
                (conde ((== x `(A1 ,A)) (Ao A))
                       ((== x `(A2 ,B)) (Bo B))
                       ((== x `(A3 ,A ,B)) (Ao A) (Bo B)))))

       (defrel (Bov1 x)
         (fresh (A B value)
                (ino x `((B1 ,value)
                         (B2 ,A)
                         (B3 ,value ,A)
                         (B4 ,B)
                         (B5 ,value ,B)
                         (B6 ,A ,B)
                         (B7 ,value ,A ,B)))))

       (defrel (Bo x)
         (fresh (A B value)
                (conde
                 ((== x `(B1 ,value)))
                 ((== x `(B2 ,A)) (Ao A))
                 ((== x `(B3 ,value ,A)) (Ao A))
                 ((== x `(B4 ,B)) (Bo B))
                 ((== x `(B5 ,value ,B)) (Bo B))
                 ((== x `(B6 ,A ,B)) (Ao A) (Bo B))
                 ((== x `(B7 ,value ,A ,B)) (Ao A) (Bo B)))))
       
       (defrel (lookup1o int tree value)
         (fresh (l r branch h t)
                (conde ((== int '()) (== tree `(has ,l ,r ,value)))
                       ((== int `(,h . ,t))
                        (conde ((== h 0) (== branch l))
                               ((== h 1) (== branch r)))
                        (lookup1o t branch value)))))]

@racketblock[> (run 500 (q) (natmap q))
             (
              ;; ...
              ((T3 (A1 (A2 (B2 (A3 (A2 (B1 _0))
                                   (B2 (A2 (B1 _1))))))))))]

@racketblock[> (run 5 (int tree) (lookup1o int tree 'value))]
@racketblock['((()
                (has _0 _1 value))
               ((0) _0)
               ((1) _0)
               ((0 0) _0)
               ((1 0) _0))]
This binary little endian intmap allows trailing zeros. It does not have singular representations for numbers.

The following Haskell mutually recursive datatypes describing an intmap tree forbids trailing zeroes.



An attempt in unary:
@chunk[<relational-unary-natmaps>
       (defrel (lookupo nat l value)
         (fresh (h t n-1)
                (== l `(,h . ,t))
                (conde ((== nat '()) (== h `(has ,value)))
                       ((== nat `(s . ,n-1))
                        (lookupo n-1 t value)))))]

@racketblock[> (run 5 (nat l) (lookupo nat l 'value))
             '((() ((has value) . _0))
               ((s) (_0 (has value) . _1))
               ((s s) (_0 _1 (has value) . _2))
               ((s s s) (_0 _1 _2 (has value) . _3))
               ((s s s s) (_0 _1 _2 _3 (has value) . _4)))]

Merging is just the relational equivalent of @racket[(zip-with conso)]
@chunk[<relational-natmaps-merge>
       (defrel (mergeo ml mr mo)
         (fresh (hl tl hr tr to)
                (conde ((== ml '()) (== mr '()) (== mo '()))
                       ((== ml '()) (== mr `(,hr . ,tr)) (== mr mo))
                       ((== ml `(,hl . ,tl)) (== mr '()) (== ml mo))
                       ((== ml `(,hl . ,tl)) (== mr `(,hr . ,tr)) (== mo `((,hl ,hr) . ,to))
                                             (mergeo tl tr to)))))]
                
@racketblock[> (run 7 (a b c) (mergeo a b c))
             '((() () ())
               (() (_0 . _1) (_0 . _1))
               ((_0 . _1) () (_0 . _1))
               ((_0) (_1) ((_0 _1)))
               ((_0) (_1 _2 . _3) ((_0 _1) _2 . _3))
               ((_0 _1 . _2) (_3) ((_0 _3) _1 . _2))
               ((_0 _1) (_2 _3) ((_0 _2) (_1 _3))))]


data IntMap = () | ((just ,x) . IntMap) | ((nothing) . IntMap)
data IntMap = List (Maybe x)
This made me realize a flaw in the representation: there is nothing constraining the inmaps from having a long tail of @racket['(nothing)] s after their last @racket[`(just ,x)] .

@chunk[<relational-natmaps>
       <relational-binary-natmaps>
       <relational-unary-natmaps>
       <relational-natmaps-merge>]

@section{Putting it all together}

Putting it all together
@chunk[<*>
       <load>
       <nonemptyo>
       <appendo>
       <riffleo>
       <picko>
       <appendo*>
       <ino>
       <nonempty-appendo*>
       <nonempty-riffleo*>
       <smaller-relations>
       <a-sample-databaseo>
       <regularo>
       <anbno>
       <context-freeo>
       <hajoso>
       <evalo>
       <post-correspondenceo>
       <color-=/=>
       <three-colorableo>
       <apply-until-fixpoint>
       <first-order-minikanren>
       <levis-lemmao>
       <relational-natmaps>
       <misc-definitions>
       <actor-association-game>]

@section{Appendix}
@chunk[<misc-definitions>
       (define (any p? l)
         (match l
           ('() #f)
           (`(,h . ,t) (or (p? h) (any p? t)))))

       (define (occurs-in-exps? var exps)
         (any (lambda (exp) (occurs-in-exps? var exp)) exps))

       (define (replace-in-exps x y exps)
         (map (lambda (exp) (replace-in-exp x y exp)) exps))

       (define (replace-in-sexps x y sexps)
         (map (lambda (sexp) (replace-in-sexp x y sexp)) sexps))

       (define (any-occurs-in-sexp? vars sexp)
         (any (lambda (var) (occurs-in-sexp? var sexp)) vars))
       
       (define (occurs-in-sexps? var sexps)
         (any (lambda (sexp) (occurs-in-sexp? var sexp) sexps)))]