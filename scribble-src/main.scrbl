#lang scribble/lp2
@;;(require scribble/lp-include)

@title{Cool Relations}

@(require scribble/manual)
@(require scriblib/figure)

@section{Code from The Reasoned Schemer 2nd Edition}
@chunk[<load>
       (require racket/match)
       (require racket/include)
       (include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")]

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

                         (and (sorted? a)
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


@section{List Length}
@chunk[<lengtho>
       (defrel (lengtho l n)
         (conde ((== n '()) (== l '()))
                ((fresh (h t lrec lrec*2 x)
                        (== n `(,h . ,t))

                        (conde ((== h 0)
                                (== l lrec*2)
                                (fresh (a d) (== t `(,a . ,d))))

                               ((== h 1)
                                (== l `(,x . ,lrec*2))))

                        (double-lengtho lrec lrec*2)
                        (lengtho lrec t)))))

       (defrel (double-lengtho l l*2)
         (conde ((== l '()) (== l*2 '()))
                ((fresh (h h1 h2 t t*2)
                        (== l `(,h . ,t))
                        (== l*2 `(,h1 ,h2 . ,t*2))
                        (double-lengtho t t*2)))))]

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

@section{Khmer Zodiac Animals}
Similar to how English speakers say "pork" instead of "pig" when discussing food, Khmer speakers uses different words for animals in the zodiac years. A 12-row table describes the relationship completely.

@chunk[<khmer-zodiaco>
       (defrel (khmer-zodiaco english typical-word zodiac-word)
         (ino `(,english ,typical-word ,zodiac-word)
              '((rat កណ្ដុរ ជូត)
                (ox គោ ឆ្លូវ)
                (tiger ខ្លា ខាល)
                (rabbit ទន្សាយ ថោះ)
                (dragon នាគ រោង)
                (snake ពស់ ម្សាញ់)
                (horse សេះ មមី)
                (goat ពពែ មមែ)
                (monkey ស្វា វក)
                (rooster មាន់ រកា)
                (dog ឆ្កែ ច)
                (pig ជ្រូក កុរ))))]
                                            
This table is crucial for Khmer learners, but I wish it had better vertical alignment.

@section{A worked example: Decoding MIPS}
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
       <rest-first-order-minikanren>
       <replace-in-exp>
       <replace-in-sexp>
       <qq-replace-in-sexp>
       <replace-definitions>]

@chunk[<exp-operations>
       (define (occurs-in-exp? sym exp)
         (match exp
           (`(conde . ,exps*)
            (any (lambda (exps) (occurs-in-exps? sym exps)) exps*))

           (`(fresh ,vars . ,exps)
            (and (not (any-occurs-in-sexp? vars sym))
                 (occurs-in-exps? sym exps)))

           (`(,rel . ,sexps)
            (occurs-in-sexps? sym sexps))))]

@chunk[<sexp-operations>
       (define (occurs-in-sexp? sym sexp)
         (or (equal? sym sexp)
             (match sexp
               (`(,h . ,t) (match h
                             ('quasiquote (qq-occurs-in-sexp? sym t))
                             ('quote #f)
                             (_ (or (occurs-in-sexp? sym h)
                                    (occurs-in-sexp? sym t)))))
               (_ #f))))]

@subsection{The problem with quasiquote}
Quasiquote means that I need to introduce mutual recursion. You can think of it like recurring with a boolean flag signaling that the s-expression in question is part of a larger, quasiquoted s-expression, so the code should deal with the case of @racket[unquote].

@chunk[<quasiquotation>
       (define (qq-occurs-in-sexp? sym sexp)
         (match sexp
           (`(,h . ,t) (match h
                         ('unquote (occurs-in-sexp? sym t))
                         ('quote #f)
                         (_ (or (qq-occurs-in-sexp? sym h)
                                (qq-occurs-in-sexp? sym t)))))
           (_ #f)))]


@subsection{Using gensym to create copies of relations with no name conflicts}

@chunk[<replace-in-exp>
       (define (replace-in-exp replacements exp)
         (match exp
           (`(conde . ,exps*)
            `(conde . ,(map* (lambda (exp)
                               (replace-in-exp replacements exp)) exps*)))

           (`(fresh ,vars . ,exps)
            (let ((gensyms (map (lambda (var) (gensym)) vars)))
              `(fresh ,gensyms . ,(map (lambda (exp) (replace-in-exp (append (zip vars gensyms) replacements) exp)) exps))))

           (`(,rel . ,args)
            `(,rel . ,(map (lambda (arg)
                             (replace-in-sexp replacements arg)) args)))))]


@chunk[<replace-in-sexp>
       (define (replace-in-sexp replacements sexp)
         (match sexp
           (`(,h . ,t) (match h
                         ('quote sexp)
                         ('quasiquote (cons h (qq-replace-in-sexp replacements t)))
                         (_ (cons (replace-in-sexp replacements h)
                                  (replace-in-sexp replacements t)))))
           
           (_ (match (lookup sexp replacements)
                (`(just ,replacement) replacement)
                ('(nothing) sexp)))))]

Given a list of pairs @racket[replacements], and an s-expression @racket[sexp], return a copy of the s-expression, except that any symbol appearing in the left-hand side of @racket[replacements] is replaced by the associated right-hand side.

@chunk[<qq-replace-in-sexp>
       (define (qq-replace-in-sexp replacements sexp)
         (match sexp
           (`(,h . ,t) (match h
                         ('unquote (cons h (replace-in-sexp replacements t)))
                         ('quote (cons h t))
                         (_ (cons (qq-replace-in-sexp replacements h)
                                  (qq-replace-in-sexp replacements t)))))

           (_ sexp)))]
As always, quasiquotation needs to be dealt with, so a separate mutually recursive call is made with the @racket[qq-] prefix to track that the procedure is in a quasiquoted environment.

@subsection{Algorithm design}
@image["img/algorithm-design.png"]


@chunk[<replace-definitions>
       (define (replace-definitions defrels exps)
         (append-map
          (lambda (exp)
            (match exp
              (`(conde . ,exps*)
               `((conde . ,(map (lambda (exps) (replace-definitions defrels exps)) exps*))))

              (`(fresh ,vars . ,exps)
               `((fresh ,vars ,(replace-definitions defrels exps))))

              (`(== ,u ,v)
               `((== ,u ,v)))

              (`(,rel . ,args) (let ((defrel (car (filter
                                                   (lambda (defrel)
                                                     (match defrel
                                                       (`(defrel (,name . ,params) . ,exps)
                                                        (equal? rel name)))) defrels))))

                                 (match defrel
                                   (`(defrel (,name . ,params) . ,exps)
                                    (map (lambda (exp) (replace-in-exp (zip params args) exp)) exps)))))))
          exps))]
Given a list of relation definitions @racket[defrels], and a list of miniKanren expressions @racket[exps],
return a new list of expressions where all named relations have been replaced with their definition.


@chunk[<is-simple-conjunction?>
       (define (is-simple-conjunction? conjunction)
         (all (lambda (exp) (match exp
                              (`(== ,u ,v) #t)
                              (_ #f))) conjunction))]
                                                                             


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


@section{Existential DNFs (EDNFs)}
An existential DNF is a special form of a miniKanren fresh. It is a fresh of n variables and only one clause: a conde clause. And no fresh or conde appear in any branches of the conde clause. It is useful to have relations in this form for reasons discussed later.

@racketblock[(fresh (x1 x2 x3 ... xn)
                    (conde ((r11 . a11) (r12 . a12) ... (r1i . a1i))
                           ((r21 . a21) (r22 . a22) ... (r2i . a2i))
                           ...
                           ((rj1 . aj1) (rj2 . aj2) ... (rij . aij))))]

Here is what an existential DNF looks like in traditional notation:

∃x₁x₂x₃...xₙ⋁ⁱ⋀ʲϕ

where ϕ is a stand in for any logical predicate.

@subsection{Conjunction of 2 EDNFs}

The conjunction of two existential DNFs can itself be represented as an existential DNF.

(∃x₁x₂x₃...xₙ⋁ⁱ⋀ʲϕ)∧(∃y₁y₂y₃...yₙ⋁ᵏ⋀ˡϕ) = ∃x₁x₂x₃...xₙy₁y₂y₃...yₙ⋁ⁱ*ᵏ⋀ʲ⁺ˡϕ

In miniKanren

@racketblock[(= (conj (fresh (x1 x2 x3 ... xn)
                             (conde (p11 p12 ... p1j)
                                    (p21 p22 ... p2j)
                                    ...
                                    (pi1 pi2 ... pij)))

                      (fresh (y1 y2 y3 ... ym)
                             (conde (q11 q12 ... q1l)
                                    (q21 q22 ... q2l)
                                    ...
                                    (qk1 qk2 ... qkl))))

                
                (fresh (x1 x2 x3 ... xn y1 y2 y3 ... ym)
                       (conde (p11 p12 ... p1j q11 q12 ... q1l)
                              (p11 p12 ... p1j q21 q22 ... q2l) ...
                              (p11 p12 ... p1j qk1 qk2 ... qkl)
                          
                              (p21 p22 ... p2j q11 q12 ... q1l)
                              (p21 p22 ... p2j q21 q22 ... q2l) ...
                              (p21 p22 ... p2j qk1 qk2 ... qkl)
                              
                              ...
                              
                              (pi1 pi2 ... pij q11 q12 ... q1l)
                              (pi1 pi2 ... pij q21 q22 ... q2l) ...
                              (pi1 pi2 ... pij qk1 qk2 ... qkl))))]


Here is the full definition in Racket.

@chunk[<conjunction-of-ednfs>
       (define (conjunction a b)
         (match `(,a ,b)
           (`((fresh ,xs
                     (conde . ,ps))
              (fresh ,ys
                     (conde . ,qs)))
            `(fresh ,(append xs ys)
                    (conde . ,(cartesian-product-with-append ps qs))))))]

Here, @racket[(cartesian-product-with-append xs ys)], means the same as [x ++ y | x <- xs, y <- ys] in Haskell.

[x ++ y | x <- xs, y <- ys]

= do x <- xs; y <- ys; return x ++ y

= concatMap (\x -> concatMap (\y -> return x ++ y) ys) xs

= concatMap (\x -> concatMap (\y -> [x ++ y]) ys) xs

= concatMap (\x -> map (\y -> x ++ y) ys) xs



Which is finally a form translateable to Scheme.
@chunk[<cartesian-product-with-append>
       (define (cartesian-product-with-append xs ys)
         (append-map (lambda (x) (map (lambda (y) (append x y)) ys)) xs))]

@subsection{Disjunction of two ENDFs}
A disjunction of two EDNFs is:


In miniKanren:-

@racketblock[(= (disj (fresh (x1 x2 x3 ... xn)
                             (conde (p11 p12 ... p1j)
                                    (p21 p22 ... p2j)
                                    ...
                                    (pi1 pi2 ... pij)))

                      (fresh (y1 y2 y3 ... ym)
                             (conde (q11 q12 ... q1l)
                                    (q21 q22 ... q2l)
                                    ...
                                    (qk1 qk2 ... qkl))))

                (fresh (x1 x2 x3 ... xn y1 y2 y3 ... ym)
                       (conde (p11 p12 ... p1j)
                              (p21 p22 ... p2j)
                              ...
                              (pi1 pi2 ... pij)
                              (q11 q12 ... q1l)
                              (q21 q22 ... q2l)
                              ...
                              (qk1 qk2 ... qkl))))]

@chunk[<disjunction-of-ednfs>
       (define (disjunction ednf-a ednf-b)
         (match `(,ednf-a ,ednf-b)
           (`((fresh ,xs (conde . ,ps)) (fresh ,ys (conde . ,qs)))

            `(fresh ,(append xs ys) (conde . ,(append ps qs))))))]
       

@subsection{Associative conjunction* and disjunction*}
@chunk[<associative-conjunction*-and-disjunction*>
       (define (conjunction* ednfs)
         (match ednfs
           ('() '(fresh () (conde . (()) )))
           (`(,a . ,d) (conjunction a (conjunction* d)))))

       (define (disjunction* ednfs)
         (match ednfs
           ('() '(fresh () (conde . ())))
           (`(,a . ,d) (disjunction a (disjunction* d)))))]


@subsection{Converting a miniKanren expression into EDNF}
@chunk[<exp-to-ednf>
       (define (exp-to-ednf exp)
         (match exp
           (`(fresh ,vars . ,exps) (match (conjunction* (map exp-to-ednf exps))
                                     (`(fresh ,xs . ,ps) `(fresh ,(append vars xs) . ,ps))))
           (`(conde . ,exps*) (disjunction* (map (lambda (exps) (conjunction* (map exp-to-ednf exps))) exps*)))
           (`(,rel . ,args) `(fresh ()
                                    (conde ((,rel . ,args)))))))]

Since a @racket[run] statement takes a conjunction of expressions, it is necessary to have a version from conjunction to ednf.

@chunk[<conjunction-to-ednf>
       (define (conjunction-to-ednf exps)
         (conjunction* (map exp-to-ednf exps)))]


@subsection{Replace definitions in EDNF}
The @racket[replace-definitions] function takes a list of expressions rather than a single expression, but an EDNF is a single expression. So a new function is needed that works specifically on EDNFs.

@chunk[<replace-definitions-in-ednf>
       (define (replace-definitions-in-ednf defrels ednf)
         (match ednf
           (`(fresh ,vars (conde . ,exps*))
            `(fresh ,vars (conde . ,(map (lambda (exps)
                                           (replace-definitions defrels exps)) exps*))))))]


@subsection{A small portion of the algorithm}
At this point there is enough code to write a small portion of the overall algorithm, namely:

@image["img/algorithm-design-subset.png"]

Below is an implementation of this subset on some sample miniKanren code.

@chunk[<algorithm-subset>
       (define (algorithm-subset)
         (let* ((defrels '((defrel (appendo l r o)
                            (conde ((== l '()) (== r o))
                                   ((fresh (h t rec)
                                           (== l `(,h . ,t))
                                           (== o `(,h . ,rec))
                                           (appendo t r rec)))))))

                (exp1 '(appendo l r o))

                (ednf1 (exp-to-ednf exp1))

                (exp2 (replace-definitions-in-ednf defrels ednf1))

                (ednf2 (exp-to-ednf exp2))

                (exp3 (replace-definitions-in-ednf defrels ednf2))

                (ednf3 (exp-to-ednf exp3)))

           (print ednf1)
           (display "\n\n")
           (print exp2)
           (display "\n\n")
           (print ednf2)
           (display "\n\n")
           (print exp3)
           (display "\n\n")
           (print ednf3)))]


@chunk[<algorithm-subset-iterative>
       (define (algorithm-subset-iterative n defrels exp)
         (match n
           (0 (exp-to-ednf exp))
           (_ (algorithm-subset-iterative (- n 1)
                                          defrels
                                          (replace-definitions-in-ednf defrels (exp-to-ednf exp))))))


       (define my-defrels '((defrel (appendo l r o)
                              (conde ((== l '()) (== r o))
                                     ((fresh (h t rec)
                                             (== l `(,h . ,t))
                                             (== o `(,h . ,rec))
                                             (appendo t r rec)))))))

       (define my-exp '(appendo x y z))]

           

           



@subsection{Running an EDNF}
An EDNF is just a special form of the fresh expression in miniKanren. So imagine the special case of @racket[run] or @racket[run*] where the expression is an EDNF.

@racketblock[(run 3 (q) (fresh (x y z)
                               (conde  ((== q 'a))
                                       ((fooo x))
                                       ((== q 'b))
                                       ((baro y))
                                       ((== q 'c))
                                       ((bazo z)))))]

Code used:
@chunk[<existential-dnfs>
       <conjunction-of-ednfs>
       <disjunction-of-ednfs>
       <associative-conjunction*-and-disjunction*>
       <exp-to-ednf>
       <conjunction-to-ednf>
       <replace-definitions-in-ednf>
       <algorithm-subset>
       <algorithm-subset-iterative>
       <cartesian-product-with-append>]

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

@section{Prime factor encoding}
@chunk[<prime-factor-encoding>
       <naturalo>
       <natlisto>
       <godel-encodingo>]

@chunk[<naturalo>
       (defrel (naturalo n)
         (conde
          ((== n '()))
          ((fresh (h t)
                  (== n `(,h . ,t))
                  (conde
                   ((== h 0) (fresh (a d) (== t `(,a . ,d))))
                   ((== h 1)))
                  (naturalo t)))))]

@chunk[<natlisto>
       (defrel (natlisto l)
         (conde
          ((== l '()))
          ((fresh (h t)
                  (== l `(,h . ,t))
                  (naturalo h)
                  (natlisto t)))))]

@chunk[<godel-encodingo>
       (defrel (godel-encodingo l)
         (fresh (a b t)
                (== l `((,a . ,b) . ,t))
                (natlisto l)))]

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
       <khmer-zodiaco>
       <regularo>
       <anbno>
       <context-freeo>
       <hajoso>
       <evalo>
       <lengtho>
       <post-correspondenceo>
       <color-=/=>
       <three-colorableo>
       <apply-until-fixpoint>
       <first-order-minikanren>
       <levis-lemmao>
       <existential-dnfs>
       <relational-natmaps>
       <misc-definitions>
       <actor-association-game>
       <prime-factor-encoding>
       <scratch-area>]

@section{Appendix}
@chunk[<misc-definitions>
       (define (any p? l)
         (match l
           ('() #f)
           (`(,h . ,t) (or (p? h) (any p? t)))))

       (define (all p? l)
         (match l
           ('() #t)
           (`(,h . ,t) (and (p? h) (all p? t)))))

       (define (append-map f l)
         (match l
           ('() '())
           (`(,h . ,t) (append (f h) (append-map f t)))))

       (define (map* f list-of-lists)
         (map (lambda (l) (map f l)) list-of-lists)) 

       (define (zip l r)
         (match `(,l ,r)
           ('(() ()) '())
           (`((,lh . ,lt) (,rh . ,rt)) `((,lh ,rh) . ,(zip lt rt)))))

       (define (lookup x l)
         (match l
           (`((,h1 ,h2) . ,t) (if (equal? x h1) `(just ,h2) (lookup x t)))
           ('() '(nothing))))

       (define (occurs-in-exps? var exps)
         (any (lambda (exp) (occurs-in-exps? var exp)) exps))

       (define (replace-in-exps sym replacement exps)
         (map (lambda (exp) (replace-in-exp sym replacement exp)) exps))

       (define (replace-in-sexps sym replacement sexps)
         (map (lambda (sexp) (replace-in-sexp sym replacement sexp)) sexps))

       (define (any-occurs-in-sexp? vars sexp)
         (any (lambda (var) (occurs-in-sexp? var sexp)) vars))
       
       (define (occurs-in-sexps? var sexps)
         (any (lambda (sexp) (occurs-in-sexp? var sexp) sexps)))]

@chunk[<scratch-area>
       (define brett (exp-to-ednf '(conde ((== l '()) (== r o))
                                          ((fresh (a d rec)
                                                  (== l `(,a . ,d))
                                                  (== o `(,a . ,rec))
                                                  (appendo d r rec))))))
       
       (define ednfs '((fresh (a b c)
                              (conde ((== a b) (== c 0))
                                     ((== `(,a ,b) '(1 2)))))
                       (fresh (m n)
                              (conde ((== m 'twelve))
                                     ((== n 'thirteen))))
                       (fresh (x y z)
                              (conde ((== x 'Alpha) (== y 'Beta) (== z 'Gamma))
                                     ((== x 'Alef)  (== y 'Bayt) (== z 'Gimel))))))
       
       (define conjunction-test (conjunction '(fresh (a b c) (conde
                                                              ((== a 2) (== b 4))
                                                              ((== a 1) (== b 7))
                                                              ((appendo a b c))))
                                             '(fresh (x y z) (conde
                                                              ((== x y) (== z 0))
                                                              ((appendo x y '(1 2 3)))))))]