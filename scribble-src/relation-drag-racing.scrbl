#lang scribble/manual

@(require scribble-math)
@(require scribble-math/dollar)

@title[#:date "2023-09-25"]{miniKanren Drag Racing}
@author{Brett Schreiber}

This article documents my attempts at writing a relation in miniKanren which asserts for a given @racket[x]:
@itemlist[
 @item{@racket[x] is an Oleg number (explained below)}
 @item{@racket[x] is a multiple of 3. In other words, there exists some natural number @($ "n") such that @($ "x = 3n")}]

There are multiple ways to write this relation in miniKanren. In this article I will explore 4 candidate relation definitions and compare them in the following @bold{drag races}:
@itemlist[
 @item{Which relation can most quickly output the first 10000 multiples of 3?}
 @item{Which relation can most quickly recognize 999999999 as being a multiple of 3?}
 @item{Which relation can most quickly recognize 1000000000 as @bold{not} being a multiple of 3?}]

An Oleg number is a list of bits whose least significant digit comes first, and whose last digit cannot be @racket[0]. For example, @racket['(0 0 1)] is an Oleg number representing @racket[4]. TRS2 defines the function @racket[build-num] for converting a base ten number into an Oleg number. Below is the reverse definition, converting an Oleg number back into base ten: @margin-note{All code in this article was executed using the miniKanren implementation from @code{TRS2}, but @code{faster-miniKanren} could also be used.}

@racketblock[(define (base-ten n)
               (match n
                 ('() 0)
                 (`(,head . ,tail) (+ head (* 2 (base-ten tail))))))]

Now, onto the four implementations:

@section{The straightforward approach}

miniKanren excels at modeling facts of the form "there exists" by using the @racket[fresh] keyword. Also, there is a well-understood relation @racket[(*o a b c)] which models facts of the form @${ab = c} where @racket[a], @racket[b], and @racket[c] are Oleg numbers. Putting these together yields a first attempt at @racket[multiple-of-3o], which I will call the @racket[3n] approach:

@racketblock[(defrel (multiple-of-3o-3n x)
               (fresh (n)
                      (*o '(1 1) n x)))]

Knowing that @racket['(1 1)] is the Oleg number representing the constant @racket[3], this miniKanren relation directly encodes the statement @($$ "\\exists n.3n = x")

Here are the first 15 multiples of 3 that this relation generates. Notice they are somewhat out of order, but all unique.

@racketblock[> (map base-ten (run 15 q (multiple-of-3o-3n q)))
             '(0 3 6 12 24 9 48 15 21 96 27 39 18 33 45)]

I am comfortable converting the results back to base-ten because this relation @bold{fully grounds} its argument. In fact, this is true for all four implentations. I was not able to find a relation expressing divisibility by 3 which leaves part of the Oleg number fresh. @margin-note{A value is @bold{fully ground} if it has no fresh variables.} 

A different version of the same statement can be created by swapping @racket[n] and @racket['(1 1)] in the relation definition. I'll call this the @racket[n3] approach.

@racketblock[(defrel (multiple-of-3o-n3 x)
               (fresh (n)
                      (*o n '(1 1) x)))

             > (map base-ten (run 15 q (multiple-of-3o-n3 q)))
             '(0 3 6 12 24 48 9 96 15 18 192 30 21 36 27)]

@section{Exploiting the structure of multiples of 3}

A base-10 number is divisible by 3 if the sum of its digits is divisible by 3. A similer criterion exists for Oleg numerals. According to  @url{https://stackoverflow.com/a/39386483/20284526}, an Oleg number (referred to in the StackOverflow answer as a binary number "from the right") is divisible by 3 if the sum of its odd-indexed bits minus the sum of its even-indexed bits is divisible by 3. Mathematically, @($$ "\\sum_{\\text{odd } i} x_i - \\sum_{\\text{even } i} x_i \\equiv 0 \\bmod 3") @margin-note{Since this is a necessary and sufficient condition, I'm pretty sure that any correct implementation of the relation must fully ground its results.}

For example, @racket['(1 1 0 1 0 0 0 0 1)] is divisible by 3 as demonstrated in the below picture.

@image["img/divisibility-of-267.png"]

Since this is a congruence mod 3, there are only a finite number of states to consider while adding up the odd- and even-indexed bits. At any point while scanning through the list of bits: 0 mod 3, 1 mod 3, and 2 mod 3. By considering these 3 possibilities, while also keeping track of whether the next bit is at an odd or even index, one can establish @($ "3 * 2 = 6") pairs of states such that, if the list is exhausted while on a 0 mod 3 state, the number is divisible by 3. Below is a deterministic finite automaton (DFA) which encodes this algorithm.

@image["img/divisibility-by-3-dfa-unminimized.png"]

While this DFA is correct, it can be minimized. I used an online tool at @url{https://aswaddev.github.io/dfa-minimizer/} to produce a DFA with only 3 states:

@image["img/divisibility-by-3-dfa-minimized.png"]

Converting a DFA into a miniKanren relation is straightforward: proceed recursively through the list while maintaining a state (here represented as a symbol), and only accept an empty list if the current state is an accepting state. The translation from DFA to miniKanren clauses is mostly direct with one exception: encountering a 0 requires the tail of the list to be nonempty (a pair instead of the empty list), since Oleg numerals cannot end in 0.

The DFA encoded as a relation, @racket[dfao] and its helper relation @racket[pairo]:

@racketblock[(defrel (dfao l state)
               (conde ((== l '()) (== state 'q1))

                      ((fresh (head tail next-state)
                              (== l `(,head . ,tail))

                              (conde ((== head 0)
                                      (pairo tail)
                                      (conde ((== state 'q1) (== next-state 'q1))
                                             ((== state 'q2) (== next-state 'q3))
                                             ((== state 'q3) (== next-state 'q2))))

                                     ((== head 1)
                                      (conde ((== state 'q1) (== next-state 'q2))
                                             ((== state 'q2) (== next-state 'q1))
                                             ((== state 'q3) (== next-state 'q3)))))

                              (dfao tail next-state)))))

             (defrel (pairo l)
               (fresh (a d) (== l `(,a . ,d))))]

So the divisibility relation can be defined by referencing @racket[dfao] on the starting state @racket['q1]. I'll call it the @racket[dfa] approach.

@racketblock[(defrel (multiple-of-3o-dfa x)
               (dfao x 'q1))

             > (map base-ten (run 15 q (multiple-of-3o-dfa q)))
             '(0 3 6 15 12 9 30 27 24 21 18 63 60 57 54)]

This looks unordered still, but look at the raw Oleg results:

@racketblock[> (run 15 q (multiple-of-3o-dfa q))
             '(()
               (1 1)
               (0 1 1)
               (1 1 1 1)
               (0 0 1 1)
               (1 0 0 1)
               (0 1 1 1 1)
               (1 1 0 1 1)
               (0 0 0 1 1)
               (1 0 1 0 1)
               (0 1 0 0 1)
               (1 1 1 1 1 1)
               (0 0 1 1 1 1)
               (1 0 0 1 1 1)
               (0 1 1 0 1 1))]

They are still ordered by length.

@section{Enumerating all multiples of 3}

My last approach is to encode the following mathematical statement: @($$ "x = 0 \\lor x = 3 \\lor x = 6 \\lor x = 9 \\lor x = 12 \\ldots")

This requires breaking the rules of miniKanren: creating a relation which relies on one of its arguments to always be ground. That way I can call functions (rather than relations) like @racket[build-num] and @racket[+], since I am dealing with a number and never a fresh variable. This hack relation is not worthy of the conventional @racket[-o] suffix.

@racketblock[(defrel (danger! x ground)
               (conde ((== x (build-num ground)))
                      ((danger! x (+ 3 ground)))))]

The divisibility relation can be defined by seeding an initial ground value of 0 to @racket[danger!]. I will suffix it with @racket[enum].

@racketblock[(defrel (multiple-of-3o-enum x)
               (danger! x 0))

             > (map base-ten (run 15 q (multiple-of-3o-enum q)))
             '(0 3 6 9 12 15 18 21 24 27 30 33 36 39 42)]

The results here are ordered.

@section{Start your engines}

For the first @bold{drag race}, I will run the following expression for each of the 4 implementations, and for increasing values of @racket[n]. Only the cpu time is considered.

@racketblock[(time (run n q (multiple-of-3o q)) (void))]

The @racket[(void)] call is a simple way to suppress the output of @racket[(run n ...)] while timing.

@section{Ready, set, go!}

@image["img/divisibility-by-3-generate.png"]

@tabular[#:sep @hspace[1]
         (list (list @racket[n] @racket[3n] @racket[n3] @racket[dfa] @racket[enum])
               (list "250"      "76"        "889"        "8"          "0")
               (list "500"      "168"       "3790"       "112"        "0")
               (list "750"      "410"       "8309"       "59"         "0")
               (list "1000"     "468"       "18165"      "54"         "1")
               (list "1250"     "1033"      "22868"      "68"         "1")
               (list "1500"     "1042"      "31357"      "123"        "1")
               (list "1750"     "1036"      "42626"      "136"        "3")
               (list "2000"     "1153"      "58484"      "130"        "3")
               (list "5000"     "8342"      "-"          "383"        "8")
               (list "10000"    "14033"     "-"          "1040"       "20")
               (list "15000"    "15919"     "-"          "1858"       "27")
               (list "20000"    "33743"     "-"          "2023"       "38")
               (list "25000"    "35274"     "-"          "5149"       "58")
               (list "30000"    "37538"     "-"          "5314"       "85"))]

I stopped measuring @racket[n3] early on because it was doing horribly. What takes @racket[enum] a handful of miliseconds takes nearly a minute for @racket[n3]. Omitting @racket[n3] for higher inputs allows seeing real differences among the other 3 implementations. One easy takeaway is that if you're comfortable doing a divisibility against a constant value by using the existential approach with @racket[*o], make sure the constant is the first value, not the second! It makes a huge difference.

So it looks like enum is the best choice, but that's only for generating values from a fresh variable. What if you want to use the relation to test if an already-fully-ground value is divisible by 3?

Here is the code template for the next drag race. Here we just want to find one answer, @racket[(_.0)], to indicate that the number @racket[x], (some variation of 999...) is indeed a multiple of 3.

@racketblock[(time (run 1 q (multiple-of-3o-enum (build-num x))))]

@image["img/divisibility-by-3-test.png"]

@tabular[#:sep @hspace[1]
         (list (list @racket[10ⁿ−1] @racket[3n] @racket[n3] @racket[dfa] @racket[enum])
               (list "1"            "0"         "1"         "0"          "0")
               (list "2"            "0"         "7"         "0"          "0")
               (list "3"            "1"         "775"       "0"          "0")
               (list "4"            "2"         "82604"     "0"          "1")
               (list "5"            "4"         "-"         "0"          "18")
               (list "6"            "10"        "-"         "0"          "163")
               (list "7"            "14"        "-"         "0"          "2329")
               (list "8"            "23"        "-"         "0"          "26567"))]

But now at higher values of @racket[x], @racket[enum] starts to do badly! That is becasue it fully enumerates every multiple of 3 in order, and there are many, many multiples of 3 that come before 99999999. @racket[dfa] remains speedy because it actually explores the Oleg number it is given, so its runtime depends only on the bitlength of the Oleg number rather than the value, which can be up to @($ "2^n") for some bitlength @($ "n"). @racket[3n] also seems to be faring fine, which surprised me.