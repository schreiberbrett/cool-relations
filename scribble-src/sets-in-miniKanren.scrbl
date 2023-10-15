#lang scribble/manual

@section{Sets in miniKanren}

A set of natural numbers in miniKanren can be represented as a sorted list of Oleg numerals. Therefore, adding an element to a set would be like inserting into a list.

@racketblock[(defrel (inserto n sᵢ sₒ)
               (conde ((== sᵢ '()) (== sₒ `(,n)))
                      ((fresh (a d sᵣ)
                              (== sᵢ `(,a . ,d))

                              (conde ((<o n a) (== sₒ `(,n ,a . ,d))))))))]
                                    

