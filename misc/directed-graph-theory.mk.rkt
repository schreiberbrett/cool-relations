#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

#;(require "riffleo.mk.rkt")

(require "natset.mk.rkt")

#;(require "../../faster-minikanren/mk.rkt")



; Consider a digraph to be a map from nat (vertex number) to natset (its neighbors)
(defrel (edgeo digraph u v)
  (fresh (neighbors)
         (entryo u neighbors digraph)
         (entryo v #t digraph)))


#;(compile (defrel (patho digraph u v)
             (conde ((edgeo digraph u v))
                    ((fresh (x)
                            (edgeo digraph u x)
                            (patho digraph x v))))))

#;(compile (defrel (patho digraph u v)
             (fresh (z0 z1)
                    (edgeo digraph z0 z1)
                    (conde ((== `(,u ,v) `(,z0 ,z1)))
                           ((fresh (x)
                                   (== `(,u ,x) `(,z0 ,z1))
                                   (patho digraph x v)))))))

(defrel (patho digraph u v)
  (fresh (z1)
         (edgeo digraph u z1)
         (conde ((== v z1))
                ((fresh (x)
                        (== x z1)
                        (patho digraph x v))))))


;; There is a cycle involving v somewhere in the digraph.
(defrel (cycleo digraph v)
  (patho digraph v v))