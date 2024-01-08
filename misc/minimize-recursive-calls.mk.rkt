#lang racket

(require "../../faster-minikanren/mk.rkt")
#;(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(defrel (all-appleo l)
  (conde ((== l '()))
         ((fresh (d)
                 (== l `(apple . ,d))
                 (all-appleo d)))))

(defrel (all-bananao l)
  (conde ((== l '()))
         ((fresh (d)
                 (== l `(banana . ,d))
                 (all-bananao d)))))

#;(time (run 15 (x y)
             (conde ((all-appleo x) (all-appleo y))
                    ((all-appleo x) (all-bananao y))
                    ((all-bananao x) (all-appleo y))
                    ((all-bananao x) (all-bananao y))))
        (void))


#;(time (run 15 (x y)
             (conde ((all-appleo x))
                    ((all-bananao x)))
             (conde ((all-appleo y))
                    ((all-bananao y))))
        (void))
