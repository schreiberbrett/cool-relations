#lang racket

#;(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(require "../../faster-minikanren/main.rkt")

(define (wordleo exl l c r exr)
  (conde ((== l '()) (== r '()) (== c '()) (== exl '()) (== exr '()))

         ((fresh (lₕ rₕ lₜ rₜ cₜ exlₜ exrₜ)
                 (== l `(,lₕ . ,lₜ))
                 (== r `(,rₕ . ,rₜ))

                 (conde ((==  lₕ rₕ) (== c `(,lₕ . , cₜ)) (== exl exlₜ) (== exr exrₜ))
                        ((=/= lₕ rₕ) (== c cₜ) (== exl `(,lₕ . ,exlₜ)) (== exr `(,rₕ . ,exrₜ))))

                 (wordleo exlₜ lₜ cₜ rₜ exrₜ)))))
  