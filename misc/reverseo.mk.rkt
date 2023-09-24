;; Relationship between two lists which are permutations of each other

#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(require "appendo.mk.rkt")

(provide reverseo)

(defrel (reverseo xyz zyx)
  (conde ((== xyz '()) (== zyx '()))
         
         ((fresh (x yz zy)
                 (== xyz `(,x . ,yz))
                 (snoco zy x zyx)
                 (reverseo yz zy)))))

(defrel (snoco ab c abc)
  (conde ((== ab '()) (== abc `(,c)))
         ((fresh (a b bc)
                 (== ab `(,a . ,b))
                 (== abc `(,a . ,bc))
                 (snoco b c bc)))))

#;(run 2 (q) (reverseo q '(a b c)))
#;'(((c b a)))

#;(run 2 (q) (reverseo '(a b c) q))
; Diverges

; Notice the conjunction:
#;(snoco zy x zyx)
#;(reverseo yz zy)
; has a zy in common

; Generalizing to:
#;(snoco abc d abcd)
#;(reverseo abc cba)

(defrel (reverse-and-snoco abc cba d abcd)
  ; Case split on abc, since it is in common to both relations
  (conde (; When abc is empty, its reverse is empty
          (== abc '())
          (== cba '())

          ; And abc contributes nothing to the snoc
          (== abcd '(,d)))

         ((fresh (a bc bcd)
                 (== abc `(,a . ,bc))
                 (== abcd `(,a . ,bcd))

                 ;; A pesky extra snoco!
                 (snoco cb a cba)

                 (reverse-and-snoco bc cb d bcd)))))
                 
;; When DOES conjunction of recursive calls with common arguments work?