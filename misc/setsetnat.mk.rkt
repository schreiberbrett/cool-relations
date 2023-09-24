#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(defrel (pairo x)
  (fresh (h t) (== x `(,h . ,t))))

#;(defrel (oro b1 b2 bo)
    (conde ((== b1 1) (== bo 1))
           ((== b2 1) (== bo 1))
           ((== b1 0) (== b2 0) (== bo 0))))
         
#;(: disjoint-uniono (<-> BinNum BinNum BinNum))
(defrel (disjoint-uniono l1 l2 lo)
  (fresh (h1 h2 ho t1 t2 to)
         (conde (;; When both are empty
                 (== l1 '()) (== l2 '()) (== lo '()))

                (;; When l1 is empty and l2 is nonempty
                 (== l1 '()) (pairo l2) (== lo l2))
         
                (;; When l1 is nonempty and l2 is empty
                 (pairo l1) (== l2 '()) (== lo l1))
         

                (;; When both are nonempty
                 (== l1 `(,h1 . ,t1))
                 (== l2 `(,h2 . ,t2))
                 (== lo `(,ho . ,to))

                 (conde ((== h1 1) (== ho 1))
                        ((== h2 1) (== ho 1))
                        ((== h1 0) (== h2 0) (== ho 0) (pairo t1) (pairo t2) (pairo to)))

                 (disjoint-uniono t1 t2 to)))))


#;(: converto (<-> BinNum (Listof UnNum))) ; The list of unary numbers must be sorted
; Diverges on
#;(run 2 (q) (converto q '((s s)) '(s)))
(defrel (converto b l nat)
  (fresh (n rest-b rest-l)
         (conde ((== b '()) (== l '()))
                ((== b `(,n . ,rest-b))

                 (conde (; When n = 0, l does not contain nat
                         (== n 0) (pairo rest-b) (== l rest-l))

                        (; When n = 1, l does contain nat
                         (== n 1) (== l `(,nat . ,rest-l))))

                 

                 (converto rest-b rest-l `(s . ,nat))))))


                 