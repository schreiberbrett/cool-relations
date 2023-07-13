#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-arith.scm")

(require pict)
(require pict/tree-layout)
(require pict/code)


(defrel (pairo x)
  (fresh (h t) (== x `(,h . ,t))))

#;(define-type Nat (U Nil
                      (Pairof 0 (Pairof (U 0 1) Nat))
                      (Pairof 1 Nat)))

#;(define-type (NatMap a) (Option (List (Option a)
                                        (NatMap a)
                                        (NatMap a))))

#;(: entry (All (a) (<-> Nat a (NatMap a))))
(defrel (entryo k v m)
  (fresh (l r val rec other h t)
         (conde ((== k '()) (== m `(,v ,l ,r)))
                ((== k `(,h . ,t))
                 (conde ((== 0 h) (pairo t) (== m `(,val ,rec ,other)))
                        ((== 1 h) (== m `(,val ,other ,rec))))
                 (entryo t v rec)))))

#;(: inserto (All (a) (<-> Nat a (NatMap a) (NatMap a))))
(defrel (inserto k v old new)
  (== #t #t)) ;; TODO


#;(define-type (Or a b) (U (Pair False b)
                           (Pair a False)
                           (Pair a b)))


#;(: oro (<-> Boolean Boolean Boolean))
(defrel (oro l r o)
  (conde ((== o #t) (conde ((== l #t)) ((== r #t))))
         ((== o #f) (== l #f) (== r #f))))

#;(: ando (<-> Boolean Boolean Boolean))
(defrel (ando l r o)
  (conde ((== o #t) (== l #t) (== r #t))
         ((== o #f) (conde ((== l #f)) ((== r #f))))))


#;(: nonempty-seto (<-> (NatMap Boolean)))
(defrel (nonempty-seto set v l r)
  ;; Allow all but (#f #f #f)
  (== set `(,v ,l ,r))
  (fresh (a b c)   
         (conde ((== v #t))
                ((== l `(,a ,b ,c)))
                ((== r `(,a ,b ,c))))))

#;(: uniono (<-> (NatMap Boolean) (NatMap Boolean) (NatMap Boolean)))
(defrel (uniono m1 m2 mu)
  (fresh (v1 v2 vu l1 l2 lu r1 r2 ru)
         (conde ((== m1 #f) (== m2 #f) (== mu #f))
                ((nonempty-seto m1 v1 l1 r1) (== m2 #f) (nonempty-seto mu v1 l1 r1))
                ((== m1 #f) (nonempty-seto m2 v2 l2 r2) (nonempty-seto mu v2 l2 r2))
                ((nonempty-seto m1 v1 l1 r1)
                 (nonempty-seto m2 v2 l2 r2)
                 (nonempty-seto mu vu lu ru)
                 (oro v1 v2 vu)
                 (uniono l1 l2 lu)
                 (uniono r1 r2 ru)))))


#;(: intersectiono (<-> (NatMap Boolean) (NatMap Boolean) (NatMap Boolean)))
(defrel (intersectiono m1 m2 mi)
  (fresh (v1 v2 vi l1 l2 li r1 r2 ri a b c)
         (conde ((== m1 #f) (== mi #f))
                ((== m2 #f) (== mi #f))
                ((== m1 `(,v1 ,l1 ,r1))
                 (== m2 `(,v2 ,l2 ,r2))
                 
                 (ando v1 v2 vi)
                 (conde (; Prune the branch when mi is a false terminal
                         (== mi #f)
                         (== vi #f) (== li #f) (== ri #f))

                        (; Otherwise keep mi...
                         (== mi `(,vi ,li ,ri))
                         (conde (; ... when mi is a false terminal...
                                 (== vi #t) (== li #f) (== ri #f))
                                
                                (; ... or when mi is nonterminal
                                 (== li `(,a ,b ,c)))
                                ((== ri `(,a ,b b,c))))))
                 (intersectiono l1 l2 li)
                 (intersectiono r1 r2 ri)))))


(define (map->tree-layout map)
  (match map
    (`(,val ,l ,r) (tree-layout #:pict (code (unsyntax val))
                                (map->tree-layout l)
                                (map->tree-layout r)))
    #;(#f #f) ; Comment/uncomment to omit leaf nodes
    (fresh-var (tree-layout #:pict (code (unsyntax fresh-var))))))


(define (map->image map)
  (naive-layered (map->tree-layout map)))


(define my-set '(#t #f #f))

(define unions (run 20 (m1 m2 mu)
                    (== mu my-set)
                    (uniono m1 m2 mu)))

#;(apply ht-append (map map->image (last (run 150000 (a b c) (uniono a b c)))))
     

