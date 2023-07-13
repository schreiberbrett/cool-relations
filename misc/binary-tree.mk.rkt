#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(require pict)
(require pict/tree-layout)
(require pict/code)

#;(define-type BinaryTree (U False (Pairof BinaryTree BinaryTree)))

#;(: binary-treeo (<-> BinaryTree))
(defrel (binary-treeo t)
  (conde ((== t #f))
         ((fresh (l r)
                 (== t `(,l . ,r))
                 (binary-treeo l)
                 (binary-treeo r)))))

#;(: display-tree (-> BinaryTree PictImage))
(define (display-tree t)
  (define (f t)
    (match t
      (`(,l . ,r) (tree-layout (f l) (f r)))
      (#f #f)
      (fresh-symbol (tree-layout #:pict (code (unsyntax fresh-symbol))))))
  (if (equal? t #f) (code #f) (naive-layered (f t)))
  #;(naive-layered (f t)))

#;(: display-trees (-> (Listof BinaryTree) PictImage))
(define (display-trees ts)
  (apply ht-append (map display-tree ts)))


#;(: uniono (<-> BinaryTree BinaryTree BinaryTree))
(defrel (uniono t1 t2 tu)
  (fresh (l1 l2 lu r1 r2 ru)
         (conde ((== t1 #f) (== t2 tu))
                ((== t2 #f) (== t1 tu))
                ((== t1 `(,l1 . ,r1))
                 (== t2 `(,l2 . ,r2))
                 (== tu `(,lu . ,ru))

                 (uniono l1 l2 lu)
                 (uniono r1 r2 ru)))))