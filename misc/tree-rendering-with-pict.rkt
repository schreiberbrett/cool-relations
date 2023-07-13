#lang racket

(require pict)
(require pict/tree-layout)


;; A tree is either a root, #f, or a 2-ary list (,l ,r) of 2 subtrees

(define (tree->image tree)
  (naive-layered (tree->tree-layout tree)))

(define (tree->tree-layout tree)
  (match tree
    (`(,l ,r) (tree-layout (tree->tree-layout l)
                           (tree->tree-layout r)))
    (#f (tree-layout #:pict (filled-ellipse 10 10 #:color "black")))))


(tree->image '((#f (#f #f))
               (#f #f)))