#lang racket

(require 2htdp/image)

;; A tree is either a root, #f, or a 2-ary list (,l ,r) of 2 subtrees

(define (tree->image tree)
  (match tree
    (`(,l ,r) (above (circle 20 'outline 'black)
                     (beside/align 'top
                                   (tree->image l)
                                   (tree->image r))))
    (#f (center-pinhole (circle 20 'solid 'black)))))


(tree->image '((#f (#f #f))
               (#f #f)))

