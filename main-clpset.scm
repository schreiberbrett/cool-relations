(load "../clpset-miniKanren/mk.scm")

#|
clpset-miniKanren does not have a 'defrel' macro, so we need to create one,
since all the below files use it. It suffices to make 'defrel' a synonym of
'define'.
|#

(define-syntax defrel
  (syntax-rules ()
    ((defrel (rel . args) . body)
     (define rel (lambda args . body)))))

;(load "../faster-minikanren/numbers.scm")
;(load "cool-relations.html")
;(load "cool-relations-faster-minikanren.html")
(load "cool-relations-clpset.html")
