#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

(require "pairo.mk.rkt")

(provide nat-neqo)

; Two natural numbers are unequal if one is longer than the other,
; or if they differ in one place
(defrel (nat-neqo n1 n2)
  (conde (; Either the nats are different lengths...
          (pairo n1) (== n2 '()))
         ((== n1 '()) (pairo n2))

         ((fresh (h1 h2 t1 t2)
                 (== n1 `(,h1 . ,t1))
                 (== n2 `(,h2 . ,t2))

                 (conde (; ... or, their heads are are unequal ...
                         (== h1 0) (== h2 1) (pairo t1))
                        ((== h1 1) (== h2 0) (pairo t2))

                        (; ... or, their heads are equal, but their tails are unequal.
                         (== h1 h2) (nat-neqo t1 t2)))))))

                        
#;(run 10 (n m) (nat-neqo n m))
#;'(((_0 . _1)
     ())

    (()
     (_0 . _1))

    ((0 _0 . _1)
     (1 . _2))
    
    ((1 . _0)
     (0 _1 . _2))

    ((_0 _1 . _2)
     (_0))
    
    ((_0)
     (_0 _1 . _2))
    
    ((_0 0 _1 . _2)
     (_0 1 . _3))
    
    ((_0 1 . _1)
     (_0 0 _2 . _3))
    
    ((_0 _1 _2 . _3)
     (_0 _1))
    
    ((_0 _1)
     (_0 _1 _2 . _3)))

#;(run 2 (q) (nat-neqo '(0 1 0 1) '(0 1 1)))
#;'((_0))

;; Notice that these nats are different in 3 places, but it succeeds only once.
#;(run 3 (q) (nat-neqo '(0 0 0 1) '(1 1 1 1)))
#;'((_0))


;; There are an infinite number of nats unequal to '(0 1), but they can be encoded finitely. Notice the illegal nat representation ((0)).
#;(run* (q) (nat-neqo q '(0 1)))
#;'((()) ((1 . _0)) ((0)) ((0 0 _0 . _1)) ((0 1 _0 . _1)))