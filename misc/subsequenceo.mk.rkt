#lang racket

(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(include "../../CodeFromTheReasonedSchemer2ndEd/trs2-arith.scm")

(require "riffleo.mk.rkt")

(defrel (dangerous! x ground)
  (conde ((== x (build-num ground)))
         ((dangerous! x (+ 3 ground)))))

(defrel (subsequenceo l1 l2 s)
  (fresh (x1 x2)
         (riffleo s x1 l1)
         (riffleo s x2 l2)))


#;(run 10 (l1 l2 s) (subsequenceo l1 l2 s))
#;'((()
     ()
     ())

    (()
     (_0 . _1)
     ())
    
    ((_0 . _1)
     (_0 . _1)
     (_0 . _1))
    
    ((_0 . _1)
     ()
     ())

    ((_0 . _1)
     (_2 . _3)
     ())

    ((   _0 . _1)
     (_2 _0 . _1)
     (   _0 . _1))
    
    ((_0)
     (_0 _1 . _2)
     (_0))
    
    ((_0 _1 . _2)
     (   _1 . _2)
     (   _1 . _2))

    ((      _0 . _1)
     (_2 _3 _0 . _1)
     (      _0 . _1))
    
    ((_0    _1 . _2)
     (_0 _3 _1 . _2)
     (_0    _1 . _2)))


#;(run 10 (l1 l2) (subsequenceo l1 l2 '(a b c)))
#;'(((a b c)
     (a b c))
    
    ((   a b c)
     (_0 a b c))

    ((_0 a b c)
     (   a b c))
    
    ((a    b c)
     (a _0 b c))
    
    ((      a b c)
     (_0 _1 a b c))
    
    ((_0 a b c)
     (_1 a b c))
    
    ((a b c)
     (a b c _0 . _1))

    ((a b    c)
     (a b _0 c))
    
    ((a _0 b c)
     (a    b c))
    
    ((   a    b c)
     (_0 a _1 b c)))


