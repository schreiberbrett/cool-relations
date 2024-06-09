# Polymorphism in miniKanren
2024-03-22

There are two great ways of representing natural numbers in miniKanren: Peano numbers (like `'(s s s))` and Oleg numbers (like `'(0 1 1)`). 

Here's a way to use their relations generically.

```scheme
(defrel (nato s n)
  (conde ((== s 'peano) (peanoo n))
         ((== s 'oleg)  (olego n))))
```

This relation is called `leqo` to distinguish from the predefined `<=o`, which is specific to Oleg numerals.

```scheme
(defrel (leqo s n m)
  (conde ((== s 'peano) (== n m))
         ((== s 'peano) (lesso n m))
         ((== s 'oleg)  (<=o n m))))
```

`lesso` is adapted from [Kiselyov, et al](http://webyrd.net/arithm/arithm.pdf)).
```scheme
(defrel (lesso n m)
  (fresh (n-1 m-1)
    (== m `(s . ,m-1))
    (conde ((== n '()))
           ((== n `(s . ,n-1))
            (lesso n-1 m-1)))))
```


Addition

```scheme
(defrel (+o s n m n+m)
  (conde ((== s 'peano) (appendo n m n+m))

         ((== s 'oleg)  (pluso n m n+m))))
```

Multiplication

This relation is called `multo` to distinguish from the predefined `*o`, which is specific to Oleg numerals.

```scheme
#;(defrel (multo s n m n*m)
  (conde ((== s 'peano)  (peano-*o n m n*m))

         ((== s 'oleg)   (*o n m n*m))))
```

`peano-*o` is adapted from `mul/3` in [Kiselyov, et al](http://webyrd.net/arithm/arithm.pdf)).

```scheme
(defrel (peano-*o n m n*m)
  (conde ((== n '()) (== n*m '()))
         ((pairo n) (== m '()) (== n*m '()))
         ((fresh (n-1 m-1 n*m-n n*m-m)
            (== n `(s . ,n-1))
            (== m `(s . ,m-1))
            (+o 'peano n n*m-n n*m)
            (peano-*o n-1 m n*m-m)))))
```


Successorship

```

(defrel (+1o3 s n n+1)
   (conde ((== s 'peano) (== `(s . ,n) n+1))
   
          ((== s 'int)   (== `(+ . ,n) n+1))
          ((== s 'int)   (== n `(- . ,n-1)))))
```
