# 3-SAT

I've tried to create a 3-SAT recognizer in miniKanren. 3-SAT is is the set of all satisfiable statements such as

```math
( x₀ ∨  x₁ ∨ ¬x₂) ∧
(¬x₀ ∨  x₂ ∨  x₃) ∧
(¬x₁ ∨ ¬x₂ ∨ ¬x₃)
```

A 3-SAT formula in miniKanren is a list of triples where at least one clause of every triple has its assigned value.

```minikanren
(require "../faster-minikanren/main.rkt")

(defrel (3sato cnf assignments)
  (conde ((== cnf '()))
         ((fresh (c1 c2 c3 rest var val)
            (== cnf `((,c1 ,c2 ,c3) . ,rest))
            (conde ((== c1 `(,var ,val)))
                   ((== c2 `(,var ,val)))
                   ((== c3 `(,var ,val))))
            (lookupo assignments var val)
            (3sato rest assignments)))))
```

We can treat `assignments` as a `Map<Nat, Bool>`. If it were an association list, then miniKanren could cheat and assign the same variable two different values. This version of `lookupo` expects a binary tree.

```minikanren
(defrel (>0o l)
  (fresh (a d)
    (== l `(,a . ,d))))

(defrel (lookupo map k v)
  (fresh (l x r a d next)
    (== map `(,x ,l ,r))
    (conde ((== k '()) (== x v))
           ((== k `(,a . ,d))
            (conde ((== a 0) (>0o d) (== next l))
                   ((== a 1) (== next r)))
            (lookupo next d v)))))
```

The inorder representation I was using earlier was hard to visually parse, so I switched the representation from `'(,l ,x ,r)` to `'(,x ,l ,r) `.

Trying the example:
```
>(run 1 (q) (3sato '(((()  #t) ((1)   #t) ((0 1) #f))
                     ((()  #f) ((0 1) #t) ((1 1) #t))
                     (((1) #f) ((0 1) #f) ((1 1) #f))) q))

'((#t _.0 (#f _.1 (#t _.2 _.3))))
```

which corresponds to the assignment:

```math
{ x₀ ← T,
  x₁ ← F,
  x₃ ← T }
```

`run*` works here, too (not shown).

Running `3sato` backwards:

```
>(run 5 (q) (fresh (x y z w)
   (3sato q `(#t ,x (#f ,y (#t ,z ,w))))))
   
'(()
  (((() #t) _.0 _.1))
  ((_.0 (() #t) _.1))
  (((() #t) _.0 _.1) ((() #t) _.2 _.3))
  ((_.0 _.1 (() #t))))
```

Which corresponds to these 5 CNFs.

```math
()
(x₀  ∨ ... ∨ ...)
(x₀  ∨ ... ∨ ...) ∧ (x₀ ∨ ... ∨ ...)
(... ∨ ... ∨ x₀ )
```

I'm starting to get a headache from all the parens. I'll try some unsatisfiable CNFs later.

