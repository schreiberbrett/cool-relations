# Length

I've been having a lot of trouble implementing a complete  and relational `lengtho` relation.
This relation would be useful, because it bridges that gap between Peano and Oleg numbers.

More information in the Scribble HTML.

```scheme
(defrel (lengtho/trs2e l n)
  (conde ((== l '()) (== n '()))
         ((fresh (a d n-1)
            (== l `(,a . ,d))
            (+1o n-1 n)
            (lengtho/trs2e d n-1)))))
```

This definition seems good, but this query diverges:

```
> (run 2 (q) (lengtho/trs2e '(x x x x) q))
```

In the line `(+1o n-1 n)`, `n-1` is fresh, since it is introduced with `fresh` and first appears in this goal. Moreover, `n` is bound to `q`, which is also fresh. So it succeeds infinitely many times.

Here's another implementation which length-instantiates `l`, then uses the host language to assert its length via `project`.

```scheme
(defrel (lengtho/project l n)
  (listo l)
  (project (l) (== n (build-num (length l)))))
```

It fares better

```
"> (run* (q) (lengtho/project '(x x x x) q))
'((0 0 1))
> (run 10 (a b) (lengtho/project a b))
'((() ())
  ((_.0) (1))
  ((_.0 _.1) (0 1))
  ((_.0 _.1 _.2) (1 1))
  ((_.0 _.1 _.2 _.3) (0 0 1))
  ((_.0 _.1 _.2 _.3 _.4) (1 0 1))
  ((_.0 _.1 _.2 _.3 _.4 _.5) (0 1 1))
  ((_.0 _.1 _.2 _.3 _.4 _.5 _.6) (1 1 1))
  ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7) (0 0 0 1))
  ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8) (1 0 0 1)))
```

But this diverges when the length is known and the list is fresh.

```
> (run 1 (q) (lengtho/project q '(1 0 0 1)))
'((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8))
> (run 2 (q) (lengtho/project q '(1 0 0 1)))
...
```

