@William Byrd, your 2024 goals have inspired me to share ideas before they're perfect. So I'd like to continue posting to this channel, here with an idea on how to represent sets of natural numbers in miniKanren.

I want to start with the simplest thing I can think of: testing a subset of the naturals for membership. I'm still thinking about how to implement testing for non-membership.

If you were to ask me which subset of the naturals contains 4, and 0, and 3, I'd say {0 ... 3, 4, ... }.

!(Picture)[img/relational-natset.png]

```minikanren
(defrel (>0o n)
  (fresh (a d) (== n `(,a . ,d))))

(defrel (elemo n s)
  (fresh (l m r)
    (== s `(,l ,m ,r))
    (conde ((== n '()) (== m #t))
           ((fresh (a d rec)
              (== n `(,a . ,d))
              (conde ((== a 0) (>0o d) (== rec l))
                     ((== a 1) (== rec r)))
              (elemo d rec))))))
```

```
> (run* (q) (elemo '(0 0 1) q)
            (elemo '() q)
            (elemo '(1 1) q))
'((((_.0 _.1 (_.2 #t _.3)) _.4 _.5) #t (_.6 _.7 (_.8 #t _.9))))
``` 
