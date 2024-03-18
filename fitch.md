# Fitch-style proofs in miniKanren

Fitch-style proofs in miniKanren

```scheme
(defrel (proof^o premises pf)
  (conde ((== pf '()))
         ((fresh (a d phi reason x x1 x2 x1-reason x2-reason l r n m)
            (== pf `((,phi ,reason) . ,d))
            
            (conde ((== reason '(premise))
                    (membero phi premises))
            
                   ((== phi `(and ,x1 ,x2))
                    (== reason `(and-intro ,n ,m))
                    (list-refᵒ d n `(,x1 ,x1-reason))
                    (list-refᵒ d m `(,x2 ,x2-reason)))
                    
                   ((== reason `(and-elim-l ,n))
                    (list-refᵒ d n `(and ,phi ,r)))
                    
                   ((== reason `(and-elim-r ,n))
                    (list-refᵒ d n `(and ,l ,phi)))
                    
                   ((== phi `(or ,l ,r))
                    (== reason `(or-intro-l ,n))
                    (list-refᵒ d n l))
                    
                   ((== phi `(or ,l ,r))
                    (== reason `(or-intro-r ,n))
                    (list-refᵒ d n r)))
            (proof^o premises d)))))
            
(defrel (membero x l)
  (fresh (a d)
    (== l `(,a . ,d))
    (conde ((== x a))
           ((membero x d)))))

```

End of file.
