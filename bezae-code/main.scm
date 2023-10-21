
'(defrel (appendo l r o)
    (conde
        ((== l '()) (== r o))
        ((fresh (h t rec)
            (== l `(,h . ,t))
            (== o `(,h . ,rec))
            (appendo t r rec)))))
