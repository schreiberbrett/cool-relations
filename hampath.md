# Hamiltonian Path in miniKanren

```minikanren
(require "../faster-minikanren/main.rkt")
```

A throwaway relation, `か`, that asserts:
* `u` is a starting vertex
* `s` is the set of vertices left to visit
* `g` is a directed graph containing `{u}∪s`
* `n = |s|`


```minikanren
(defrel (か-with-countdown u s g n)
  (conde ((== n '()) (emptyo s))
         ((fresh (v rest n-1)
            (== n `(o . ,n-1))
            (conjo v #t rest s)
            (edgeo u v g)
            (か-with-countdown v rest g n-1)))))
```


```minikanren
(defrel (か-w/o-countdown u s g)
  (conde ((emptyo s))
         ((fresh (v rest)
            (conjo v #t rest s)
            (edgeo u v g)
            (か-w/o-countdown v rest g)))))
```

Where `conjo` is the relation that adds an element to (or, picks an element from) a dict

```minikanren
(defrel (conjo k v s s+1)
  (!elemo k s)
  (elemo k v s+1))
```

Where `elemo` and `!elemo` are defined as follows:

```minikanren
(defrel (>0o n)
  (fresh (a d) (== n `(,a . ,d))))

(defrel (elemo k v s)
  (fresh (val l r)
    (== s `(,val ,l ,r))
    (conde ((== k '()) (== val v))
           ((fresh (a d rec)
              (== k `(,a . ,d))
              (conde ((== a 0) (>0o d) (== rec l))
                     ((== a 1) (== rec r)))
              (elemo d v rec))))))

;; Adapted from code by Raffi Sanna
(defrel (!elemo n s)
  (fresh (l r)
    (conde ((== s '()))
           ((== n '()) (== s `(#f ,l ,r)))
           ((fresh (val b rec)
              (== s `(,val ,l ,r))
              (conde ((== n `(0 . ,b)) (>0o b) (== rec l))
                     ((== n `(1 . ,b)) (== rec r)))
              (!elemo b rec))))))
```

If a graph `g` is a map of its vertices to its neighbors, then `edgeo` would assert that `g` has vertex `u` whose neighbors include `v`.

```minikanren
(defrel (edgeo u v g)
  (fresh (neighbors)
    (elemo u neighbors g)
    (elemo v #t neighbors)))
```

THe only map that can be empty is the empty map.

```minikanren
(defrel (emptyo m)
  (== m '()))
```

The definition of directed graphs that have a hamiltonian path:

```minikanren
(defrel (has-hamiltonian-patho g)
  (fresh (starting-vertex neighbors)
    (elemo starting-vertex neighbors g)
     (か-w/o-countdown starting-vertex neighbors g)))
```

```minikanren
(defrel (hampatho g)
  (conde ((emptyo g))
         ((fresh (v neighbors)
            (conjo v neighbors g)
            (hampatho neighbors)))))
```
