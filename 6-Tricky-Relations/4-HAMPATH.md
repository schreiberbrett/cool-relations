# Hamiltonian Path in miniKanren

A throwaway relation, `か`, that asserts:
* `u` is a starting vertex
* `s` is the set of vertices left to visit
* `g` is a directed graph containing `{u}∪s`
* `n = |s|`


```scheme
(defrel (か-with-countdown u s g n)
  (conde ((== n '()) (emptyo s))
         ((fresh (v rest n-1)
            (== n `(o . ,n-1))
            (conjo v #t rest s)
            (edgeo u v g)
            (か-with-countdown v rest g n-1)))))
```


```scheme
(defrel (か-w/o-countdown u s g)
  (conde ((emptyo s))
         ((fresh (v rest)
            (conjo v #t rest s)
            (edgeo u v g)
            (か-w/o-countdown v rest g)))))
```

Where `conjo` is the relation that adds an element to (or, picks an element from) a dict

```scheme
(defrel (conjo k v s s+1)
  (!elemo k s)
  (elemo k v s+1))
```

If a graph `g` is a map of its vertices to its neighbors, then `edgeo` would assert that `g` has vertex `u` whose neighbors include `v`.

```scheme
(defrel (edgeo u v g)
  (fresh (neighbors)
    (elemo u neighbors g)
    (elemo v #t neighbors)))
```

THe only map that can be empty is the empty map.

```scheme
(defrel (emptyo m)
  (== m '()))
```

The definition of directed graphs that have a hamiltonian path:

```scheme
(defrel (has-hamiltonian-patho g)
  (fresh (starting-vertex neighbors)
    (elemo starting-vertex neighbors g)
     (か-w/o-countdown starting-vertex neighbors g)))
```

```scheme
(defrel (hampatho g)
  (conde ((emptyo g))
         ((fresh (v neighbors)
            (conjo v neighbors g)
            (hampatho neighbors)))))
```
