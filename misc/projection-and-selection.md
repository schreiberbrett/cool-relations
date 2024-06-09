# Projection and Selection

In relational algebra, projection and selection are operations on relations. There are similar concepts in miniKanren which I will give the same names.

## Projection

In some sense, `listo` is a projection of the first parameter of `appendo`. Consider the following statements. They are equivalent up to ordering.

```
(run* (q) (listo q))

(run* (q) (fresh (y z) (appendo q y z)))
```

In fact this third statement is also equivalent.

```
(run* (q) (listo q)
          (fresh (y z) (appendo q y z)))
```

Mechanically proving this projection is difficult. This is the closest thing I have to a proof.
1. `listo` is a relation containing 1 conde clause, whose only argument `x` is either `'()` or `\`(,h . ,t)`, recurring with its tail as its only argument.
2. `appendo` is a relation containing 1 conde clause, whose first argument `l` is either `'()` or `\`(,a . ,d)`, recurring with its tail as its first argument.
3. These two statements are morally the same, so `listo` is a projection of `appendo` on its first argument.

## Selection

Consider the following relations:

```scheme
(defrel (unary-naturalo n)
  (conde ((== n '()))
         ((fresh (rec) (== n `(s . ,rec))
                       (unary-naturalo rec)))))

(defrel (unary-eveno n)
  (conde ((== n '()))
         ((fresh (rec) (== n `(s s . ,rec))
                       (unary-eveno rec)))))
```

Any `q` that satisfies `(unary-eveno q)` also must satisfy `(unary-naturalo q)`. Equivalently,
```
(run* (q) (unary-eveno q))
```
is a strict subset of
```
(run* (q) (unary-naturalo q))
```
up to ordering.

So you could say that `unary-eveno` is a selection of the even-length rows of `unary-naturalo`.

Now consider the binary equivalents to the two relations.

```scheme
(defrel (binary-naturalo n)
  (conde ((== n '()))
         ((fresh (h t)
                 (== n `(,h . ,t))
                 (conde ((== h 0) (pairo t))
                        ((== h 1)))
                 (binary-naturalo t)))))

(defrel (binary-eveno n)
  (conde ((== n '()))
         ((fresh (t)
                 (== n `(0 . t))
                 (binary-naturalo t)))))
```
This selection I can only prove via the definition of even binary numbers.

Finally consider:

```scheme
(defrel (topo x)
  (== #t #t))

(defrel (bottomo)
  (== #t #f))
```
Every 1-argument relation is a selection of `topo`, and `bottomo` is a selection of every 1-argument relation, since:
```
> (run* (q) (topo q))
((_0))

> (run* (q) (bottomo q))
()
```

And this is a clue to how free variables should fit into all this: they should act as variables for all values, where values are a recursive data type defined as integers, booleans, symbols, and pairs of values.

## Unresolved questions

How should a relation with duplicate solutions be considered?
1. Duplicates don't violate projection/selection.
2. Both relations need to have the same duplicates (i.e., there must be a one-to-one correspondence).
3. We shouldn't consider relations with duplicates; always correct the relation first.
