# Statically-typed relations

If relations were statically typed, what would `appendo` look like?

Using Haskell syntax, we know that:

```haskell
append :: [a] -> [a] -> [a]
```

Let's figure out what the relational version might look like. A first attempt yields:

```haskell
appendo :: [([a], [a], [a])]
```

The infinite list of all triples such that the third item is the first and second. But this solution ignores the possibliity of fresh variables. Look at the result of the first 5 entries in the appendo infinite table:

```
> (run 5 (a b c) (appendo a b c))
'((() _0 _0)
  ((_0) _1 (_0 . _1))
  ((_0 _1) _2 (_0 _1 . _2))
  ((_0 _1 _2) _3 (_0 _1 _2 . _3))
  ((_0 _1 _2 _3) _4 (_0 _1 _2 _3 . _4)))
```
There are lots of fresh variables! The contents of the first argument, the elements of the list, remain fresh (which Haskell's `[a]` takes care of), but the second list altogether remains fresh; it is itself not a list, rather, it represents *any* list.

Here's a second attempt at the type signature of appendo:

```haskell
appendo :: [(Freshable [Freshable a],
             Freshable [Freshable a],
             Freshable [Freshable a])]
```

Where `Freshable a` is defined as:

```haskell
data Freshable a = Fresh Int
                 | Ground a
```


