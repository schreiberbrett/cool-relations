# Context free grammars in miniKanren

The language of balanced angle brackets, e.g.

```
'(< > < < > > < < > < > >)
```

As a Chomsky normal form grammar (with the empty string omitted).

```scheme
(defrel (balanced-brackets-grammaro g)
  (== g '((B1 -> B B)
          (B1 -> O M)
          (B1 -> O C)
          (B -> B B)
          (B -> O M)
          (B -> O C)
          (M -> B C)
          (O -> <)
          (C -> >))))
```

Right now, this relation only supports grammars that are in [Chomksy normal form](https://en.wikipedia.org/wiki/Chomsky_normal_form) which additionally don't admit the empty string.

`recognizeo`: Given grammar `g`, and starting from the nonterminal symbol `A`, reach the string `sA`.
```scheme
(defrel (recognizeo g A sA)
  (fresh (rule B C a sB sC)
    (membero rule g)
    (conde ((== rule `(,A -> ,a))
            (== sA `(,a)))
           ((== rule `(,A -> ,B ,C))
            (appendo sB sC sA)
            (recognizeo g B sB)
            (recognizeo g C sC)))))
```

## Results

```
> (run 10 (q)
    (fresh (g)
      (balanced-brackets-grammaro g)
      (recognizeo g 'B1 q)))
'((< >)
  (< < > >)
  (< > < >)
  (< < < > > >)
  (< > < < > >)
  (< < < > < > >)
  (< > < < > < >)
  (< < > > < >)
  (< < > > < < > >)
  (< > < < < > > >))
```

Can it run backwards?

```
> (time (run 1 (g)
          (recognizeo g 'B1 '(< >))
          (recognizeo g 'B1 '(< > < >))
          (recognizeo g 'B1 '(< > < > < >))
          (recognizeo g 'B1 '(< < < > > > < >))))
cpu time: 18953 real time: 18989 gc time: 9289
'(((B1 -> B1 B1) (B1 -> <) (B1 -> >) . _.0))
```

After thinking for 18 seconds, it gave me the language of all positive even-length strings over the alphabet `'(< >)`. Close enough, for now.



