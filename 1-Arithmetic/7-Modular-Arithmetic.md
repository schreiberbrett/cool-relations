# Modular Arithmetic

## Congruence

The relation $a \equiv b \mod k$ ought to hold for any Oleg numerals `a`, `b`, and `k`.

One easy answer is to use multiplication. $a \equiv b \mod k \iff ak = bk$.

```scheme
(defrel (congruento/* a b k)
  (fresh (product)
    (*o a k product)
    (*o b k product)))
```


```scheme
(defrel (congruento a b k)
  (fresh (a$ b$ a$R b$R)
    (interleaveo a a$)
    (interleaveo b b$)
    (reverseo a$ a$R)
    (reverseo b$ b$R)
    (olego k)
    (project (k)
      (let ((dfa (make-dfa k)))
        (fresh (end-state)
          (dfao a$R dfa '() end-state)
          (dfao b$R dfa '() end-state))))))
```

### Helpers

Interleave a special symbol `$` between every Oleg bit.

```scheme
(defrel (interleaveo n n$)
  (conde ((== n '()) (== n$ '()))
         ((== n '(1)) (== n$ '(1)))
         ((fresh (a d d$)
            (== n `(,a . ,d))
            (pairo d)
            (== n$ `(,a $ . ,d$))
            (interleaveo d d$)))))
```

Example:

```
> (run 7 (a a$) (interleaveo a a$))
'((() ())
  ((1) (1))
  ((_.0 1) (_.0 $ 1))
  ((_.0 _.1 1) (_.0 $ _.1 $ 1))
  ((_.0 _.1 _.2 1) (_.0 $ _.1 $ _.2 $ 1))
  ((_.0 _.1 _.2 _.3 1) (_.0 $ _.1 $ _.2 $ _.3 $ 1))
  ((_.0 _.1 _.2 _.3 _.4 1) (_.0 $ _.1 $ _.2 $ _.3 $ _.4 $ 1)))
```
