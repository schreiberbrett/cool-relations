# The Majority Relation

I really want to describe a relation between a value `x` and a list `l` where `x` is the majority (more than half) of all values in `l`.

An approach I've found works well is to use a helper relation `count-differenceo` that subtracts the number of occurrences of `x` from the number of non-occurrences of `x` in `l`. It works by recurring through `l` with an integer `counter` variable. Anytime `x` is encountered in `l`, increment the counter. Otherwise, decrement the counter. `majorityo` only succeeds if the difference is positive.

Notice I don't need a disequality check in the second `conde` branch, because as long as the majority value is more than half, there can be some "defectors".

```scheme
(defrel (majorityo x l)
  (fresh (difference)
    (>0o difference)
    (count-differenceo x l difference)))
```

Notice that `-1o` is really `+1o` with its arguments swapped.

```scheme
(defrel (-1o x x-1)
  (+1o x-1 x))
```

That mean there is a `+1o` in both `conde` branches. It can be factored out by introducing placeholder logic variables.

```scheme
(defrel (count-differenceo x l c)
  (conde ((== l '()) (== c '()))
         ((fresh (a d rec alpha1 alpha2)
            (== l `(,a . ,d))
            (conde ((== a x) (== `(,alpha1 ,alpha2) `(,rec ,c)))
                   ((== `(,alpha1 ,alpha2) `(,c ,rec))))
            (+1o alpha1 alpha2)
            (count-differenceo x d rec)))))
```

But how should integers be represented in miniKanren, and how should `+1o` be defined over them?

Today, I was interested in writing the successor relation over a representation of the integers in miniKanren.

Here was my first attempt: Encode the integers like so:

```
...
 4: '(+ + + +)
 3: '(+ + +)
 2: '(+ +)
 1: '(+)
 0: '()
-1: '(-)
-2: '(- -)
-3: '(- - -)
-4: '(- - - -)
...
```

Then the successor relation can be described as follows:

```scheme
(defrel (+1o x x+1)
  (conde ((== x   `(- . ,x+1)))
         ((== x+1 `(+ . ,x)))))
```

But this is incorrect. The below query says that there are two predecessors of 2. And the first result is not a well-formed integer.

```
> (run* (q) (+1o q '(+ +)))
'((- + +)
  (+))
```

Using a different representation of integers, which uses a unary number represented as a list, plus the tags `'(1 0 0)`, `'(0 1 0)`, and `'(0 0 1)` representing positive, zero, and negative, respectively.

```
...
 4: '((1 0 0) (o o o o))
 3: '((1 0 0) (o o o))
 2: '((1 0 0) (o o))
 1: '((1 0 0) (o))
 0: '((0 1 0) ())
-1: '((0 0 1) (o))
-2: '((0 0 1) (o o))
-3: '((0 0 1) (o o o))
-4: '((0 0 1) (o o o o))
...
```

There are two cases of the successor relation:
1. If a number is nonnegative, then its successor must be positive, with one additional unary digit.
2. If a number is negative, then its successor must not be positive, with one less unary digit.
```scheme
(defrel (+1o2 x x+1)
  (fresh (p z n l)
    (conde ((== x `((,p ,z 0) ,l)) (== x+1 `((1 0 0) (o . ,l))))
           ((== x `((0 0 1) (o . ,l))) (== x+1 `((0 ,z ,n) ,l))))))
```

Lets see how this fares:

```
> (run* (q) (+1o2 q '((1 0 0) (o o))))
'(((_.0 _.1 0) (o)))
```

One result!
