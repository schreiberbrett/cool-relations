make # Tmp

this is temporary

```scheme
;;
;; Jason Hemann and Brysen Pfingsten
;;
;; Computes the non-zero backwards-binary numbers where the sum of odd
;; indices minus the sum of the even indices, mod 3 is zero.
;;
;; Relies on two help relations that track other remainders.
;;

(defrel (same-counto bn)
  (conde
    [(== bn `(1 1))]
    [(fresh (a ad dd)
       (== `(,a ,ad . ,dd) bn)
       (conde
         [(== a ad) (same-counto dd)]
         [(== `(,a ,ad) '(1 0)) (mod+1o dd)]
         [(== `(,a ,ad) '(0 1)) (mod+2o dd)]))]))

(defrel (mod+1o bn)
  (conde
    [(== bn `(0 1))]
    [(fresh (a ad dd)
       (== `(,a ,ad . ,dd) bn)
       (conde
         [(== a ad) (mod+1o dd)]
         [(== `(,a ,ad) '(1 0)) (mod+2o dd)]
         [(== `(,a ,ad) '(0 1)) (same-counto dd)]))]))

(defrel (mod+2o bn)
  (conde
    [(== bn '(1))]
    [(fresh (a ad dd)
       (== `(,a ,ad . ,dd) bn)
       (conde
         [(== a ad) (mod+2o dd)]
         [(== `(,a ,ad) '(1 0)) (same-counto dd)]
         [(== `(,a ,ad) '(0 1)) (mod+1o dd)]))]))

#|

(time (begin (run 10000 (q) (same-counto q)) #t))
cpu time: 86 real time: 88 gc time: 15
#t

|#

(defrel (multiple-of-threeo bn)
  (conde
    [(== bn '())]
    [(same-counto bn)]))
```

And some additional definitions for myself.

```scheme
;; Adapted from The Reasoned Schemer 2nd Ed, page 104, frame 120.
(defrel (lengtho l n)
  (conde ((== l '()) (== n '()))
         ((fresh (a d res)
            (== l `(,a . ,d))
            (pluso '(1) res n)
            (lengtho d res)))))
```


I've been playing around with your fresh `multiple-of-threeo` and my naive fully-grounding relation, by using the following functions below. (`lengtho` is from TS2E, page 104, frame 120).
```scheme            
(define (f n)
  (length (run* (q)
    (lengtho q (build-num n))
    (multiple-of-threeo q))))
    
(define (g n)
  (length (run* (q)
    (lengtho q (build-num n))
    (fresh (q/3) (*o '(1 1) q/3 q)))))
```

This means that `multiple-of-threeo` can describe all miniKanren numbers of bitlength `n` with `f(n)` solutions. Whereas the naive version needs `g(n)` solutions.

Results:

```
> (map f '(0 1 2 3 4 5 6 7 8 9 10 11 12 13))
'(1 0 1 1 2 3 6  9  18 27 54  81  162 243)
> (map g '(0 1 2 3 4 5 6 7 8 9 10 11 12 13))
'(1 0 1 1 3 5 11 21 43 85 171 341 683 1365)
```

It looks to me like, for n > 3, f(n) = A038754(n - 3), and g(n) = A001045(n - 1). [*]

I figured out that lim n -> infty A038754(n - 3) / A001045(n - 1) = 0. So f(n) grows asymptotically slower than g(n). I think that means the multiple-of-threeo solution set is asymptotically fresher than any multiple-of-three relation definition which fully grounds its argument.

I believe this means that a 4-bit version which freshens more would be even better. And a 6-bit version, better still. And an 8-bit version. I'm starting to think there's a way to write a multiple-of-three relation without grounding any of the digits (except the final 1). More to come.

[*] https://oeis.org/A038754, https://oeis.org/A001045

Best,

Brett Schreiber




If timing relations is a drag race, then comparing is an arm wrestle.

```math
\lim_{n → ∞} \frac{f(n)}{g(n)} = \lim_{n → ∞} \frac{A038754(n - 3)}{A001045(n - 1)}
```

```math
\lim_{n → ∞} \frac{A038754(n - 3)}{A001045(n - 1)} = 
```


Table of results:

| n  | f(n) | g(n) | A038754(n) | A038754(n - 3) | A001045(n) | A001045(n - 1) |
|----|------|------|------------|----------------|------------|----------------|
| 0  | 1    | 1    | 1          | -              | 0          | -              |
| 1  | 1    | 1    | 2          | -              | 1          | 0              |
| 2  | 1    | 1    | 3          | -              | 1          | 1              |
| 3  | 1    | 1    | 6          | 1              | 3          | 1              |
| 4  | 2    | 3    | 9          | 2              | 5          | 3              |
| 5  | 3    | 5    | 18         | 3              | 11         | 5              |
| 6  | 6    | 11   | 27         | 6              | 21         | 11             |
| 7  | 9    | 21   | 54         | 9              | 43         | 21             |
| 8  | 18   | 43   | 81         | 18             | 85         | 43             |
| 9  | 27   | 85   | 162        | 27             | 171        | 85             |
| 10 | 54   | 171  | 243        | 54             | 341        | 171            |
| 11 | 81   | 341  | 486        | 81             | 683        | 341            |
| 12 | 162  | 683  | 729        | 162            | 1365       | 683            |
| 13 | 243  | 1365 | 1458       | 243            | 2731       | 1365           |
| 14 | 486  | 2731 | 2187       | 486            | 5461       | 2731           |


It seems like, for n > 3, f(n) = A038754(n - 3), and g(n) =  A001045(n - 1).



Starting from the same `n = 3`, the naive relation looks a lot like the [Jacobsthal numbers](https://oeis.org/A001045): a(n) = nearest integer to 2^n/3.

So the question is, does freshening pairwise-bits produce asymptotically fewer results?


