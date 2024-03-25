# One-to-one relationships

miniKanren can model one-to-one relationships.


## Fahrenheit and Celsius
For example, every Fahrenheit temperature has exactly one corresponding Celsius temperature.

```math
c = (f - 32) * 5/9 \\
9c = 5(f - 32)
```

```scheme
(defrel (temperatureo f c)
  (fresh (f-32 9c)
    (pluso f-32 (build-num 32) f)
    (*o (build-num 9) c 9c)
    (*o (build-num 5) f-32 9c)))
```

## Odds to probability

...

## Godel numbering

Per [Wikipedia](https://en.wikipedia.org/wiki/G%C3%B6del_numbering#mw-content-text/div/dl/dd/span/img):

```math
\text{enc}(x_1, x_2, x_3, \dots, x_n) = 2^{x_1} \dot 3^{x_2} \dot 5^{x_3} \dots p_n^{x_n}
```

## Unary and Binary Nats

todo

## Prime factorization

See [prime-factorization.md](prime-factorization.md).

## The power of `once`

todo
