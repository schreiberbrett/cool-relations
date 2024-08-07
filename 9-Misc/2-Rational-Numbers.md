# Introduction
This is a document on programming rational numbers. This document is presented in a literate programming style, which has two consequences:
1. The code in this essay will make use of procedures whose definitions implemented further down in the document. Often these are mathematical functions that have familiar definitions, so their appearance should not make the code they are used in confusing.
2. The full, final version of this source code is is available here as `src.c`.

@s How to work with exact rational numbers in C

When implementing rational numbers in programming, the most important fact is that adding, subtracting, or multiplying two rational numbers will always result in a rational number. This behavior is called [closure](https://en.wikipedia.org/wiki/Closure_(mathematics)).

```c
// Add, subtract, and multiply have the following types, since rationals are closed under these operations.

struct Rational      add(struct Rational, struct Rational);
struct Rational subtract(struct Rational, struct Rational);
struct Rational multiply(struct Rational, struct Rational);
```

Division is more difficult because of the impossibility of dividing by zero, even though zero is a perfectly fine rational number. It will be introduced later.

A function to compute the greatest common divisor is also required sot that the rational numbers returned are in reduced form.

```c
int greatest_common_divisor(int, int);
```

# Rational numbers

A rational number is any number that can be written as a *ratio* $\frac{a}{b}$ given whole numbers $a$ and $b$, known as the *numerator* and *denominator*. There are some other constraints (for example, the denominator cannot be zero), but those will be addressed later.

```c
struct Rational {
    int numerator;
    int denominator;
};
```

# Addition

The sum of two rational numbers is also a rational number, as shown by this formula.

$\frac{p_1}{q_1} + \frac{p_2}{q_2} = \frac{p_1q_2 + p_2q_1}{q_1q_2}$

But using the above formula directly can create fractions which need to be simplified. For example,

$\frac{3}{4} + \frac{1}{6} = \frac{3 * 6 + 1 * 4}{(4)(6)} = \frac{22}{24} (= \frac{11}{12})$

To simplify $\frac{22}{24}$, divide both the numerator and the denominator by their greatest common divisor (GCD). Here the GCD is 2. One famous method for computing the GCD is Euclid's algorithm. It is implemented in the appendix.

$\frac{22}{24} = \frac{11}{12}$.

```c
struct Rational simplify(struct Rational r) {
    int p = r.numerator;
    int q = r.denominator;

    int gcd = greatest_common_divisor(p, q);

    return (struct Rational) {
        .numerator = p / gcd,
        .denominator = q / gcd
    };
}

struct Rational add(struct Rational r1, struct Rational r2) {
    int p1 = r1.numerator;
    int q1 = r1.denominator;

    int p2 = r2.numerator;
    int q2 = r2.denominator;

    return simplify((struct Rational) {
        .numerator = p1 * q2 + p2 * q1,
        .denominator = q1 * q2
    });
}
```

# Multiplication

The formula for rational multiplication is:

$(\frac{p_1}{q_1})(\frac{p_2}{q_2}) = \frac{p_1q_1}{p_2q_2}$

The code implementation for this formula also needs a simplification step. Consider.

$(\frac{1}{2})(\frac{2}{3}) = \frac{(1)(2)}{(2)(3)} = \frac{2}{6} (= \frac{1}{3})$

```c
struct Rational multiply(struct Rational r1, struct Rational r2) {
    int p1 = r1.numerator;
    int q1 = r1.denominator;

    int p2 = r2.numerator;
    int q2 = r2.denominator;

    return simplify((struct Rational) {
        .numerator = p1 * p2,
        .denominator = q1 * q2
    });
}
```

# Appendix

```c
int greatest_common_divisor(int a, int b) {
    if (b == 0) {
        return a;
    }

    return greatest_common_divisor(b, a % b);
}
```