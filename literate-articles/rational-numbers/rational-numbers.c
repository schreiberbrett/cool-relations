struct Rational {
    int numerator;
    int denominator;
};

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


