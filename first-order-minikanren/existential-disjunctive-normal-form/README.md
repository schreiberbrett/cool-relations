# Existential Disjunctive Normal Form

A logical formula is said to be in **disjunctive normal form** if it has exactly two levels of nesting: an $n$-ary conjunction of $n$ disjunctions. See [this Wolfram Mathworld article](https://mathworld.wolfram.com/DisjunctiveNormalForm.html) for an example in propositional logic. In order to align better with miniKanren, my own examples are in first-order logic without negation.

I'd like to augment a logical formula in disjunctive normal form with exactly one existential quantifier placed at the beginning. A formula like this is said to be in **existential disjunctive normal form**.

Here is an example of a formula in existential disjunctive normal form:
```math
\begin{aligned}
    \exists_{a_1, a_2, a_3, b_1, b_2, b_3, c_1, c_2, c_3} &(P(a_1) \land Q(a_2) \land R(a_3)) \\
    \lor &(P(b_1) \land Q(b_2) \land R(b_3)) \\
    \lor &(P(c_1) \land Q(c_2) \land R(c_3))
\end{aligned}
```

An expression in miniKanren can be in existential disjunctive normal form as well. All that means is that the expression has the shape `(fresh (_) (conde (_) (_) (_) ... (_)))`. For convenience, I will call miniKanren expressions in this form "EDNFs".

An EDNF:
```scheme
(fresh (a1 a2 a3 b1 b2 b3 c1 c2 c3)
    (conde
        ((po a1) (qo a2) (ro a3))
        ((po b1) (qo b2) (ro b3))
        ((po c1) (qo c2) (ro c3))))
```

Converting an unstructured miniKanren expression into an EDNF can be computationally expensive, but its doable. The main source of blowup converting conjunction of two EDNFs into a singular EDNF, because this requires performing the cartesian product in order to group together every combination of inner conjunctions.

# Disjunction and conjunction of two formulas in existential disjunctive normal form
## Disjunction
```math
\begin{aligned}
  (\exists_{a, b}
    &(P_1(a) \land P_2(b)) \\
  \lor
    &(Q_1(a) \land Q_2(b)) \\
  \lor
    &(R_1(a) \land R_2(b))
) \\
\lor \\
(
  \exists_{c, d}
    &(P_1(c) \land P_2(d)) \\
  \lor
    &(Q_1(c) \land Q_2(d)) \\
  \lor
    &(R_1(c) \land R_2(d))
) \\\\
=
(
  \exists_{a, b, c, d}
    &(P_1(a) \land P_2(b)) \\
  \lor
    &(Q_1(a) \land Q_2(b)) \\
  \lor
    &(R_1(a) \land R_2(b)) \\
  \lor
    &(P_1(c) \land P_2(d)) \\
  \lor
    &(Q_1(c) \land Q_2(d)) \\
  \lor
    &(R_1(c) \land R_2(d))
)
\end{aligned}
```

## Conjunction
```math
\begin{aligned}
  (\exists_{a, b}
    &(P_1(a) \land P_2(b)) \\
  \lor
    &(Q_1(a) \land Q_2(b)) \\
  \lor
    &(R_1(a) \land R_2(b))
) \\
\land \\
(
  \exists_{c, d}
    &(P_1(c) \land P_2(d)) \\
  \lor
    &(Q_1(c) \land Q_2(d)) \\
  \lor
    &(R_1(c) \land R_2(d))
) \\\\
=
(
  \exists_{a, b, c, d}
    &(P_1(a) \land P_2(b) \land P_1(c) \land P_2(d)) \\
  \lor
    &(Q_1(a) \land Q_2(b) \land P_1(c) \land P_2(d)) \\
  \lor
    &(R_1(a) \land R_2(b) \land P_1(c) \land P_2(d)) \\\\
  \lor
    &(P_1(a) \land P_2(b) \land Q_1(c) \land Q_2(d)) \\
  \lor
    &(Q_1(a) \land Q_2(b) \land Q_1(c) \land Q_2(d)) \\
  \lor
    &(R_1(a) \land R_2(b) \land Q_1(c) \land Q_2(d)) \\\\
  \lor
    &(P_1(a) \land P_2(b) \land R_1(c) \land R_2(d)) \\
  \lor
    &(Q_1(a) \land Q_2(b) \land R_1(c) \land R_2(d)) \\
  \lor
    &(R_1(a) \land R_2(b) \land R_1(c) \land R_2(d))
)
\end{aligned}
```
