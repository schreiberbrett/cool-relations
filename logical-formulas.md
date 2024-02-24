# Logical formulas  I have found useful

## 1

* If `P(x) ∨ P(y)`
* Then: `∃z.(z⇔x ∨ z⇔y) ∧ P(z)`

Use this to reduce blowup in a first order representation of miniKanren. Generalizes to factoring, e.g. `ax + bx = (a + b)x`.

## 2

* If: `((p ∧ x₁) ∨ (¬p ∧ y₁)) ∧ ((p ∧ x₂) ∨ (¬p ∧ y₂))`
* Then: `(p ∧ x₁ ∧ x₂) ∨ (¬p ∧ y₁ ∧ y₂)`

Skips a very wide intermediate step. Useful in miniKanren when you are recurring through the same data structure twice.

## 3

* If: `(b ∨ p) ∧ (¬p ∨ ¬q) ∧ (q ∨ d)`
* Then: `b ∨ d`

This is useful to prove that a graph is not 3-colorable. Suppose you are coloring it down and you've narrowed it down that one of `b` or `p` must be colored green, and one of `q` or `d` must be colored green. And suppose there is an edge `(p, q)` in the graph. Then one of `p` or `q` must not be green, because they cannot both be green. From this you can deduce that one of `b` or `d` must be colored green. A kind of symmetric [resolution](https://en.wikipedia.org/wiki/Resolution_(logic)).

## 4

* Tautology: `((a ↔ b) ↔ (b ↔ a))`

I like the symmetry.

## 5

* Premise, exactly one of `x₁, x₂, x₃` is true: `(x₁ ∨ x₂ ∨ x₃) ∧ (¬x₁ ∨ ¬x₂) ∧ (¬x₂ ∨ ¬x₃) ∧ (¬x₃ ∨ ¬x₁)`
* Premise, exactly one of `y₁, y₂, y₃` is true: `(y₁ ∨ y₂ ∨ y₃) ∧ (¬y₁ ∨ ¬y₂) ∧ (¬y₂ ∨ ¬y₃) ∧ (¬y₃ ∨ ¬y₁)`
* Premise, exactly one of `z₁, z₂, z₃` is true: `(z₁ ∨ z₂ ∨ z₃) ∧ (¬z₁ ∨ ¬z₂) ∧ (¬z₂ ∨ ¬z₃) ∧ (¬z₃ ∨ ¬z₁)`
* Premise: `x₁ ∨ y₁ ∨ z₁`
* Conclusion 1: `(¬x₁ ∨ ¬y₁) ∧ (¬y₁ ∨ ¬z₁) ∧ (¬z₁ ∨ ¬x₁)`
* Conclusion 2: `(¬x₁ ∨ ¬y₁) ∧ (¬y₁ ∨ ¬z₁) ∧ (¬z₁ ∨ ¬x₁)`
If `x`, `y` and `z` are vertices of a 3-colorable graph, and it must be the case that one of them is colored `1`, then  
