# Intro

This chapter details miniKanren relations which describe types and data structures. The relations should not be used directly, because they they can ground variables too soon. Instead, use these as a pattern match for each clause.

Related to the logical rule:

```
  ((p ∧ a)     ∨ (¬p ∧ b))
∧ ((p ∧ x)     ∨ (¬p ∧ y))
-----------------------
  ( p ∧ a ∧ x) ∨ (¬p ∧ b ∧ y)
```