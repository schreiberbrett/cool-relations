# A rule of inference
Brett Schreiber, 2023-01-02

Consider the following premises and conclusion:

Premises:
- $x \lor p$
- $y \lor q$
- $\neg x \lor  \neg y$

Conclusion:
- $p \lor q$

Note that $\neg a \lor \neg b$ in the above formula can be expressed equivalently (via [DeMorgan's law](https://en.wikipedia.org/wiki/De_Morgan%27s_laws)) as $\neg (a \land b)$, or even more compactly as $a ⊼ b$, where $⊼$ represents the logical NAND symbol.
 
---
Here is another argument that uses the same rule of inference:
Premises:
- $x_1 \lor x_2 \lor x_3 \lor p$
- $y_1 \lor y_2 \lor y_3 \lor q$
- $(\neg x_1 \lor \neg y_1) \land (\neg x_1 \lor \neg y_2) \land (\neg x_1 \lor \neg y_3)$
- $(\neg x_2 \lor \neg y_1) \land (\neg x_2 \lor \neg y_2) \land (\neg x_2 \lor \neg y_3)$
- $(\neg x_3 \lor \neg y_1) \land (\neg x_3 \lor \neg y_2) \land (\neg x_3 \lor \neg y_3)$

Conclusion:
- $p \lor q$
---
The general formula of this rule of inference is:
Premises:
- $(\bigvee_{x}^{X} x) \lor p$
- $(\bigvee_{y}^{Y} z) \lor q$
- $\bigwedge_{x}^{X} \bigwedge_{y}^{Y} (\neg x \lor \neg y)$

Conclusion:
- $p \lor q$