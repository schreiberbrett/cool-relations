(load "mk-compiler.scm")
(load "~/CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")

;; P(x) ∨ P(y) ⇒ ∃α.(α≡x ∨ α≡y) ∧ P(α)
(define test-case-1
    '(defrel (test-case-1 x y)
        (conde
            ((P x))
            ((P y)))))

;; (P(x) ∧ Q(y)) ∨ (P(z) ∧ R(w)) ⇒ ∃α.((α≡x ∧ Q(y)) ∨ (α≡z ∧ R(w))) ∧ P(α)
(define test-case-2
    '(defrel (test-case-2 x y z w)
        (conde
            ((P x) (Q y))
            ((P z) (R w)))))

;; P(x, y) ∨ P(z, w) ⇒ ∃αβ.((α≡x ∧ β≡y) ∨ (α≡z ∧ β≡w)) ∧ P(α, β)
(define test-case-3
    '(defrel (test-case-3 x y z w)
        (conde
            ((P x y))
            ((P z w)))))

;;   (A(x₁) ∧ P(x₂) ∧ Q(x₃))
;; ∨ (B(y₁) ∧ P(y₂) ∧ Q(y₃))
;; ∨ (C(z₁) ∧ Q(z₂) ∧ R(z₃))
;; ∨ (D(w₁) ∧ Q(w₂) ∧ R(w₃))
;; ⇒
;; etc.
(define test-case-4
    '(defrel (test-case-4 x1 x2 x3 y1 y2 y3 z1 z2 z3 w1 w2 w3)
        (conde
            ((A x1) (P x2) (Q x3)       )
            ((B y1) (P y2) (Q y2)       )
            ((C z1)        (Q z2) (R z3))
            ((D w1)        (Q w2) (R w3)))))





(define test-case-riffle
    '(defrel (riffleo l r o)
        (fresh (h t rec)
            (conde
                ((== l '()) (== r '()) (== o '()))
                ((== l `(,h . ,t)) (== o `(,h . ,rec)) (riffleo t r rec))
                ((== r `(,h . ,t)) (== o `(,h . ,rec)) (riffleo l t rec))))))

(defrel (riffleo l r o)
    (fresh (h t rec)
        (conde
            ((== l '()) (== r '()) (== o '()))
            ((== l `(,h . ,t)) (== o `(,h . ,rec)) (riffleo t r rec))
            ((== r `(,h . ,t)) (== o `(,h . ,rec)) (riffleo l t rec)))))


(defrel-optimized (riffleo^ l r o)
    (fresh (h t rec)
        (conde
            ((== l '()) (== r '()) (== o '()))
            ((== l `(,h . ,t)) (== o `(,h . ,rec)) (riffleo t r rec))
            ((== r `(,h . ,t)) (== o `(,h . ,rec)) (riffleo l t rec)))))


(defrel (double-riffleo q)
    (fresh (a b)
        (riffleo a b '(a 1 b 2 c 3 4 5 6))
        (riffleo q b '(4 x 5 6))))