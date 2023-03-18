(load "~/CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(define (time-suppress-output x) (time (and x #t)))

(define (fib n)
    (if (or (eq? n 0) (eq? n 1)) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(defrel (nilo x)
    (== x '()))

(defrel (conso h t x)
    (== x `(,h . ,t)))

(defrel (appendo l1 l2 lo)
    (conde
        ((nilo l1) (== l2 lo))
        ((fresh (h1 t1 ho to)
            (conso h1 t1 l1)
            (conso ho to lo)
            (== h1 ho)
            (appendo t1 l2 to)))))

#|
(defrel (riffleo l1 l2 lo)
    (fresh (h1 h2 t1 t2 to)
        (conde
            ((== l1 '())          (== l2 '())          (== lo '()))
            ((== l1 '())          (== l2 `(,h2 . ,t2)) (== lo `(,h2 . ,t2)))
            ((== l1 `(,h1 . ,t1)) (== l2 '())          (== lo `(,h1 . ,t1)))
            ((== l1 `(,h1 . ,t1)) (== l2 `(,h2 . ,t2)) (== lo `(,h1 . ,to)) (riffleo t1 l2 to))
            ((== l1 `(,h1 . ,t1)) (== l2 `(,h2 . ,t2)) (== lo `(,h2 . ,to)) (riffleo l1 t2 to)))))
|#


(defrel (riffleo l1 l2 lo)
    (fresh (h1 h2 t1 t2 to)
        (conde
            ((== l1 '())          (== l2 '())          (== lo '()))
            ((== l1 '())          (== l2 `(,h2 . ,t2)) (== lo `(,h2 . ,t2)))
            ((== l1 `(,h1 . ,t1)) (== l2 '())          (== lo `(,h1 . ,t1)))
            ((== l1 `(,h1 . ,t1)) (== l2 `(,h2 . ,t2))
                (fresh (x0 x1 x2 x3)
                    (conde
                        ((== x0 h1) (== x1 t1) (== x2 l2) (== x3 to))
                        ((== x0 h2) (== x1 l1) (== x2 t2) (== x3 to)))
                    (== lo `(,x0 . ,to))
                    (riffleo x1 x2 x3))))))


(defrel (riffleo* l o)
    (fresh (x y z n)
        (conde
            ((== l `(,x)) (== o x))
            ((== l `(,x ,y . ,z))
                (riffleo x y n)
                (riffleo* `(,n . ,z) o)))))

(defrel (resolutiono clauses)
    (fresh (c1 c2 co rest-clauses)
        (conde
            ((== clauses `(() . ,rest-clauses)))
            (
                (riffleo `(,c1 ,c2) rest-clauses clauses)
                (resolveo c1 c2 co)
                (resolutiono `(,co . ,clauses))))))

(defrel (resolveo c1 c2 co)
    (fresh (x c1^ c2^ c1_rest c2_rest)
        (riffleo `(,c1^) `(,c2^) `(,c1 ,c2))
        (riffleo `((,x)) c1_rest c1^)
        (riffleo `((not ,x)) c2_rest c2^)
        (appendo c1_rest c2_rest co)))


(define cnf-formula-0 '(
    ((a))
    ((not b))))

(define cnf-formula-1 '(
    ((a) (b))
    ((not a) (b))
    ((a) (not b))
    ((not a) (not b))))

(define cnf-formula-2 '(
    ((a) (b) (c))
    ((a) (b) (not c))
    ((a) (not b) (c))
    ((a) (not b) (not c))
    ((not a) (b) (c))
    ((not a) (b) (not c))
    ((not a) (not b) (c))
    ((not a) (not b) (not c))))

(defrel (reverseo l lo)
    (reverse-and-appendo l '() lo))

(defrel (reverse-and-appendo l1 l2 lo)
    (conde
        ((== l1 '()) (== l2 lo))
        ((fresh (h1 t1)
            (== l1 `(,h1 . ,t1))
            (reverse-and-appendo t1 `(,h1 . ,l2) lo)))))

(defrel (numbero n)
    (conde
        ((== n '()))
        ((fresh (h t)
            (== n `(,h . ,t))
            (not==0 h)))))

(defrel (not==0 digit)
    (conde
        ((== digit 1))
        ((== digit 2))
        ((== digit 3))
        ((== digit 4))
        ((== digit 5))
        ((== digit 6))
        ((== digit 7))
        ((== digit 8))
        ((== digit 9))))

(defrel (not==9 digit)
    (conde
        ((== digit 0))
        ((== digit 1))
        ((== digit 2))
        ((== digit 3))
        ((== digit 4))
        ((== digit 5))
        ((== digit 6))
        ((== digit 7))
        ((== digit 8))))

(defrel (+o x y z)
    (numbero x)
    (numbero y)
    (numbero z)
    (fresh (xR yR zR)
        (reverseo x xR)
        (reverseo y yR)
        (reverseo z zR)
        (help+o xR yR zR 0)))

(defrel (help+o x y z c)
    (fresh (d hx hy hz tx ty tz)
        (conde
            ((== x '()) (== y '()) (== c 0) (== z '()))
            ((== x '()) (== y '()) (== c 1) (== z `(,c)))
            ((== x `(,hx . ,tx)) (== y '()) (== c 0) (== x z))
            ((== x `(,hx . ,tx)) (== y '()) (== c 1) (+1o x z))
            ((== x '()) (== y `(,hy . ,ty)) (== c 0) (== y z))
            ((== x '()) (== y `(,hy . ,ty)) (== c 1) (+1o y z))
            (
                (== x `(,hx . ,tx))
                (== y `(,hy . ,ty))
                (== z `(,hz . ,tz))
                (digit+o c hx hy hz d)
                (help+o tx ty tz d)))))


(defrel (succo k n)
    (fresh (kR nR)
        (reverseo k kR)
        (reverseo n nR)
        (+1o kR nR)))

;; n is a reversed number, e.g. '(9 0 3)
;; Sn is the same number but plus one, e.g. '(0 1 3)
(defrel (+1o n Sn)
    (conde
        ((== n '()) (== Sn '(1)))
        ((fresh (hn tn hSn tSn)
            (== n `(,hn . ,tn))
            (== Sn `(,hSn . ,tSn))
            (conde
                ((== tn tSn) (not==9 hn) (not==0 hSn))
                ((== hn 9) (== hSn 0) (+1o tn tSn)))))))


(defrel (digit+o x a b out carry)
    (conde
        ((== x 0) (== a 0) (== b 0) (== carry 0) (== out 0))
        ((== x 0) (== a 0) (== b 1) (== carry 0) (== out 1))
        ((== x 0) (== a 0) (== b 2) (== carry 0) (== out 2))
        ((== x 0) (== a 0) (== b 3) (== carry 0) (== out 3))
        ((== x 0) (== a 0) (== b 4) (== carry 0) (== out 4))
        ((== x 0) (== a 0) (== b 5) (== carry 0) (== out 5))
        ((== x 0) (== a 0) (== b 6) (== carry 0) (== out 6))
        ((== x 0) (== a 0) (== b 7) (== carry 0) (== out 7))
        ((== x 0) (== a 0) (== b 8) (== carry 0) (== out 8))
        ((== x 0) (== a 0) (== b 9) (== carry 0) (== out 9))

        ((== x 0) (== a 1) (== b 0) (== carry 0) (== out 1))
        ((== x 0) (== a 1) (== b 1) (== carry 0) (== out 2))
        ((== x 0) (== a 1) (== b 2) (== carry 0) (== out 3))
        ((== x 0) (== a 1) (== b 3) (== carry 0) (== out 4))
        ((== x 0) (== a 1) (== b 4) (== carry 0) (== out 5))
        ((== x 0) (== a 1) (== b 5) (== carry 0) (== out 6))
        ((== x 0) (== a 1) (== b 6) (== carry 0) (== out 7))
        ((== x 0) (== a 1) (== b 7) (== carry 0) (== out 8))
        ((== x 0) (== a 1) (== b 8) (== carry 0) (== out 9))
        ((== x 0) (== a 1) (== b 9) (== carry 1) (== out 0))

        ((== x 0) (== a 2) (== b 0) (== carry 0) (== out 2))
        ((== x 0) (== a 2) (== b 1) (== carry 0) (== out 3))
        ((== x 0) (== a 2) (== b 2) (== carry 0) (== out 4))
        ((== x 0) (== a 2) (== b 3) (== carry 0) (== out 5))
        ((== x 0) (== a 2) (== b 4) (== carry 0) (== out 6))
        ((== x 0) (== a 2) (== b 5) (== carry 0) (== out 7))
        ((== x 0) (== a 2) (== b 6) (== carry 0) (== out 8))
        ((== x 0) (== a 2) (== b 7) (== carry 0) (== out 9))
        ((== x 0) (== a 2) (== b 8) (== carry 1) (== out 0))
        ((== x 0) (== a 2) (== b 9) (== carry 1) (== out 1))

        ((== x 0) (== a 3) (== b 0) (== carry 0) (== out 3))
        ((== x 0) (== a 3) (== b 1) (== carry 0) (== out 4))
        ((== x 0) (== a 3) (== b 2) (== carry 0) (== out 5))
        ((== x 0) (== a 3) (== b 3) (== carry 0) (== out 6))
        ((== x 0) (== a 3) (== b 4) (== carry 0) (== out 7))
        ((== x 0) (== a 3) (== b 5) (== carry 0) (== out 8))
        ((== x 0) (== a 3) (== b 6) (== carry 0) (== out 9))
        ((== x 0) (== a 3) (== b 7) (== carry 1) (== out 0))
        ((== x 0) (== a 3) (== b 8) (== carry 1) (== out 1))
        ((== x 0) (== a 3) (== b 9) (== carry 1) (== out 2))

        ((== x 0) (== a 4) (== b 0) (== carry 0) (== out 4))
        ((== x 0) (== a 4) (== b 1) (== carry 0) (== out 5))
        ((== x 0) (== a 4) (== b 2) (== carry 0) (== out 6))
        ((== x 0) (== a 4) (== b 3) (== carry 0) (== out 7))
        ((== x 0) (== a 4) (== b 4) (== carry 0) (== out 8))
        ((== x 0) (== a 4) (== b 5) (== carry 0) (== out 9))
        ((== x 0) (== a 4) (== b 6) (== carry 1) (== out 0))
        ((== x 0) (== a 4) (== b 7) (== carry 1) (== out 1))
        ((== x 0) (== a 4) (== b 8) (== carry 1) (== out 2))
        ((== x 0) (== a 4) (== b 9) (== carry 1) (== out 3))

        ((== x 0) (== a 5) (== b 0) (== carry 0) (== out 5))
        ((== x 0) (== a 5) (== b 1) (== carry 0) (== out 6))
        ((== x 0) (== a 5) (== b 2) (== carry 0) (== out 7))
        ((== x 0) (== a 5) (== b 3) (== carry 0) (== out 8))
        ((== x 0) (== a 5) (== b 4) (== carry 0) (== out 9))
        ((== x 0) (== a 5) (== b 5) (== carry 1) (== out 0))
        ((== x 0) (== a 5) (== b 6) (== carry 1) (== out 1))
        ((== x 0) (== a 5) (== b 7) (== carry 1) (== out 2))
        ((== x 0) (== a 5) (== b 8) (== carry 1) (== out 3))
        ((== x 0) (== a 5) (== b 9) (== carry 1) (== out 4))

        ((== x 0) (== a 6) (== b 0) (== carry 0) (== out 6))
        ((== x 0) (== a 6) (== b 1) (== carry 0) (== out 7))
        ((== x 0) (== a 6) (== b 2) (== carry 0) (== out 8))
        ((== x 0) (== a 6) (== b 3) (== carry 0) (== out 9))
        ((== x 0) (== a 6) (== b 4) (== carry 1) (== out 0))
        ((== x 0) (== a 6) (== b 5) (== carry 1) (== out 1))
        ((== x 0) (== a 6) (== b 6) (== carry 1) (== out 2))
        ((== x 0) (== a 6) (== b 7) (== carry 1) (== out 3))
        ((== x 0) (== a 6) (== b 8) (== carry 1) (== out 4))
        ((== x 0) (== a 6) (== b 9) (== carry 1) (== out 5))

        ((== x 0) (== a 7) (== b 0) (== carry 0) (== out 7))
        ((== x 0) (== a 7) (== b 1) (== carry 0) (== out 8))
        ((== x 0) (== a 7) (== b 2) (== carry 0) (== out 9))
        ((== x 0) (== a 7) (== b 3) (== carry 1) (== out 0))
        ((== x 0) (== a 7) (== b 4) (== carry 1) (== out 1))
        ((== x 0) (== a 7) (== b 5) (== carry 1) (== out 2))
        ((== x 0) (== a 7) (== b 6) (== carry 1) (== out 3))
        ((== x 0) (== a 7) (== b 7) (== carry 1) (== out 4))
        ((== x 0) (== a 7) (== b 8) (== carry 1) (== out 5))
        ((== x 0) (== a 7) (== b 9) (== carry 1) (== out 6))

        ((== x 0) (== a 8) (== b 0) (== carry 0) (== out 8))
        ((== x 0) (== a 8) (== b 1) (== carry 0) (== out 9))
        ((== x 0) (== a 8) (== b 2) (== carry 1) (== out 0))
        ((== x 0) (== a 8) (== b 3) (== carry 1) (== out 1))
        ((== x 0) (== a 8) (== b 4) (== carry 1) (== out 2))
        ((== x 0) (== a 8) (== b 5) (== carry 1) (== out 3))
        ((== x 0) (== a 8) (== b 6) (== carry 1) (== out 4))
        ((== x 0) (== a 8) (== b 7) (== carry 1) (== out 5))
        ((== x 0) (== a 8) (== b 8) (== carry 1) (== out 6))
        ((== x 0) (== a 8) (== b 9) (== carry 1) (== out 7))

        ((== x 0) (== a 9) (== b 1) (== carry 1) (== out 0))
        ((== x 0) (== a 9) (== b 0) (== carry 0) (== out 9))
        ((== x 0) (== a 9) (== b 2) (== carry 1) (== out 1))
        ((== x 0) (== a 9) (== b 3) (== carry 1) (== out 2))
        ((== x 0) (== a 9) (== b 4) (== carry 1) (== out 3))
        ((== x 0) (== a 9) (== b 5) (== carry 1) (== out 4))
        ((== x 0) (== a 9) (== b 6) (== carry 1) (== out 5))
        ((== x 0) (== a 9) (== b 7) (== carry 1) (== out 6))
        ((== x 0) (== a 9) (== b 8) (== carry 1) (== out 7))
        ((== x 0) (== a 9) (== b 9) (== carry 1) (== out 8))


        ((== x 1) (== a 0) (== b 0) (== carry 0) (== out 1))
        ((== x 1) (== a 0) (== b 1) (== carry 0) (== out 2))
        ((== x 1) (== a 0) (== b 2) (== carry 0) (== out 3))
        ((== x 1) (== a 0) (== b 3) (== carry 0) (== out 4))
        ((== x 1) (== a 0) (== b 4) (== carry 0) (== out 5))
        ((== x 1) (== a 0) (== b 5) (== carry 0) (== out 6))
        ((== x 1) (== a 0) (== b 6) (== carry 0) (== out 7))
        ((== x 1) (== a 0) (== b 7) (== carry 0) (== out 8))
        ((== x 1) (== a 0) (== b 8) (== carry 0) (== out 9))
        ((== x 1) (== a 0) (== b 9) (== carry 1) (== out 0))

        ((== x 1) (== a 1) (== b 0) (== carry 0) (== out 2))
        ((== x 1) (== a 1) (== b 1) (== carry 0) (== out 3))
        ((== x 1) (== a 1) (== b 2) (== carry 0) (== out 4))
        ((== x 1) (== a 1) (== b 3) (== carry 0) (== out 5))
        ((== x 1) (== a 1) (== b 4) (== carry 0) (== out 6))
        ((== x 1) (== a 1) (== b 5) (== carry 0) (== out 7))
        ((== x 1) (== a 1) (== b 6) (== carry 0) (== out 8))
        ((== x 1) (== a 1) (== b 7) (== carry 0) (== out 9))
        ((== x 1) (== a 1) (== b 8) (== carry 1) (== out 0))
        ((== x 1) (== a 1) (== b 9) (== carry 1) (== out 1))

        ((== x 1) (== a 2) (== b 0) (== carry 0) (== out 3))
        ((== x 1) (== a 2) (== b 1) (== carry 0) (== out 4))
        ((== x 1) (== a 2) (== b 2) (== carry 0) (== out 5))
        ((== x 1) (== a 2) (== b 3) (== carry 0) (== out 6))
        ((== x 1) (== a 2) (== b 4) (== carry 0) (== out 7))
        ((== x 1) (== a 2) (== b 5) (== carry 0) (== out 8))
        ((== x 1) (== a 2) (== b 6) (== carry 0) (== out 9))
        ((== x 1) (== a 2) (== b 7) (== carry 1) (== out 0))
        ((== x 1) (== a 2) (== b 8) (== carry 1) (== out 1))
        ((== x 1) (== a 2) (== b 9) (== carry 1) (== out 2))

        ((== x 1) (== a 3) (== b 0) (== carry 0) (== out 4))
        ((== x 1) (== a 3) (== b 1) (== carry 0) (== out 5))
        ((== x 1) (== a 3) (== b 2) (== carry 0) (== out 6))
        ((== x 1) (== a 3) (== b 3) (== carry 0) (== out 7))
        ((== x 1) (== a 3) (== b 4) (== carry 0) (== out 8))
        ((== x 1) (== a 3) (== b 5) (== carry 0) (== out 9))
        ((== x 1) (== a 3) (== b 6) (== carry 0) (== out 0))
        ((== x 1) (== a 3) (== b 7) (== carry 1) (== out 1))
        ((== x 1) (== a 3) (== b 8) (== carry 1) (== out 2))
        ((== x 1) (== a 3) (== b 9) (== carry 1) (== out 3))

        ((== x 1) (== a 4) (== b 0) (== carry 0) (== out 5))
        ((== x 1) (== a 4) (== b 1) (== carry 0) (== out 6))
        ((== x 1) (== a 4) (== b 2) (== carry 0) (== out 7))
        ((== x 1) (== a 4) (== b 3) (== carry 0) (== out 8))
        ((== x 1) (== a 4) (== b 4) (== carry 0) (== out 9))
        ((== x 1) (== a 4) (== b 5) (== carry 1) (== out 0))
        ((== x 1) (== a 4) (== b 6) (== carry 1) (== out 1))
        ((== x 1) (== a 4) (== b 7) (== carry 1) (== out 2))
        ((== x 1) (== a 4) (== b 8) (== carry 1) (== out 3))
        ((== x 1) (== a 4) (== b 9) (== carry 1) (== out 4))

        ((== x 1) (== a 5) (== b 0) (== carry 0) (== out 6))
        ((== x 1) (== a 5) (== b 1) (== carry 0) (== out 7))
        ((== x 1) (== a 5) (== b 2) (== carry 0) (== out 8))
        ((== x 1) (== a 5) (== b 3) (== carry 0) (== out 9))
        ((== x 1) (== a 5) (== b 4) (== carry 1) (== out 0))
        ((== x 1) (== a 5) (== b 5) (== carry 1) (== out 1))
        ((== x 1) (== a 5) (== b 6) (== carry 1) (== out 2))
        ((== x 1) (== a 5) (== b 7) (== carry 1) (== out 3))
        ((== x 1) (== a 5) (== b 8) (== carry 1) (== out 4))
        ((== x 1) (== a 5) (== b 9) (== carry 1) (== out 5))

        ((== x 1) (== a 6) (== b 0) (== carry 0) (== out 7))
        ((== x 1) (== a 6) (== b 1) (== carry 0) (== out 8))
        ((== x 1) (== a 6) (== b 2) (== carry 0) (== out 9))
        ((== x 1) (== a 6) (== b 3) (== carry 1) (== out 0))
        ((== x 1) (== a 6) (== b 4) (== carry 1) (== out 1))
        ((== x 1) (== a 6) (== b 5) (== carry 1) (== out 2))
        ((== x 1) (== a 6) (== b 6) (== carry 1) (== out 3))
        ((== x 1) (== a 6) (== b 7) (== carry 1) (== out 4))
        ((== x 1) (== a 6) (== b 8) (== carry 1) (== out 5))
        ((== x 1) (== a 6) (== b 9) (== carry 1) (== out 6))

        ((== x 1) (== a 7) (== b 0) (== carry 0) (== out 8))
        ((== x 1) (== a 7) (== b 1) (== carry 0) (== out 9))
        ((== x 1) (== a 7) (== b 2) (== carry 1) (== out 0))
        ((== x 1) (== a 7) (== b 3) (== carry 1) (== out 1))
        ((== x 1) (== a 7) (== b 4) (== carry 1) (== out 2))
        ((== x 1) (== a 7) (== b 5) (== carry 1) (== out 3))
        ((== x 1) (== a 7) (== b 6) (== carry 1) (== out 4))
        ((== x 1) (== a 7) (== b 7) (== carry 1) (== out 5))
        ((== x 1) (== a 7) (== b 8) (== carry 1) (== out 6))
        ((== x 1) (== a 7) (== b 9) (== carry 1) (== out 7))

        ((== x 1) (== a 8) (== b 0) (== carry 0) (== out 9))
        ((== x 1) (== a 8) (== b 1) (== carry 1) (== out 0))
        ((== x 1) (== a 8) (== b 2) (== carry 1) (== out 1))
        ((== x 1) (== a 8) (== b 3) (== carry 1) (== out 2))
        ((== x 1) (== a 8) (== b 4) (== carry 1) (== out 3))
        ((== x 1) (== a 8) (== b 5) (== carry 1) (== out 4))
        ((== x 1) (== a 8) (== b 6) (== carry 1) (== out 5))
        ((== x 1) (== a 8) (== b 7) (== carry 1) (== out 6))
        ((== x 1) (== a 8) (== b 8) (== carry 1) (== out 7))
        ((== x 1) (== a 8) (== b 9) (== carry 1) (== out 8))

        ((== x 1) (== a 9) (== b 0) (== carry 1) (== out 0))
        ((== x 1) (== a 9) (== b 1) (== carry 1) (== out 1))
        ((== x 1) (== a 9) (== b 2) (== carry 1) (== out 2))
        ((== x 1) (== a 9) (== b 3) (== carry 1) (== out 3))
        ((== x 1) (== a 9) (== b 4) (== carry 1) (== out 4))
        ((== x 1) (== a 9) (== b 5) (== carry 1) (== out 5))
        ((== x 1) (== a 9) (== b 6) (== carry 1) (== out 6))
        ((== x 1) (== a 9) (== b 7) (== carry 1) (== out 7))
        ((== x 1) (== a 9) (== b 8) (== carry 1) (== out 8))
        ((== x 1) (== a 9) (== b 9) (== carry 1) (== out 9))))



(defrel (pluso n m k)
    (conde
        ((== n '()) (== m k))
        ((fresh (n^ k^)
            (== n `(S . ,n^))
            (== k `(S . ,k^))
            (pluso n^ m k^)))))

(defrel (multo n m k)
    (conde
        ((== m '()) (== k '()))

        ((fresh (m^ k^)
            (== m `(S . ,m^))
            (pluso n k^ k)
            (multo n m^ k^)))))


(defrel (poopo dnf)
    (conde
        ((fresh (rest)
            (riffleo '(()) rest dnf)))

        ((fresh (x p^ q^ p q p-1 q-1 p-1q-1 rest)
            (riffleo `((not ,x)) q-1 q)
            (riffleo `(,p^ ,q^) rest dnf)
            (riffleo `(,p) `(,q) `(,p^ ,q^))
            (riffleo `(,x) p-1 p)
            ;;(appendo p-1 q-1 p-1q-1)
            ;;(poopo `(,p-1q-1 . ,rest))
            
            
            ))))


(defrel (eveno n)
  (conde
    ((== n '()))
    ((fresh (n-1)
       (== n `(s . ,n-1))
       (fresh (n)
         (== (list n) (list n-1))
         (fresh (n-1)
           (== n `(s . ,n-1))
           (fresh (n)
             (== (list n) (list n-1))
             (conde
               ((== n '()))
               ((fresh (n-1)
                  (== n `(s . ,n-1))
                  (fresh (n)
                    (== (list n) (list n-1))
                    (fresh (n-1)
                        (== n `(s . ,n-1))
                        (eveno n-1)))))))))))))

(defrel (fibo n o)
    (conde
        ((== n '()) (== o '(s)))
        ((== n '(s)) (== o '(s)))
        ((fresh (n-1 n-2 o-1 o-2)
            (== `(s . ,n-1) n)
            (== `(s s . ,n-2) n)
            (appendo o-1 o-2 o)
            (fibo n-1 o-1)
            (fibo n-2 o-2)))))


(defrel (accepto regex str)
    (fresh (e1 e2 s1 s2 z)
        (conde
            ((== regex 0) (== str '(0)))
            ((== regex 1) (== str '(1)))
            ((== regex `(& ,e1 ,e2))
                (appendo s1 s2 str)
                (accepto e1 s1)
                (accepto e2 s2))
            ((== regex `(+ ,e1 ,e2))
                (conde
                    ((== z e1))
                    ((== z e2)))
                (accepto z str))

            ((== regex `(* ,e1))
                (conde
                    ((== str '()))
                    ((accepto `(& ,e1 (* ,e1)) str)))))))

(defrel (naive-accepto regex str)
    (fresh (e1 e2 s1 s2 z)
        (conde
            ((== regex 0) (== str '(0)))
            ((== regex 1) (== str '(1)))
            ((== regex `(& ,e1 ,e2))
                (appendo s1 s2 str)
                (naive-accepto e1 s1)
                (naive-accepto e2 s2))
            
            ((== regex `(+ ,e1 ,e2)) (naive-accepto e1 str))
            ((== regex `(+ ,e1 ,e2)) (naive-accepto e2 str))

            ((== regex `(* ,e1)) (== str '()))

            ((== regex `(* ,e1)) (accepto `(& ,e1 (* ,e1)) str)))))


(define movies '(
    (troy brad-pitt sean-bean orlando-blooom)
    (moneyball brad-pitt jonah-hill)
    (wolf-of-wall-street jonah-hill leonardo-dicaprio)
    (whats-eating-gilbert-grape leonardo-dicaprio johnny-depp)))


(define tmovies '(
    (A x y)
    (B y z)))

(defrel (pick1o x not-picked l)
    (riffleo `(,x) not-picked l))

(defrel (picko picked not-picked l)
    (conde
        ((== picked '()) (== not-picked l))
        ((fresh (first not-first rest)
            (== picked `(,first . ,rest))
            (pick1o first not-first l)
            (picko rest not-picked not-first)))))

(defrel (bacono movies x y proof)
    (fresh (movie actors rest-movies ^1)
        (picko `((,movie . ,actors)) rest-movies movies)
        (conde
            (
                (== proof `((,movie ,x ,y)))
                (picko `(,x ,y) ^1 actors))
        
            ((fresh (next rest-proof)
                (== proof `((,movie ,x ,next) . ,rest-proof))
                (picko `(,x ,next) ^1 actors)
                (bacono rest-movies next y rest-proof))))))


(defrel (three-dimensional-matchingo triples matching xs ys zs)
    (fresh (unmatched)
        (riffleo matching unmatched triples)
        (containso matching xs ys zs)))

(defrel (containso triples xs ys zs)
    (conde
        ((== triples '()))

        ((fresh (x y z rest-triples rest-xs rest-ys rest-zs)
            (== triples `((,x ,y ,z) . ,rest-triples))
            (picko `(,x) rest-xs xs)
            (picko `(,y) rest-ys ys)
            (picko `(,z) rest-zs zs)
            (containso rest-triples rest-xs rest-ys rest-zs)))))

(defrel (exact-three-dimensional-matchingo triples matching xs ys zs)
    (fresh (unmatched)
        (riffleo matching unmatched triples)
        (coverso matching xs ys zs)))

(defrel (coverso triples xs ys zs)
    (conde
        ((== triples '()) (== xs '()) (== ys '()) (== zs '()))

        ((fresh (x y z rest-triples rest-xs rest-ys rest-zs)
            (== triples `((,x ,y ,z) . ,rest-triples))
            (picko `(,x) rest-xs xs)
            (picko `(,y) rest-ys ys)
            (picko `(,z) rest-zs zs)
            (coverso rest-triples rest-xs rest-ys rest-zs)))))



