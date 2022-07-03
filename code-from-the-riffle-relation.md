(load "~/CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
(load "~/CodeFromTheReasonedSchemer2ndEd/trs2-arith.scm")

(defrel (riffleo a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o z0 z1)
        (conde
            ;; When one of `a` or `b` is empty
            ((== a '()) (== b '()) (== o '()))
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are nonempty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o))
                (conde
                    ((== car-o car-a) (== z0 cdr-a) (== z1 b))
                    ((== car-o car-b) (== z0 a) (== z1 cdr-b)))
                    
                (riffleo z0 z1 cdr-o)))))


(defrel (three-partitiono l partitions sum)
    (fresh (e0 e1 e2 rest-l e0+e1 rest-partitions)
        (conde
            ;; Base case
            ((== l '())
                (== partitions '()))

            ;; Recursive case
            ((== partitions `((,e0 ,e1 ,e2) . ,rest-partitions))
                (riffleo `(,e0 ,e1 ,e2) rest-l l)
                (pluso e0 e1 e0+e1)
                (pluso e0+e1 e2 sum)
                (three-partitiono rest-l rest-partitions sum)))))

(define (unbuild-num n)
    (cond
        ((null? n) 0)
        (else (+ (car n) (* 2 (unbuild-num (cdr n)))))))
        
(define (deep-unbuild-num l)
    (cond
        ((or (null? l) (eq? (car l) 0) (eq? (car l) 1))
            (unbuild-num l))
        (else
            (map deep-unbuild-num l))))

(define example (map build-num
    '(20 23 25 30 49 45 27 30 30 40 22 19)))
