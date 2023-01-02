;; A compiler for miniKanren as defined in The Reasoned Schemer 2nd Edition.

;; Goals of the compiler:
;; - Correctness checks
;; --- correct structure: (defrel, fresh, conde)
;; --- no free variables
;; --- all relations are defined
;; --- all relation calls have the correct arity
;; - Optimizations
;; --- (A & x) | (B & x) --> (A | B) & x
;; --- sort conjuctions by "score" for some meaningful definition of "score"
;; --- unify in-place where possible


(load "pmatch.scm")


;; Adapted from https://www.scheme.com/tsp13/io.html
(define (read-defrels filename)
    (call-with-input-file filename
        (lambda (p)
            (let f ((x (read p)))
                (if (eof-object? x)
                    '()
                    (cons x (f (read p))))))))


(define (defrel-is-well-formed defrel)
    (pmatch defrel
        ((defrel ,parameters . ,clauses)
            (append 
                (list-of-unique-symbols parameters)
                (append-map clause-is-well-formed clauses)))

        (_ ("A relation definition must have the form (defrel ,parameters . ,clauses)"))))

(define (list-of-unique-symbols l)
    (if (list? l)
        (append (all-symbols l) (no-duplicates l))
        `(,(format "Expected a list, instead got: ~s" l))))

(define (all-symbols l)
    (pmatch l
        (() '())
        ((,first . ,rest)
            (if (symbol? first)
                (all-symbols rest)
                (cons (format ("Not a symbol: ~s")) (all-symbols rest))))))


(define (no-duplicates list-of-symbols)
    (pmatch list-of-symbols
        (() '())
        ((,first . ,rest)
            (if (contains? rest first)
                (cons (format "Duplicate parameter: ~s" first) (no-duplicates rest))
                (no-duplicates rest)))))


(define (conde-clause-is-well-formed conde-clause)
    (if (list? conde-clause)
        (append-map clause-is-well-formed conde-clause)
        (expected-a-list conde-clause)))


(define (expected-a-list x)
    `(,(format "Expected a list, instead got: ~s" x)))



(define (contains? l x)
    (cond
        ((null? l) #f)
        ((pair? l) (or
            (equal? (car l) x)
            (contains? (cdr l) x)))))


(define (append-map f l)
    (cond
        ((null? l) '())
        ((pair? l) (append (f (car l)) (append-map f (cdr l))))))


;; Helper functions for inspecting the contents of clauses

;; `(defrel (,defrel-name . ,defrel-params) . ,defrel-clauses)
(define (defrel? clause)
    (eqv? (car clause) 'defrel))

(define (defrel-name defrel)
    (caadr defrel))

(define (defrel-params defrel)
    (cdadr defrel))

(define (defrel-clauses defrel)
    (cddr defrel))

;; `(fresh ,fresh-vars . ,fresh-clauses)
(define (fresh? clause)
    (eqv? (car clause) 'fresh))

(define (fresh-vars fresh-clause)
    (cadr fresh-clause))

(define (fresh-clauses fresh-clause)
    (cddr fresh-clause))


;; `(conde . ,conde-clause-lists)
(define (conde? clause)
    (eqv? (car clause) 'conde))

(define (conde-clause-lists conde-clause)
    (cdr conde-clause))


;; `(,rel-name . ,rel-args)
(define (rel-name clause)
    (car clause))

(define (rel-args clause)
    (cdr clause))

;; unbound-vars: Given a relation, return a list of variables that are not defined
(define (unbound-vars defrel)
    (let (
        (env (params defrel))
        (clauses (defrel-clauses defrel)))

        (append-map (lambda (clause) (clause-unbound-vars clause env)) clauses)))


(define (clause-unbound-vars clause env)
    (cond
        ((fresh? clause)
            (let ((new-env (append (fresh-vars clause) env)))
            (append-map (lambda (x) (clause-unbound-vars x new-env)) (fresh-clauses clause))))
        
        ((conde? clause)
            (append-map
                (lambda (x) (map (lambda (y) (clause-unbound-vars y env)) x)
                (conde-clause-lists clause))))
        
        (else (filter (lambda (x) (not (contains? env x))) (cdr clause)))))


(define (multiset-intersection* l)
    (pmatch l
        ((,x) x)
        ((,x ,y . ,rest) (multiset-intersection x (multiset-intersection* `(,y . ,rest))))))

(define (multiset-intersection l1 l2)
    (pmatch l1
        (() '())
        ((,h . ,t) (pmatch (multiset-find h l2)
            ((not-found) (multiset-intersection t l2))
            ((found ,rest) `(,h . ,(multiset-intersection t rest)))))))

(define (multiset-find x l)
    (multiset-find-helper x '() l))

(define (multiset-find-helper x seen l)
    (pmatch l
        (() '(not-found))
        ((,h . ,t) (if (eqv? x h)
            `(found ,(append seen t))
            (multiset-find-helper x `(,h . ,seen) t)))))


(define (multimap-intersection m1 m2 f)
    (pmatch m1
        (()
            '())
        (((,k . ,v1) . ,rest1)
            (pmatch (lookup k m2)
                ((empty)
                    (multimap-intersection rest1 m2 f))
                ((some ,v2 ,rest2)
                    `((,k . ,(f v1 v2)) .
                        ,(multimap-intersection rest1 rest2 f)))))))

(define (lookup k m)
    (lookup-helper k m '()))

(define (lookup-helper k1 m seen)
    (pmatch m
        (()
            '(empty))
        (((,k2 . ,v) . ,rest)
            (if (eqv? k1 k2)
                `(some ,v ,(append seen rest))
                (lookup-helper k1 rest `((,k2 . ,v) . ,seen))))))


(define (append* l)
    (pmatch l
        (() '())
        ((,h . ,t) (append h (append* t)))))

(define (unique l)
    (pmatch l
        (() '())
        ((,h . ,t) (if (contains? t h)
            (unique t)
            (cons h (unique t))))))

(define (partition l predicate)
    (pmatch l
        (() '(() . ()))
        ((,h . ,t)
            (pmatch (partition t predicate)
                ((,yes . ,no) (if (predicate h)
                    `((,h . ,yes) . ,no)
                    `(,yes . (,h . ,no))))))))

;; Algorithm steps: (assume no duplicate relations in a clause)
;; 1. Count the relations by # of conde clauses they appear in, e.g., 3 out of 5
;; 2. Find the relation with the highest count. (There may be two with the same number, just pick one). If the highest count is 1, return early
;;     Requires max-by function
;; 3. Split the conde clauses into two lists: those containing the relation, and those that don't.
;;     Requires partition function
;; 4. Factor out the relation
;; 5. Recur on both sublists

(define (algorithm clauses)
    (letrec* (
        (relations (unique-relations clauses))
        (most-frequent-relation (max-by (lambda (relation) (number-of-occurences relation clauses)) relations))
        (pair (partition clauses (lambda (clause)
            (contains-relation? most-frequent-relation clause))))
        (incl (car pair))
        (excl (cdr pair)))

        (if (eqv? (number-of-occurences most-frequent-relation clauses) 1)
            '(none)
            `(some
                ,(map (lambda (clause) (remove-relation most-frequent-relation clause)) incl)
                ,most-frequent-relation
                ,excl))))

(define (contains-relation? relation clause)
    (contains? (map car clause) relation))

(define (remove-relation relation clause)
    (pmatch clause
        (() '())
        (((,r . ,x) . ,t)
            (if (eqv? r relation)
                t
                `((,r . ,x) . ,(remove-relation relation t))))))

(define (number-of-occurences relation clauses)
    (length (filter (lambda (clause) (contains-relation? relation clause)) clauses)))

(define (unique-relations clauses)
    (unique (append* (map (lambda (x) (map car x)) clauses))))

(define (max-by f l)
    (pmatch l
        (() '(nothing))
        ((,h . ,t) (pmatch (max-by f t)
            ((nothing) '(just ,h))
            ((just ,m) `(just ,(if (> (f h) (f m)) h m)))))))

;; should be some permutation of
;; '((a . (x . z)) (a . (y . y)) (b . (z . x)))
(define mmi-test (multimap-intersection
    '((a . x) (a . y) (b . z) (c . x))
    '((b . x) (a . z) (a . y))
    cons))

;; Test cases:
(define sample-defrel '
    (defrel (fundamental-theorem-of-arithmetico x primes)
        (conde
            ((primeo x) (== primes `(,x)))
            ((fresh (a b rest-primes)
                (*o a b x)
                (primeo a)
                (fundamential-theorem-of-arithmetico b rest-primes)
                (== primes `(,a . ,rest-primes)))))))

(define another-sample-defrel '(defrel (reverseo x y) (reverseo y x)))

(define sample-clauses '(
    ((primeo x) (== primes `(,x)))
    (
        (*o a b x)
        (primeo a)
        (fundamential-theorem-of-arithmetico b rest-primes)
        (== primes `(,a . ,rest-primes)))))

(define sample-clauses-2 '(
    ((a x) (b x) (c x))
    ((d x) (b x) (c x))))


;; Consolidate all fresh variables into one top-level fresh declaration.
;; Assumes that no two fresh variables with the same name.
(define (single-fresh d) (pmatch d ((defrel ,args . ,exps)
    (letrec* (

        ;; gather-fresh-vars :: [Exp] -> [Symbol]
        (gather-fresh-vars (lambda (exps)
            (append-map (lambda (exp) (pmatch exp
                ((conde . ,clauses) (append-map gather-fresh-vars clauses))
                ((fresh ,vars . ,fresh-exps) (append vars (gather-fresh-vars fresh-exps)))
                ((,relation . ,args) '()))) exps)))

        ;; remove-fresh-decls :: [Exp] -> [Exp]
        (remove-fresh-decls (lambda (exps)
            (append-map (lambda (exp) (pmatch exp
                ((conde . ,clauses) `((conde . ,(map remove-fresh-decls clauses))))
                ((fresh ,vars . ,fresh-exps) (remove-fresh-decls fresh-exps))
                ((,relation . ,args) `((,relation . ,args))))) exps)))

        (fresh-vars (gather-fresh-vars exps))
        (new-exps (remove-fresh-decls exps)))

    (if (null? fresh-vars)
        `(defrel ,args . ,exps)
        `(defrel ,args . ((fresh ,fresh-vars . ,new-exps))))))))



(define (prune-empty-condes d)
    (letrec* (
        (go (lambda (exp)
            (pmatch exp
                ((conde . ,clauses)
                    (if (null? clauses) '()
                    (if (equal? (length clauses) 1) (car clauses)
                    `((conde . ,(map go* clauses))))))

                ((fresh ,vars . ,fresh-exps)
                    `((fresh ,vars . ,(go* fresh-exps))))
                ((,relation . ,args)
                    `((,relation . ,args))))))

        (go* (lambda (exps)
            (append-map go exps))))

    (pmatch d
        ((defrel ,args . ,exps)
            `(defrel ,args . ,(go* exps))))))




(define (optimize-condes d)
    (pmatch d
        ((defrel ,args . ,exps)
            `(defrel ,args . ,(map optimize-conde exps)))))

(define (optimize-conde exp)
    (pmatch exp
        ((fresh ,vars . ,exps)
            `(fresh ,vars . ,(map optimize-conde exps)))
        ((conde . ,exps*)
            `(conde . ,(optimize-conde-exps*
                (map (lambda (exps)
                    (map optimize-conde exps)) exps*))))
        ((,rel . ,args)
            `(,rel . ,args))))


(define (maybe-most-frequent-relation exps*)
    (letrec* (
        (contains-relation? (lambda (exps rel)
            (and 
                (pair? exps)
                (or
                    (pmatch (car exps)
                        ((conde . ,clauses) #f)
                        ((fresh ,vars . ,fresh-exps) #f)
                        ((,relation . ,args) (equal? rel relation)))

                    (contains-relation?
                        (cdr exps) rel)))))

        (frequency (lambda (rel)
            (length (filter (lambda (exps)
                (contains-relation? exps rel)) exps*))))

        (all-relations
            (append-map (lambda (exps) (append-map (lambda (exp)
                (pmatch exp
                    ((conde . ,clauses) '())
                    ((fresh ,vars . ,fresh-exps) '())
                    ((== ,x ,y) '())
                    ((,relation . ,args) `(,relation)))) exps)) exps*))


        (maybe-most-freq (max-by frequency all-relations)))

    (pmatch (max-by frequency all-relations)
        ((nothing) '(nothing))
        ((just ,rel) (if (equal? (frequency rel) 1) '(nothing) `(just ,rel))))))


(define (arity relation exps*)
    (car (append-map (lambda (exps) (append-map (lambda (exp) (pmatch exp
        ((conde . ,clauses) '())
        ((fresh ,vars . ,clauses) '())
        ((,rel . ,args) (if (equal? relation rel) `(,(length args)) '())))) exps)) exps*)))

(define (optimize-conde-exps* exps*)
    (pmatch (maybe-most-frequent-relation exps*)
        ((nothing) exps*)
        ((just ,most-frequent-relation)
            (extract-common-relation
                most-frequent-relation
                (map (lambda (x) (gensym)) (iota (arity most-frequent-relation exps*)))
                exps*))))

;; extract-common-relation :: Symbol -> [Symbol] -> [[Exp]] -> [[Exp]]
(define (extract-common-relation rel fresh-args clauses)
    (letrec* (
        (partition (partition-justs (lambda (exps)
            (split-on-just (lambda (exp)
                (pmatch exp
                    ((fresh ,args . ,exps) '(nothing))
                    ((conde . ,conde-clauses) '(nothing))
                    ((,r . ,args) (if (equal? r rel)
                        `(just ,(zip-with (lambda (arg fresh-arg) `(== ,arg ,fresh-arg)) args fresh-args))
                        '(nothing))))) exps)) clauses))

        (excludes-rel (optimize-conde-exps* (cdr partition)))
        (includes-rel (optimize-conde-exps* (map (lambda (triple)
            (pmatch triple
                ((,before ,just ,after) (append just before after))))
            (car partition)))))


    ;; (pmatch excludes-rel (() `((fresh ,fresh-args () ()))

    (if (null? excludes-rel)
        `(
            ((fresh ,fresh-args
                (conde . ,includes-rel)
                (,rel . ,fresh-args))))

        `(
            ((fresh ,fresh-args
                (conde . ,includes-rel)
                (,rel . ,fresh-args)))
            ((conde . ,excludes-rel))))))


;; map-maybe :: (a -> Maybe b) -> [a] -> [b]
(define (map-maybe f l)
    (pmatch l
        (() '())
        ((,h . ,t) (pmatch (f h)
            ((nothing) (map-maybe t))
            ((just ,just) `(,just . ,(map-maybe t)))))))

;; partition-justs :: (a -> Maybe b) -> [a] -> ([b], [a])
(define (partition-justs f l)
    (pmatch l
        (() `(() . ()))
        ((,h . ,t) (pmatch (partition-justs f t)
            ((,justs . ,nothings) (pmatch (f h)
                ((nothing) `(,justs . (,h . ,nothings)))
                ((just ,just) `((,just . ,justs) . ,nothings))))))))

;; split-on-just :: (a -> Maybe b) -> [a] -> ([a], b, [a])
(define (split-on-just f l)
    (pmatch l
        (() '(nothing))
        ((,h . ,t) (pmatch (f h)
            ((just ,just) `(just (() ,just ,t)))
            ((nothing) (pmatch (split-on-just f t)
                ((nothing) '(nothing))
                ((just (,before ,just ,after)) `(just ((,h . ,before) ,just ,after)))))))))

;; zip-with :: (a -> b -> c) -> [a] -> [b] -> [c]
(define (zip-with f l1 l2)
    (pmatch `(,l1 . ,l2)
        ((() . ()) '())
        (((,h1 . ,t1) . (,h2 . ,t2))
            `(,(f h1 h2) . ,(zip-with f t1 t2)))))


(define sample-clauses-3 '(
    ((a x) (b x) (c x))
    ((d x) (c x) (e x) (f x))
    ((g x) (h x))
    ((c x) (i x))
    ((j x) (k x) (l x))))


(define sample-defrel-2
    '(defrel (bretto x)
        (conde
            ((a x) (b x) (c x))
            ((d x) (c x) (e x) (f x))
            ((g x) (h x))
            ((c x) (i x))
            ((j x) (k x) (l x)))))

(define sample-defrel-3
    '(defrel (courtneyo x)
        (conde
            ((a x) (b x)      )
            (      (b x) (c x)))))


(define sample-defrel-4
    '(defrel (jakeo x)
        (conde
            ((a x) (b x) (c x))
            (      (b x) (c x) (d x)))))


(define sample-defrel-5
    '(defrel (nicoleo x)
        (conde
            ((a x) (b x) (c x)            )
            (      (b x) (c x) (d x)      )
            (            (c x) (d x) (e x)))))

'(defrel (nicoleo x)
  (conde
    (fresh (g2)
      (conde
        (fresh (g3)
          (conde
            ((== x g3) (== x g2) (a x))
            ((== x g3) (== x g2) (d x)))
          (b g3))
        (conde
          ((== x g2) (d x) (e x))))
      (c g2))
    (conde)))

'(defrel
    (nicoleo x)
    (conde
        ((fresh (g0)
            (conde
                ((fresh (g1)
                (conde
                    ((== x g1) (== x g0) (a x))
                    ((== x g1) (== x g0) (d x)))
                (b g1)))
                ((conde
                ((== x g0) (d x) (e x)))))
            (c g0)))
        ((conde))))

'(defrel (nicoleo x)
    (conde
        ((fresh (g0)
            (conde
                ((fresh (g1)
                    (conde
                        ((== x g1) (== x g0 ) (a x))
                        ((== x g1) (== x g0) (d x)))
                    (b g1)))
                ((== x g0) (d x) (e x)))
            (c g0)))
        ()))



(define sample-defrel-6
    '(defrel (dado x y z)
        (conde
            ((a x) (b y) (c z)      )
            (      (b x) (c y) (d z)))))

'(defrel (dado x y z)
  (fresh (g4 g5)
    (conde
      ((== z g5) (== y g4) (a x))
      ((== y g5) (== x g4) (d z)))
    (c g5) (b g4)))





(define-syntax defrel-optimized
    (syntax-rules ()
        ((_ (name args ...) body ...)
            (letrec* (
                (arg-list (list 'args ...))
                (body-list (list 'body ...)))

            (eval (optimize-condes `(defrel (name args ...) . (body ...))))))))
