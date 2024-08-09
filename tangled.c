
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>         
// .\README.md
// .\0-Types\0-Intro.md
// .\0-Types\1-Basic-Types.md
// .\0-Types\2-Higher-Order-Types.md
// .\0-Types\3-Natmaps.md
// .\0-Types\4-Natsets.md
// .\1-Arithmetic\0-Intro.md
// .\1-Arithmetic\1-Inequalities.md
// .\1-Arithmetic\2-Divisibility-by-Three.md
// .\1-Arithmetic\3-Fresh-Multiples-of-Three.md
// .\1-Arithmetic\4-Prime-Factorization.md
// .\1-Arithmetic\5-GCD.md
// .\1-Arithmetic\6-Triangle-Numbers.md
// .\2-Combinatorics\1-Riffle.md
// .\2-Combinatorics\2-Cartesian-Product.md
// .\2-Combinatorics\3-Associative-Cartesian-Product.md
// .\2-Combinatorics\4-Multiset-Venn-Diagrams.md
// .\3-Theory-of-Computation\0-Intro.md
// .\3-Theory-of-Computation\1-Lexing.md
// .\3-Theory-of-Computation\2-Context-Free-Grammars.md
// .\3-Theory-of-Computation\3-3SAT.md
// .\3-Theory-of-Computation\4-3COLOR.md
// .\3-Theory-of-Computation\5-UNSAT.md
// .\5-Puzzles-and-Games\1-The-Zebra-Puzzle.md
// .\6-Tricky-Relations\1-Length.md
// .\6-Tricky-Relations\2-Majority.md
// .\6-Tricky-Relations\3-Equal-Popcount.md
// .\6-Tricky-Relations\4-HAMPATH.md
// .\7-Techniques\1-Fresh-Tagging.md
// .\7-Techniques\2-Polymorphism-in-miniKanren.md
// .\7-Techniques\3-One-to-One-Relationships.md
// .\8-Implementing-miniKanren\1-Dovetailing-Streams.md
// .\8-Implementing-miniKanren\2-Defunctionalization.md
// .\8-Implementing-miniKanren\3-miniKanren-in-C.md
struct Stream;
struct Thunk;
struct Goal;
struct Term;
struct Substitution;
struct RelDef;

struct RelDef *reldefs;
int varCount = 0;

struct Stream* applyGoal(struct Goal *, struct Substitution *);
struct Substitution* unify(struct Term *, struct Term *, struct Substitution *);
bool occurs(struct Term *, struct Term *, struct Substitution *);
struct Term *walk(struct Term *, struct Substitution *);
bool equalTerms(struct Term *, struct Term *);
struct Term *copyTerm(int, struct Term *);
struct Goal *copyGoal(int, struct Goal *);
struct Term *var(int)

struct Stream {
    enum {
        EMPTY,
        NONEMPTY,
        DELAYED
    } kind;

    union {
        // EMPTY

        // NONEMPTY
        struct {
            struct Substitution *car;
            struct Stream *cdr;
        };

        // DELAYED
        struct Thunk *thunk;
    };
};

struct Stream *empty();
struct Stream *nonempty(struct Substitution *car, struct Stream *cdr);
struct Stream *delayed(struct Thunk *);

struct Thunk {
    enum {
        APPEND_INF,
        APPEND_MAP_INF
    } kind;

    union {
        // APPEND_INF
        struct Stream *stream;

        // APPEND_MAP_INF
        struct Goal *goal;
    };

    struct Thunk *thunk;
};

struct Thunk* appendInfThunk(struct Stream *, struct Thunk *);
struct Thunk* appendMapInfThunk(struct Goal *, struct Thunk *);

struct Stream* appendInf(struct Stream *s1, struct Stream *s2) {
    switch (s1->kind) {
        case EMPTY:
            return s2;

        case NONEMPTY:
            return nonempty(s1->car, appendInf(s1->cdr, s2));

        case DELAYED:
            return delayed(appendInfThunk(s2, s1->thunk));
    }
}

struct Stream* appendMapInf(struct Goal *g, struct Stream *s) {
    switch (s->kind) {
        case EMPTY:
            return empty();

        case NONEMPTY:
            return appendInf(
                applyGoal(g, s->car),
                appendMapInf(g, s->cdr)
            );

        case DELAYED:
            return delayed(appendMapInfThunk(g, s->thunk));
    }
}

struct Stream* pull(struct Thunk *t) {
    switch (t->kind) {
        case APPEND_INF:
            return appendInf(t->stream, pull(t->thunk));

        case APPEND_MAP_INF:
            return appendMapInf(t->goal, pull(t->thunk));
    }
}

struct Goal {
    enum {
        CONJ2,
        DISJ2,
        EQ,
        RELATE
    } kind;

    union {
        // CONJ2, DISJ2
        struct {
            struct Goal *g1;
            struct Goal *g2;
        };

        // EQ
        struct {
            struct Term *u;
            struct Term *v;
        };

        // RELATE
        struct {
            int i;
            int argc;
            struct Term **argv;
        };
    };
};

struct Goal *conj2(struct Goal *, struct Goal *);
struct Goal *disj2(struct Goal *, struct Goal *);
struct Goal *eq(struct Term *, struct Term *);
struct Goal *relate(int, int, struct Term **); // TODO

struct RelDef {
    struct Goal *g;
    int num_vars;
};

struct Stream* applyGoal(struct Goal *g, struct Substitution *s) {
    switch (g->kind) {
        case CONJ2:
            return appendMapInf(g->g2, applyGoal(g->g1, s));

        case DISJ2:
            return appendInf(
                applyGoal(g->g1, s),
                applyGoal(g->g2, s)
            );

        case EQ:
            struct Substitution *newS = unify(g->u, g->v, s);
            return (newS == NULL)
                ? empty()
                : nonempty(newS, empty());

        case RELATE:
            // Lookup the relation definition
            struct RelDef reldef = reldefs[g->i];

            // copy the goal
            struct Goal *copy = copyGoal(varCount, reldef.g);

            // Use unification for binding
            // chain conj2s to the beginning of the copied goal
            struct Goal *curr = copy;
            for (int i = 0; i < g->argc; i++) {
                curr = conj2(
                    eq(g->argv[i], var(i)),
                    curr
                );
            }

            varCount += reldef.num_vars;

            return curr;
    }
}

struct Term {
    enum {
        NIL,
        VAR,
        SYM,
        PAIR
    } kind;

    union {
        // NIL
        
        // VAR
        int i;

        // SYM
        char *s;

        // PAIR
        struct {
            struct Term *car;
            struct Term *cdr;
        };
    };
};

struct Term *var(int i) {
    struct Term *result = malloc(sizeof(struct Term));
    result->kind = VAR;
    result->i = i;
    return result;
}

struct Substitution {
    bool isEmpty;
    struct Term *x;
    struct Term *v;
    struct Substitution *rest;
};

struct Substitution *extS(struct Term *x, struct Term *v, struct Substitution *s) {
    if (occurs(x, v, s)) {
        return NULL;
    }

    struct Substitution *extended = malloc(sizeof(struct Substitution));
    extended->isEmpty = false;
    extended->x = x;
    extended->v = v;
    extended->rest = s;
    return extended;
}

struct Term *walk(struct Term *v, struct Substitution *s) {
    if (v->kind != VAR) {
        return v;
    }

    struct Substitution *curr = s;
    while (!curr->isEmpty) {
        if (curr->x->kind == VAR && curr->x->i == v->i) {
            return walk(curr->v, s);
            // It may be possible to return walk(curr->v, curr->next);
            // since the substitution is a DAG
        }

        curr = curr->rest;
    }

    return v;
}

bool occurs(struct Term *x, struct Term *v, struct Substitution *s) {
    v = walk(v, s);

    switch (v->kind) {
        case VAR:
            return (v->i == x->i);

        case PAIR:
            return occurs(x, v->car, s) || occurs(x, v->cdr, s);

        default:
            return false;
    }
}

struct Substitution *unify(struct Term *u, struct Term *v, struct Substitution *s) {
    u = walk(u, s);
    v = walk(v, s);

    if (equalTerms(u, v)) {
        return s;
    }

    if (u->kind == VAR) {
        return extS(u, v, s);
    }

    if (v->kind == VAR) {
        return extS(v, u, s);
    }

    if (u->kind == PAIR && v->kind == PAIR) {
        struct Substitution *newS = unify(u->car, v->car, s);
        return newS == NULL ? NULL : unify(u->cdr, v->cdr, newS);
    }

    return NULL;
}

bool equalTerms(struct Term *u, struct Term *v) {
    if (u == v) {
        return true;
    }

    if (u->kind != v->kind) {
        return false;
    }

    switch (u->kind) {
        case NIL:
            return true;

        case VAR:
            return u->i == v->i;

        case SYM:
            return strcmp(u->s, v->s) == 0;

        case PAIR:
            return (
                equalTerms(u->car, v->car) &&
                equalTerms(u->cdr, v->cdr)
            );
    }
}

struct Term *copyTerm(int offset, struct Term *t) {
    return (t->kind == VAR) ? var(t->i + offset) : t;
}


struct Goal *copyGoal(int offset, struct Goal *g) {
    switch (g->kind) {
        case CONJ2:
            return conj2(
                copyGoal(offset, g->g1)
                copyGoal(offset, g->g2)
            );

        case DISJ2:
            return disj2(
                copyGoal(offset, g->g1)
                copyGoal(offset, g->g2)
            );

        case EQ:
            return eq(
                copyTerm(g->u),
                copyTerm(g->v)
            );

        case RELATE:
            struct Term **newArgv = malloc(sizeof(struct Term *) * g->argc);

            for (int i = 0; i < g->argc; i++) {
                newArgv[i] = copyTerm(g->argv[i]);
            }

            return relate(
                g->i,
                g->argc,
                newArgv,
            );
    }
}

void runAndReify(int n, struct Goal *g) {
    struct Substitution emptyS = { .isEmpty = true };

    struct Stream *s = applyGoal(g, &emptyS);

    int i = 0;
    while (true) {
        if (i == n + 1) {
            break;
        }

        switch (s->kind) {
            case EMPTY:
                return;

            case NONEMPTY:
                struct Substitution *cur = s->car;
                // reify here

                if (i >= 0) i++;
                s = s->cdr;
                break;

            case DELAYED:
                s = pull(s->thunk);
        }
    }
}

struct Stream singletonEmpty = { .kind = EMPTY };

struct Stream* empty() {
    return &singletonEmpty;
}

struct Stream* nonempty(struct Substitution *car, struct Stream *cdr) {
    struct Stream* result = malloc(sizeof(struct Stream));
    result->kind = NONEMPTY;
    result->car = car;
    result->cdr = cdr;
    return result;
}

struct Stream* delayed(struct Thunk *thunk) {
    struct Stream* result = malloc(sizeof(struct Stream));
    result->kind = DELAYED;
    result->thunk = thunk;
    return result;
}

struct Thunk* appendInfThunk(struct Stream *stream, struct Thunk *thunk) {
    struct Thunk* result = malloc(sizeof(struct Thunk));
    result->kind = APPEND_INF;
    result->stream = stream;
    result->thunk = thunk;
    return result;
}

struct Thunk* appendMapInfThunk(struct Goal *goal, struct Thunk *thunk) {
    struct Thunk* result = malloc(sizeof(struct Thunk));
    result->kind = APPEND_MAP_INF;
    result->goal = goal;
    result->thunk = thunk;
    return result;
}

struct Goal *conj2(struct Goal *g1, struct Goal *g2) {
    struct Goal *result = malloc(sizeof(struct Goal));
    result->kind = CONJ2;
    result->g1 = g1;
    result->g2 = g2;
    return result;
}

struct Goal *disj2(struct Goal *g1, struct Goal *g2) {
    struct Goal *result = malloc(sizeof(struct Goal));
    result->kind = DISJ2;
    result->g1 = g1;
    result->g2 = g2;
    return result;
}

struct Goal *eq(struct Term *u, struct Term *v) {
    struct Goal *result = malloc(sizeof(struct Goal));
    result->kind = EQ;
    result->u = u;
    result->v = v;
    return result;
}

struct Goal *relate(int i, int argc, struct Term **argv) {
    struct Goal *result = malloc(sizeof(struct Goal));
    result->i = i;
    result->argc = argc;
    result->argv = argv;
    return result;
}

// .\9-Misc\1-A-Rule-of-Inference.md
// .\9-Misc\2-Rational-Numbers.md
// Add, subtract, and multiply have the following types, since rationals are closed under these operations.

struct Rational      add(struct Rational, struct Rational);
struct Rational subtract(struct Rational, struct Rational);
struct Rational multiply(struct Rational, struct Rational);

int greatest_common_divisor(int, int);

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

int greatest_common_divisor(int a, int b) {
    if (b == 0) {
        return a;
    }

    return greatest_common_divisor(b, a % b);
}

// .\9-Misc\3-Utility-Definitions.md
// .\first-order-miniKanren\defrel-and-run\README.md
// .\first-order-miniKanren\existential-disjunctive-normal-form\README.md
// .\first-order-miniKanren\misc\README.md
// .\first-order-miniKanren\mk-expression-functions\README.md
// .\first-order-miniKanren\s-expression-functions\README.md
// .\liarKanren\README.md
// .\misc\expression-problem.md
// .\misc\natset.md
// .\misc\projection-and-selection.md
// .\misc\statically-typed-relations.md
// .\scribble-htmls\katex\README.md
