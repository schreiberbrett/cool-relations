# miniKanren in C

## Headers

```c
struct Stream;
struct Thunk;
struct Goal;
struct Term;
struct Substitution;
struct RelDef;
```

Global variables:

```c
struct RelDef *reldefs;
int varCount = 0;
```

And functions

```c
struct Stream* applyGoal(struct Goal *, struct Substitution *);
struct Substitution* unify(struct Term *, struct Term *, struct Substitution *);
bool occurs(struct Term *, struct Term *, struct Substitution *);
struct Term *walk(struct Term *, struct Substitution *);
bool equalTerms(struct Term *, struct Term *);
struct Term *copyTerm(int, struct Term *);
struct Goal *copyGoal(int, struct Goal *);
struct Term *var(int);
```

## Streams and Thunks

Since C does not support higher-order functions, the implementation of streams must be defunctionalized.

```c
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
```

A thunk is a data structure which gives instructions on how to produce a stream. In TRS2E miniKanren, there are only two contexts that delayed streams are created in, `appendInf` and `appendMapInf`. One requires a stream, the other require a goal, but they both require an inner thunk.

```c
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
```

```c
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
```

And goals:

```c
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
```

A `RelDef` is an entry in the `reldefs` table, which associated an index to a `Goal`, along with a count of the number of variables that the goal uses.

```c
struct RelDef {
    struct Goal *g;
    int num_vars;
};
```

```c
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

            return applyGoal(curr, s);
    }
}
```

## Terms

```c
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
```

## Substitution

A substitution is a linked list, but its type
```c
struct Substitution {
    bool isEmpty;
    struct Term *x;
    struct Term *v;
    struct Substitution *rest;
};
```

The code for extending a substitution, unifying against a substitution, and walking a substitution are derived from from `TRS2E-impl.scm`.

Adding a circular term to a substitution should cause a failure. Here, C's `NULL` is used in place of Scheme's `#f`.
```c
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
```

Walking a term means recursively finding what it points to in the substitution, if its a variable present within.
```c
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
```

The occurs check is nearly the same as in TRS2E, except that checking equivalence between variables is done explicitly, like in `walk`.
```c
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
```


```c
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
```

### Term equality

```c
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
```

## Invoking a defined relation

Apart from the defunctionalization of goals and streams, which follows the same path that the First-order miniKanren paper did, this C version also defunctionalizes relation definitions.

When a goal of kind `RELATE` is encountered, the definition of the relation must be looked up at index `i` in an array containing all the relation definitions.

A relation definition is a compound term of a `Goal`, an `arity`, and a count `num_vars` of how many logic variables it uses.

A relation definition has parameters: logic variables which get constrained between each other. These parameters are included in this count, therefore `arity <= num_vars`. Since a relation definitions exists as a self-contained chunk of miniKanren code, any logic variables in its inner `Goal` must have an index `i` such that `0 <= i < num_vars`.

So when a relation definition gets copied over within `applyGoal`, all its logic variables must be introduced at the same time, shifted by the current count of logic variables in the program.

```c
struct Term *copyTerm(int offset, struct Term *t) {
    return (t->kind == VAR) ? var(t->i + offset) : t;
}

```

```c
struct Goal *copyGoal(int offset, struct Goal *g) {
    switch (g->kind) {
        case CONJ2:
            return conj2(
                copyGoal(offset, g->g1),
                copyGoal(offset, g->g2)
            );

        case DISJ2:
            return disj2(
                copyGoal(offset, g->g1),
                copyGoal(offset, g->g2)
            );

        case EQ:
            return eq(
                copyTerm(offset, g->u),
                copyTerm(offset, g->v)
            );

        case RELATE:
            struct Term **newArgv = malloc(sizeof(struct Term *) * g->argc);

            for (int i = 0; i < g->argc; i++) {
                newArgv[i] = copyTerm(offset, g->argv[i]);
            }

            return relate(
                g->i,
                g->argc,
                newArgv
            );
    }
}
```


First, the relation definition exists as self-contained code -- it has no free variables. That is good! But that means any logic variables referenced within, either the parameters or logic variables introduced with `fresh` are indexed independently of the actual number of logic variables in the running program.

So when the relation definition gets copied over and inlined, its logic variables must offset by the current count of runtime logic variables.

## Running a goal, taking from a stream, and reifying

In TRS2E, the process of runnnig a goal `n` times, taking from a stream, and mapping to reified results are split into multiple functions, but they only ever get called together. This implementation performs all three steps in one pass.

```c
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
```

## Appendix

Constructors

Empty streams are never mutated, so all empty streams can point to the same area in memory. So the `empty()` constructor can simply return a reference to the singleton empty stream.

```c
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
```

Thunks
```c
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
```

Goals:

```c
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
```