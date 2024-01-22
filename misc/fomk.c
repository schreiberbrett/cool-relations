#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>


struct Thunk {
    
};


struct State {
    
};

struct Substitution {
    
};

struct Term {
    enum { NIL, CONS, INT, VAR } kind;
    union {
        int n;
        struct { struct Term *car; struct Term *cdr; };
    };
};

struct Goal {
    enum { DISJ, CONJ, EQ, RELATE } kind;
    union {
        struct { struct Goal *g1; struct Goal *g2; };
        struct { struct Term *t1; struct Term *t2; };
        struct { struct Goal *(*thunk)(); char *description; };
    };
};

struct Stream {
    enum { MPLUS, BIND, PAUSE, EMPTY, PAIR } kind;
    union {
        struct { struct Stream *s1; struct Stream *s2; };
        struct { union { struct Stream *s; struct State *st; }; struct Goal *g; };
        struct { struct State *car; struct Stream *cdr; };
    };
};

struct Substitution *unifySub(struct Term *u, struct Term *v, struct Substitution *sub) {
    u = walk(u, sub);
    v = walk(v, sub);
    
    if (u->kind == VAR && v->kind == VAR && u->index == v->index) {
        return sub;
    }
    
    // TODO: Rest
    return NULL;
}

struct Stream *unify(struct Term *u, struct Term *v, struct State *st) {
    struct Substitution *sub = unifySub(u, v, (stateSub st));
    if (sub == NULL) {
        return EMPTY;
    }
    
    return cons((state sub), empty());
}

struct Stream *mplus(struct Stream *s1, struct Stream *s2) {
    struct Stream *result = malloc(sizeof(struct Stream));
    *result = (struct Stream){
        .kind = MPLUS,
        .s1 = s1,
        .s2 = s2
    };
    return result;
}

struct Stream *bind(struct Stream *s, struct Goal *g) {
    struct Stream *result = malloc(sizeof(struct Stream));
    *result = (struct Stream){
        .kind = BIND,
        .s = s,
        .g = g
    };
    return result;
}

struct Stream *empty() {
    struct Stream *result = malloc(sizeof(struct Stream));
    *result = (struct Stream){
        .kind = EMPTY
    };
    return result;
}

struct Stream *pair(struct State *car, struct Stream *cdr) {
    struct Stream *result = malloc(sizeof(struct Stream));
    *result = (struct Stream){
        .kind = PAIR,
        .car = car,
        .cdr = cdr
    };
    return result;
}

struct Stream *pause(struct State *st, struct Goal *g) {
    struct Stream *result = malloc(sizeof(struct Stream));
    *result = (struct Stream){
        .kind = PAUSE,
        .st = st,
        .g = g
    };
    return result;
}

struct Stream *step(struct Stream *s);

bool is_mature(struct Stream *s) {
    return s->kind == EMPTY || s->kind == PAIR;
}

struct Stream *mature(struct Stream *s) {
    if (is_mature(s)) {
        return s;
    }
    
    return mature(step(s));
}

struct Stream *step(struct Stream*);

struct Stream *start(struct State *st, struct Goal *g) {
    switch (g->kind) {
        case DISJ:
            return step(mplus(
                pause(st, g->g1),
                pause(st, g->g2)
            ));
        case CONJ:
            return step(bind(
                pause(st, g->g1),
                g->g2
            ));
        case RELATE:
            return pause(st, g->thunk());
            
        case EQ:
            return unify(g->t1, g->t2, st);
    }
}

struct Stream *step(struct Stream *s) {
    switch (s->kind) {
        case MPLUS: {
            struct Stream *s2 = s->s2;
            struct Stream *s1 = is_mature(s1) ? s1 : step(s1);
            
            switch (s1->kind) {
                case EMPTY: return s2;
                case PAIR: return pair(s1->car, mplus(s2, s1->cdr));
                default: return mplus(s2, s1);
            }
        }
        
        case BIND: {
            struct Goal *g = s->g;
            s = is_mature(s->s) ? s->s : step(s->s);
            switch (s->kind) {
                case EMPTY: return empty();
                case PAIR: return step(
                    mplus(
                        pause(s->car, g),
                        bind(s->cdr, g)
                    )
                );
                default: return bind(s, g);
            }
        }
        
        case PAUSE: {
            struct State *st = s->st;
            struct Goal *g = s->g;
            
            return start(st, g);
        }
        
        default: return s;
    }
}

int main() {
    return 0;
}
