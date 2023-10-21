
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

typedef char* Symbol;

enum SExpTag { SYMBOL, CONS };

struct SExp {
    enum SExpTag kind;
    union {
        // kind == SYMBOL
        struct {
            Symbol sym;
        };
        // kind == CONS
        struct {
            struct SExp *car;
            struct SExp *cdr;
        };
    };
};

// It would be useful to have some constructors as well

struct SExp *make_Cons(struct SExp *car, struct SExp *cdr) {
    struct SExp *result = malloc(sizeof(struct SExp));
    result->kind = CONS;
    result->car = car;
    result->cdr = cdr;
    return result;
}


bool symbol_occurs_in_sexp(Symbol x, struct SExp *sexp) {
    switch (sexp->kind) {
        case SYMBOL:
            return strcmp(x, sexp->sym) == 0;

        case CONS:
            return (
                symbol_occurs_in_sexp(x, sexp->car) ||
                symbol_occurs_in_sexp(x, sexp->cdr)
            );
    }
}

struct SExp *replace_symbol_in_sexp(Symbol replace_me, struct SExp *replace_with, struct SExp *sexp) {
    switch (sexp->kind) {
        case SYMBOL:
            return (strcmp(sexp->sym, replace_me) == 0)
                ? replace_with
                : sexp;

        case CONS:
            return make_Cons(
                replace_symbol_in_sexp(replace_me, replace_with, sexp->car),
                replace_symbol_in_sexp(replace_me, replace_with, sexp->cdr)
            );
    }
}

enum ClauseTag { RELATION, CONDE, FRESH };

struct Clause {
    enum ClauseTag kind;

    union {
        // kind == RELATION
        struct {
            Symbol name;
            struct SExp *args;
            int num_args;
        };

        // kind == CONDE
        struct {
            struct Clause **conjunctions;
            int *nums_clauses;
            int num_conjunctions;
        };

        // kind == FRESH
        struct {
            Symbol *vars;
            int num_vars;
            struct Clause *clauses;
            int num_clauses;
        };
    };
};

struct SExp *replace_symbol_in_clause(Symbol replace_me, struct SExp *replace_with, struct Clause *clause) {
    switch (clause->kind) {
        case RELATION:
            int n = clause->num_args;
            struct Clause *result = malloc(sizeof(struct Clause));
            result->kind = RELATION;
            result->num_args = n;
            result->args = malloc(sizeof(struct SExp *) * n);
            for (int i = 0; i < n; i++) {
                result->args[i] = replace_symbol_in_sexp(replace_me, replace_with, clause->args[i]);
            }

            return result;

        case CONDE:
            int n = clause->num_conjunctions;
            struct Clause *result = malloc(sizeof(struct Clause));
            result->kind = RELATION;
            result->num_conjunctions = n;
            result->nums_clauses = clause->nums_clauses;
            result->conjunctions = malloc(sizeof(struct Clause *) * n);
            for (int i = 0; i < n; i++) {
                int m = clause->nums_clauses[i];
                for (int j = 0; j < m; j++) {
                    result->conjunctions[i * n + j] = replace_symbol_in_clause(
                        replace_me, replace_with,
                        clause->conjunctions[i * n + j]
                    );
                }
            }

            return result;

        case FRESH:
            for (int i = 0; i < clause->num_vars; i++) {
                if (strcmp(replace_me, clause->vars[i]) == 0) {
                    return clause;
                }
            }

            struct Clause *result = malloc(sizeof(struct Clause));
            

            return;
    }
}
