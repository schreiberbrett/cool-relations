
#include <stdbool.h>

typedef char* Symbol;

enum Tag { SYMBOL, CONS };

struct SExp {
    enum Tag kind;
    Symbol sym;
    struct SExp *car;
    struct SExp *cdr;
};

bool symbol_occurs_in_sexp(Symbol sym, struct SExp *sexp) {
    switch (sexp->kind) {
        case CONS:
            return (
                symbol_occurs_in_sexp(sym, sexp->car) ||
                symbol_occurs_in_sexp(sym, sexp->cdr)
            );

        case SYMBOL:
            return strcmp(sym, sexp->sym) == 0;
    }
}
