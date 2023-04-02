typedef int symbol;
enum SExpressionType {
    ATOM,
    CONS
};

struct Atom {}; // TODO

struct SExpression {
    enum SExpressionType type;
    union {
        struct Atom atom;
        struct {
            struct SExpression *car;
            struct SExpression *cdr;
        } cons;
    };
};

struct SExpression *c(struct SExpression *car, struct SExpression *cdr) {
    struct SExpression *result = malloc(sizeof (struct SExpression));

    result->type = CONS;
    result->cons.car = car;
    result->cons.cdr = cdr;

    return result;
}

struct Predicate {
    char *name;
    struct SExpression *s_expressions;
    int arity;
};

struct EDNF {
    symbol *existentials;
    int number_of_existentials;

    struct Conjunction *conjunctions;
    int number_of_conjunctions;
};

struct Predicate p(symbol name, int arity, struct SExpression *args) {
    return (struct Predicate){.name = name, .arity = arity, .args = args};
}


symbol x = gensym(), y = gensym(), z = gensym(), w = gensym();
symbol P = gensym(), Q = gensym(), R = gensym(), S = gensym();

struct EDNF my_ednf = (struct EDNF){
    .existentials = (symbol[]){x, y, z, w},
    .number_of_existentials = 4,

    .conjunctions = (struct Conjunction[]) {
        (struct Conjunction){
            .predicates = (struct Predicate[]){
                p(P, 2, (symbol[]){x, y}),
                p(Q, 2, (symbol[]){z, w})
            },
            .number_of_predicates = 2,
        }
    },
    .number_of_conjunctions = 5 
}


void free_ENDF(struct EDNF ednf) {
    return;
}

