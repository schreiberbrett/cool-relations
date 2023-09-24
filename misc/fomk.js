// deno-lint-ignore-file prefer-const
// @ts-check

// First order miniKanren, translated into JavaScript

///** @typedef {{name: string, index: number}} Var */

/** @typedef {{kind: 'Var', name: string, index: number}} Var */
/** @typedef {{kind: 'Pair', car: Term, cdr: Term}} Pair */
/** @typedef {{kind: 'Other', value: any }} Other */

/** @typedef {Var | Pair | Other} Term */


/** @type {(x: Var, y: Var) => boolean} */
function varEqual(x, y) {
    return x.index == y.index;
}

// We need to translate Racket's builtin eqv.
/** @type {(x: Term, y: Term) => boolean} */
function eqv(x, y) {
    if (x.kind === 'Var' && y.kind === 'Var') {
        return varEqual(x, y);
    }

    if (x.kind === 'Pair' && y.kind === 'Pair') {
        return eqv(x.car, y.car) && eqv(x.cdr, y.cdr);
    }

    if (x.kind === 'Other' && y.kind === 'Other') {
        return x === y;
    }

    return false;
}

/** @type {Var} */
const initialVar = { kind: 'Var', name: '', index: 0 }

/** @type {(name: string) => Var} */
const varFresh = (() => {
    let index = 0;
    return (name) => {
        index++;
        return {kind: 'Var', name, index};
    }
})();

// In this implementation, a substitution is a Javascript Map, not an association list like it is in the paper.
/** @typedef {Map<number, Term>} Sub */

/** @type {Sub} */
const emptySub = new Map();


/** @type {(t: Term, sub: Sub) => Term} */
function walk(t, sub) {
    if (t.kind === 'Var') {
        const result = sub.get(t.index);
        if (result === undefined) {
            return t;
        }

        return walk(result, sub);
    }

    return t;
}

/** @type {(x: Var, t: Term, s: Sub) => boolean} */
function occurs(x, t, sub) {
    if (t.kind === 'Pair') {
        return occurs(x, t.car, sub) || occurs(x, t.cdr, sub);
    }

    if (t.kind === 'Var') {
        return varEqual(x, t);
    }

    return false;
}

/** @type {(x: Var, t: Term, sub: Sub) => (Sub | null)} */
function extendSub(x, t, sub) {
    if (occurs(x, t, sub)) {
        return null;
    }

    let result = new Map();
    for (let [k, v] of sub) {
        result.set(k, v);
    }

    result.set(x, t);

    return result;
}

/** @typedef {{sub: Sub}} State */

/** @type {State} */
const emptyState = {sub: emptySub};


/** @type {(u: Term, v: Term, sub: Sub) => (Sub | null)} */
function unifySub(u, v, sub) {
    u = walk(u, sub);
    v = walk(v, sub);

    if (u.kind === 'Var' && v.kind === 'Var' && varEqual(u, v)) {
        return sub;
    }

    if (u.kind === 'Var') {
        return extendSub(u, v, sub);
    }

    if (v.kind === 'Var') {
        return extendSub(v, u, sub);
    }

    if (u.kind === 'Pair' && v.kind === 'Pair') {
        const carSub = unifySub(u.car, v.car, sub);
        if (carSub == null) {
            return null;
        }

        return unifySub(u.cdr, v.cdr, carSub);
    }

    if (eqv(u, v)) {
        return sub;
    }

    return null;
}

/** @type {(u: Term, v: Term, st: State) => (State[] | null)} */
function unify(u, v, st) {
    const sub = unifySub(u, v, st.sub);
    if (sub == null) {
        return null;
    }

    return [{sub}];
}

// Reification

/** @type {(tm: Term, sub: Sub) => Term} */
function walkStar(tm, sub) {
    tm = walk(tm, sub);

    if (tm.kind == 'Pair') {
        return {
            kind: 'Pair',
            car: walkStar(tm.car, sub),
            cdr: walkStar(tm.cdr, sub)
        }
    }

    return tm;
}

/** @type {(index: number) => Term} */
function reifiedIndex(index) {
    return {kind: 'Other', value: `_.${index}`};
}

/** @type {(tm: Term, st: State) => Sub} */
function reify(tm, st) {
    let index = -1;

    /** @type {(tm: Term, sub: Sub) => Sub} */
    function loop(tm, sub) {
        let t = walk(tm, sub);

        if (t.kind == 'Pair') {
            return loop(t.cdr, loop(t.car, sub));
        }

        if (t.kind == 'Var') {
            index++;
            return /** @type {Sub} */ (extendSub(t, reifiedIndex(index), sub));
        }

        return sub;
    }

    return loop(tm, st.sub);
}

/** @type {(st: State) => Sub} */
function reifyInitialVar(st) {
    return reify(initialVar, st);
}

/** @typedef {Disj | Conj | Relate | Equal} Goal */

/** @typedef {{kind: 'Disj', g1: Goal, g2: Goal}} Disj */
/** @typedef {{kind: 'Conj', g1: Goal, g2: Goal}} Conj */
/** @typedef {{kind: 'Relate', thunk: number, description: string}} Relate */
/** @typedef {{kind: '==', t1: Term, t2: Term}} Equal */
/** @typedef {{kind: 'Bind', bindS: Stream, bindG: Goal}} Bind */
/** @typedef {{kind: 'MPlus', mplusS1: Stream, mplusS2: Stream}} Mplus */
/** @typedef {{kind: 'Pause', pauseState: State, pauseGoal: Goal}} Pause */


