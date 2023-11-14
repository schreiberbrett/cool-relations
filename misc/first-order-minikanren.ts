type Var = ['var', string, number]

type Term =
	Var |
	['pair', Term, Term] |
	['other', any];
	
type Substitution = [Var, Term][];
	
type Goal =
	['disj', Goal, Goal] |
	['conj', Goal, Goal] |
	['relate', () => Goal, string] |
	['==', Term, Term];
	
type Stream =
	['mplus', Stream, Stream] |
	['bind', Stream, Goal] |
	['pause', Substitution, Goal] |
	['pair', Substitution, Stream] |
	['empty'];
	
function varEqual(x1: Var, x2: Var) {
	return x1[2] === x2[2];
}

const initialVar: Var = ['var', '#f', 0];

let index = 0;
function varFresh(name: string): Var {
	index++;
	return ['var', name, index];
}

const emptySub: Substitution = [];
function walk(t: Term, sub: Substitution): Term | null {
	if (t[0] !== 'var') {
		return t;
	}
	
	for (let [v, term] of sub) {
		if (varEqual(t, v)) {
			return walk(term, sub);
		}
	}
	
	return t;
}

function occurs(x: any, t: Term, sub: Substitution) {
	if (t[0] == 'pair') {
		const [_, carT, cdrT] = t;
		return (
			occurs(x, walk(carT, sub), sub) ||
			occurs(x, walk(cdrT, sub), sub)
		);
	}
	
	// TODO: Finish
}


function unifySub(u: Term, v: Term, sub: Substitution) Substitution | null {
	u = walk(u, sub);
	v = walk(v, sub);
	if (u[0] == 'var' && v[0] == 'var' && varEqual(u, v))  {
		return sub;
	}
	
	if (u[0] == 'var') {
		return extendSub(u, v, sub);
	}
	
	if (v[0] == 'var') {
		return extendSub(v, u, sub);
	}
	
	if (u[0] == 'pair' && v[0] == 'pair') {
		const [_, carU, cdrU] = u;
		const [_, carV, cdrV] = v;

		const newSub = unifySub(carU, carV, sub);
		if (newSub === null) {
			return null;
		}
		
		return unifySub(cdrU, cdrV, newSub);
	}
	
	if (termEqual(u, v)) {
		return sub;
	}
	
	return null;
}

function unify(u: Term, v: Term, st: Substitution): Substitution | null {
	const sub = unifySub(u, v, stateSub(st));
	if (sub === null) {
		return null;
	} else {
		return [state(sub)];
	}
}

function isMature(s: Stream): boolean {
	return s[0] == 'empty' || s[0] == 'pair';
}

function start(st: Substitution, g: Goal): Stream {
	if (g[0] === 'disj') {
		let [_, g1, g2] = g;

		return step(['mplus',
			['pause', st, g1],
			['pause', st, g2]
		]);
	} else if (g[0] === 'conj') {
		let [_, g1, g2] = g;
		
		return step(['bind', ['pause', st, g1], g2]);
	} else if (g[0] === 'relate') {
		let [_, thunk, __] = g;
		
		return ['pause', st, thunk()];
	} else /* (g[0] === '==') */ {
		let [_, t1, t2] = g;
		
		return unify(t1, t2, st);
	}
}

function step(stream: Stream): Stream {
	if (stream[0] === 'mplus') {
		let [_, s1, s2] = stream;
		s1 = isMature(s1) ? s1 : step(s1);
		
		if (s1[0] === 'empty') {
			return s2;
		} else if (s1[0] === 'pair') {
			return ['pair', s1[1], ['mplus', s2, s1[2]]];
		} else {
			return ['mplus', s2, s1];
		}

	} else if (stream[0] === 'bind') {
		let [_, s, g] = stream;
		s = isMature(s) ? s : step(s);
		
		if (s[0] === 'empty') {
			return ['empty'];
		} else if (s[0] === 'pair') {
			return step(['mplus',
				['pause', s[1], g],
				['bind', s[2], g]
			]);
		} else {
			return ['bind', s, g];
		}

	} else if (stream[0] === 'pause') {
		let [_, st, g] = stream;
		return start(st, g);

	} else {
		return stream;
	}
}
