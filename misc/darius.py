def e_closure(seen, nfa):
	new_seen = [x for x in move("epsilon", seen, nfa) if x not in seen]
	
	if len(new_seen) == 0:
		return seen
	
	return e_closure(seen + new_seen)

	

