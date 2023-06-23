# `defrel` and `run`

miniKanren expressions in existential disjunctive normal form (EDNFs) become especially useful when they have one ore more **simple conjunctions**. A simple conjunction is when one of the inner conjunctions of the EDNF contains only the equality relation `==`.

Consider the following `run` clause:
```scheme
(run 10 (q)
    (fresh (a b c)
        (conde
            ((== a 'x) (== b 'y) (== c 'z))
            ((po a) (qo b) (ro c))
            ((== a 0) (== a 1))
            ((so a b c)))))
```

The first and third conjunctions are simple conjunctions. There is a straightforward unification algorithm when every clause in a conjunction is `==`. Applying this algorithm on the first algorithm succeeds with result `'(x y z)`, which can be added to the output.

Then the `run` expression can remove that simple conjunction and decrement its run number.
```scheme
(run 9 (q)
    (fresh (a b c)
        (conde
            ((po a) (qo b) (ro c))
            ((== a 0) (== a 1))
            ((so a b c)))))
```

The second simple conjunction fails because `a` cannot be both 0 and 1. It can be removed but no result is added to the output and the run number stays 9.

```scheme
(run 9 (q)
    (fresh (a b c)
        (conde
            ((po a) (qo b) (ro c))
            ((so a b c)))))
```

Since there are no more simple conjunctions, the next step is to apply the non-`==` relations to their arguments, that is, replace those relations with their definitions.


Here is the algorithm in full:

```mermaid
graph TD;
    starting_state["Starting state"];
    convert_to_ednf["Convert the formula into an EDNF."];
    try_unifying["Try unifying according to the simple conjunction."];
    remove_simple_conjunction["Remove the simple conjunction from the EDNF."];
    output_unified["Output the unified variabels and decrement the run number."];
    halt["Halt."];
    references_definition["EDNF must reference a definition.\nReplace all definitons with their bodies."];

    run_equal_zero{{"Does the run number equal 0?"}};
    did_unification_succeed{{"Did unification succeed?"}};
    is_ednf_empty{{"Is the EDNF empty?"}};
    contains_a_simple_conjunction{{"Does the EDNF contain\na simple conjunction?"}};
    
    starting_state --> convert_to_ednf;

    try_unifying --> did_unification_succeed;
    
    did_unification_succeed -- "Yes" --> output_unified;
    did_unification_succeed -- "No" --> remove_simple_conjunction;

    output_unified --> remove_simple_conjunction;

    remove_simple_conjunction --> run_equal_zero;
    
    contains_a_simple_conjunction -- "Yes" --> try_unifying;
    
    is_ednf_empty -- "Yes" --> halt;
    is_ednf_empty -- "No" --> contains_a_simple_conjunction;
    contains_a_simple_conjunction -- "No" --> references_definition;
    
    references_definition --> convert_to_ednf;
    convert_to_ednf --> run_equal_zero;
    
    run_equal_zero -- "Yes" --> halt;
    run_equal_zero -- "No" --> is_ednf_empty;
```