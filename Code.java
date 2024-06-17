
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Map;
import java.util.NoSuchElementException;
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
public class Dovetail<A, B>
implements Iterator<Map.Entry<A, B>> {
    private Iterator<A> iterA;
    private Iterator<B> iterB;
    private Queue<Map.Entry<A, B>> queue;
    private boolean turnA;
    private List<A> seenA;
    private List<B> seenB;

    public Dovetail(
        Iterator<A> _iterA,
        Iterator<B> _iterB
    ) {
        iterA = _iterA;
        iterB = _iterB;
        queue = new LinkedList<>();
        turnA = true;
        seenA = new ArrayList<>();
        seenB = new ArrayList<>();
    }

    public boolean hasNext() {
        return
            iterA.hasNext() ||
            iterB.hasNext() ||
            !queue.isEmpty();
    }

    public Map.Entry<A, B> next() {
        if (!this.hasNext()) {
            throw new NoSuchElementException();
        }

        if (!queue.isEmpty()) {
            return queue.remove();
        }

        if (turnA && iterA.hasNext()) {
            var a = iterA.next();
            for (var b : seenB) {
                queue.add(Map.entry(a, b));
            }
            seenA.add(a);
        } else if (iterB.hasNext()) {
            var b = iterB.next();
            for (var a : seenA) {
                queue.add(Map.entry(a, b));
            }
            seenB.add(b);
        }

        turnA = !turnA;

        return this.next();
    }
}

public class CountUp
implements Iterator<Integer> {
    private int counter;

    public CountUp() {
        counter = 0;
    }

    public boolean hasNext() {
        return true;
    }

    public Integer next() {
        counter++;
        return counter - 1;
    }
}

// .\9-Misc\1-A-Rule-of-Inference.md
// .\9-Misc\2-Rational-Numbers.md
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
