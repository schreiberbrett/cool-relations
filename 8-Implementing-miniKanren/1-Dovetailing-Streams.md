# Dovetailing Streams

Streams are an important part of implementing miniKanren. The canonical implementation of miniKanren has an elegant definition of streams: a stream of elements from a set $S$ is either empty, a pair of an element from $S$ and a stream, or a thunk. A thunk is a zero-argument lambda function which, when evaluated (or, "forced", "pulled") results in a stream.

Loosely, miniKanren *goals* return infinite streams of substitutions. One such goal in canonical miniKanren is `conj`, which reconciles two infinite streams and produces the infinite stream of unifiable substitutions, dropping any contradictions.

Since `conj` outputs all unified substitutions and drop all contradictory ones, and since miniKanren promises a complete solution set, `conj` must, at some level, compare all pairs of streams.

This is equivalent to the Cartesian product on two infinite sets. An established way to compute this is a technique called "dovetailing", which records the seen values from each stream and carefully pulls the next element $a$ from stream $X$, and outputs pairs $(x, y)$ for all previously seen values of $Y$. The same argument can be made with $X$ and $Y$ exchanged to perform the complete dovetail.

## Python

Python's `yield` keyword is a convenient syntax for operating on streams, which are called `Iterator`s in Python. The `yield` keyword is actually more expressive and can be used to produce an industrial `Generator` abstract data-type which implements `Iterator`.

```python
def dovetail(
    iterX: Iterator[X],
    iterY: Iterator[Y]
) -> Iterator[Tuple[X, Y]]:
    hasX = True
    hasY = True

    seenX: List[X] = []
    seenY: List[Y] = []

    while hasX or hasY:
        if hasX:
            try:
                x = next(iterX)
                yield from (x, y for y in seenY)
                seenA.append(x)
            except StopIteration:
                hasX = False

        if hasY:
            try:
                y = next(iterY)
                yield from (x, y for x in seenX)
                seenY.append(y)
            except StopIteration:
                hasY = False
```

Essentially, this is an iterative round-robin algorithm which first tries to find the next $a$ from the infinite set of $A$, and then tries to find the next $b$ from the infinite set of $B$. If one of the sets is finite and is exhausted, it proceeds on the other set. If both sets are exhausted, the generator halts.

## Java

The Java programming language also supports streams in its own `Iterator<E>` interface. But Java does not have an equivalent to Python's `yield` keyword, nor does it have a builtin generic datatype meant for representing  pairs.

To perform the infinite cartesian product in Java, one must implement the `Iterator<E>` interface, namely the `next()` and `hasNext()`. The expectation is that the `Iterator` produced representing the Cartesian product is an *object*, which periodically will receive messages to retrieve the next value in the stream of pairs, or to check if such a next value exists at all.

It is possible to derive this Java implementation from the Python version by figuratively turning the code inside-out.

If we take `yield` statements in Python to mean a pause in execution, then when you resume execution, you could be in one of three places:

- Having just taken a value out of $A$ and pairing it with seen $b$s.
- Having just taken a value out of $B$ and pairing it with seen $a$s.
- At the end of the generator function, no more values to be seen.

This makes the definition of `hasNext()` straightforward: the Cartesian iterator has a next value if it currently has a queue of pairs to yield, or if either one of its sources has a next value.

```java
public class Dovetail<X, Y>
implements Iterator<Map.Entry<X, Y>> {
    private Iterator<X> iterX;
    private Iterator<Y> iterY;
    private Queue<Map.Entry<X, Y>> queue;
    private boolean turnX;
    private List<X> seenX;
    private List<Y> seenY;

    public Dovetail(
        Iterator<X> _iterX,
        Iterator<Y> _iterY
    ) {
        iterX = _iterX;
        iterY = _iterY;
        queue = new LinkedList<>();
        turnX = true;
        seenX = new ArrayList<>();
        seenY = new ArrayList<>();
    }

    public boolean hasNext() {
        return
            iterX.hasNext() ||
            iterY.hasNext() ||
            !queue.isEmpty();
    }

    public Map.Entry<X, Y> next() {
        if (!this.hasNext()) {
            throw new NoSuchElementException();
        }

        if (!queue.isEmpty()) {
            return queue.remove();
        }

        if (turnX && iterX.hasNext()) {
            var x = iterX.next();
            for (var y : seenY) {
                queue.add(Map.entry(x, y));
            }
            seenX.add(x);
        } else if (iterY.hasNext()) {
            var y = iterY.next();
            for (var x : seenX) {
                queue.add(Map.entry(x, y));
            }
            seenY.add(y);
        }

        turnX = !turnX;

        return this.next();
    }
}
```

And some test code:

```java
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
```

And the result:

```
jshell> var x = new Dovetail<>(new CountUp(), new CountUp());
x ==> Dovetail@77a567e1

jshell> for (int i = 0; i < 10; i++) { System.out.println(x.next()); }
0=0
1=0
0=1
1=1
2=0
2=1
0=2
1=2
2=2
3=0
```

## Scheme

```scheme
(define (dovetail $x $y)
  (let loop (($x $x)      ($y $y)
             (seen-x '()) (seen-y '())
             (turn 'x))
    (cond ((and (null? $x) (null? $y)) '())

          ((and (null? $x) (equal? turn 'x)) (loop '() $y seen-x seen-y 'y))
          ((and (null? $y) (equal? turn 'y)) (loop $x '() seen-x seen-y 'x))

          ((procedure? $x) (λ () (loop ($x) $y  seen-x seen-y turn)))
          ((procedure? $y) (λ () (loop  $x ($y) seen-x seen-y turn)))
          
          ((equal? turn 'x) (append
                              (map (λ (y) (cons (car $x) y)) seen-y)
                              (loop (cdr $x) $y (cons (car $x) seen-x) seen-y 'y)))
          ((equal? turn 'y) (append
                              (map (λ (x) (cons x (car $y))) seen-x)
                              (loop $x (cdr $y) seen-x (cons (car $y) seen-y) 'x))))))
```

Helpers:

```scheme
(define (count-up n)
  (cons n (lambda () (count-up (+ n 1)))))


(define (take n $)
  (cond ((or (zero? n) (null? $)) '())
        ((pair? $) (cons (car $) (take (- n 1) (cdr $))))
        (else (take n ($)))))
```

And the results:

```
> (take 20 (dovetail (count-up 0) (count-up 0)))
'((0 . 0)
  (1 . 0)
  (1 . 1)
  (0 . 1)
  (2 . 1)
  (2 . 0)
  (2 . 2)
  (1 . 2)
  (0 . 2)
  (3 . 2)
  (3 . 1)
  (3 . 0)
  (3 . 3)
  (2 . 3)
  (1 . 3)
  (0 . 3)
  (4 . 3)
  (4 . 2)
  (4 . 1)
  (4 . 0))
```