# Defunctionalization

Consider the stream functions from TRS2e:

```scheme
(define (append-inf s-inf t-inf)
  (cond
    ((null? s-inf) t-inf)
    ((pair? s-inf)
     (cons (car s-inf)
           (append-inf (cdr s-inf) t-inf)))
    (else (lambda ()
            (append-inf t-inf (s-inf))))))

(define (append-map-inf g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf (g (car s-inf))
       (append-map-inf g (cdr s-inf))))
    (else (lambda () 
            (append-map-inf g (s-inf))))))
```

Here is a direct translation into Python.

```python
@dataclass
class Empty:
    pass

@dataclass
class Nonempty(Generic[T]):
    first: T
    rest: 'Stream[T]'

@dataclass
class Suspended(Generic[T]):
    suspension: Callable[[], 'Stream[T]']

Stream = Empty | Nonempty[T] | Suspended[T]

def append_inf(s: Stream[T], t: Stream[T]) -> Stream[T]:
    match s:
        case Empty():
            return t
        case Nonempty(first, rest):
            return Nonempty(first, append_inf(rest, t))
        case Suspended(sus):
            return Suspended(lambda: append_inf(t, sus()))
```

Defunctionalization in Python is the process of rewriting every `Callable` and `lambda` in the program.