# Preface
In this article, I will use the following convention:

| Vocabulary | Java meaning | Haskell meaning |
| ---------- | ------------ | --------------- |
| type       | interface    | data type       |
| subtype    | subclass     | constructor     |
| operation  | method       | function        |

# Original code
Consider a type `X` with two subtypes `X1` and `X2`, and two operations on `X`. Two subtypes & two operations = 4 definitons.

In Java, this would be an interface with 2 subclasses and 2 methods.
```java
public interface X {
    int f1();
    int f2();
}

public class X1 implements X {
    int f1() { /* ... */ }
    int f2() { /* ... */ }
}

public class X2 implements X {
    int f1() { /* ... */ }
    int f2() { /* ... */ }
}
```

In Haskell, this would be a data type with 2 constructors and 2 functions.
```haskell
data X
    = X1
    | X2

f1 :: X -> Int
f1 X1 = {- ... -}
f1 X2 = {- ... -}

f2 :: X -> Int
f2 X1 = {- ... -}
f2 X2 = {- ... -}
```

# Adding a third operation`f3`
In Java, the new code is divided into 3 parts.
```java
public interface X {
    int f1();
    int f2();
    int f3(); // NEW
}

public class X1 extends X {
    int f1() { /* ... */ }
    int f2() { /* ... */ }
    int f3() { /* ... */ } // NEW
}

public class X2 extends X {
    int f1() { /* ... */ }
    int f2() { /* ... */ }
    int f3() { /* ... */ } // NEW
}
```

In Haskell, it is together.
```haskell
data X
    = X1
    | X2

f1 :: X -> Int
f1 X1 = {- ... -}
f1 X2 = {- ... -}

f2 :: X -> Int
f2 X1 = {- ... -}
f2 X2 = {- ... -}

f3 :: X -> Int    -- NEW
f3 X1 = {- ... -} -- NEW
f3 X2 = {- ... -} -- NEW
```

# Adding a third subtype `X3` instead
In Java, the new code for `X3` is grouped together.

```java
public interface X {
    int f1();
    int f2();
}

public class X1 extends X {
    int f1() { /* ... */ }
    int f2() { /* ... */ }
}

public class X2 extends X {
    int f1() { /* ... */ }
    int f2() { /* ... */ }
}

public class X3 extends X { // NEW
    int f1() { /* ... */ }  // NEW
    int f2() { /* ... */ }  // NEW
}                           // NEW
```

In Haksell, it is divided into three parts.
```haskell
data X
    = X1
    | X2
    | X3 -- NEW

f1 :: X -> Int
f1 X1 = {- ... -}
f1 X2 = {- ... -}
f1 X3 = {- ... -} -- NEW

f2 :: X -> Int
f2 X1 = {- ... -}
f2 X2 = {- ... -}
f2 X3 = {- ... -} -- NEW
```