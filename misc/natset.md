# Relational Nat Sets

Consider the of little-endian binary natural numbers:
`{(0 0 1), (1 1), ()}`

We can represent this as a binary tree with boolean nodes representing set membership for the binary number describing the node's location.

```haskell
data BinaryTree = BinaryTree
    Boolean -- has
    (Maybe BinaryTree) -- left
    (Maybe BinaryTree) -- right
```
Observations:
- All rooted nodes must end in a 1 (must be right-children), and must be true.
- Therefore left children must have a right child.
- Left children also never have membership


```haskell
data BinaryTree = BinaryTree
    Boolean -- has
    (Maybe LeftChild) -- left
    (Maybe BinaryTree) -- right

data LeftChild = LeftChild
    (Maybe LeftChild) -- left
    BinaryTree -- right
```

But `(BinaryTree False None None)` should be impossible. It is a dead branch of the tree. But that means we need to think positively about the different states. In general, a pair `((Maybe a) (Maybe b))` that disallows `(Nothing, Nothing)` is an `(Or a b)`.

```haskell
data Or a b = Left a | Right b | Both a b

data BinaryTree = Root | HasChild Bool (Xor LeftChild BinaryTree)

data LeftChild = LeftChild
    (Maybe LeftChild)
    BinaryTree
```

But now we lost the ability to express the empty set, let's add it back:

```haskell
data Or a b = Left a | Right b | Both a b

type BinaryTree = Maybe NonemptyBinaryTree

data NonemptyBinaryTree = Root | HasChild Bool (Or LeftChild NonemptyBinaryTree)

data LeftChild = LeftChild
    (Maybe LeftChild)
    NonemptyBinaryTree
```

Cleaning this up a bit

```haskell
data Nil a = Nil (Maybe a) (Maybe (Zero a)) (Maybe (One a))
data Zero a = Zero (Maybe (Zero a)) (One a)
data One a = HasVal a (Maybe (Zero a)) (Maybe (One a)) | NoVal (Or (Zero a) (One a))
```