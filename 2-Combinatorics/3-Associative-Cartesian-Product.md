# The Associative Cartesian Product

The n-ary cartesian product when and $k = 4$ is something like:

```python
def concrete_product(l: List[List[T]]) -> List[List[T]]:
    l1, l2, l3, l4 = l
    result = []
    for e1 in l1:
        for e2 in l2:
            for e3 in l3:
                for e4 in l4:
                    result.append([e1, e2, e3, e4])

    return result
```

If all inner lists have size $n$, then the running time is $O(n^4)$ for the above. It should be possible to make an algorithm with runtime $O(n^k)$ when the input list is size $k$. In fact, that is the lower bound, since the associative cartesian product requires $O(n^k)$ space to write its output.

Consider the associative cartesian product:

```math
{0, 1, 2, 3, 4, 5, 6, 7, 8, 9} \times {0, 1, 2, 3, 4, 5, 6, 7, 8, 9} = {
    {0, 0},
    {0, 1},
    {0, 2},
    {0, 3},
    {0, 4},
    {0, 5} ...
    {9, 8}, {9, 9}
    }
```

This set has a one-to-one correspondence to the numbers 0 - 99. In fact, it is their base 10 representation. In general:

```math
\bigtimes^n {0, 1, 2, 3, 4, 5, 6, 7, 8, 9} = \text{A set isomorphic to} {0 ... 9^n}
```

Moreover, this works for binary, as well as any base.

```math
\bigtimes^n {0, 1} = \text{A set isomorphic to {0 ... 2^n}}
```

This even works when the sets aren't uniform, where it is known as *mixed radix*. So I will shift focus and work on writing a function that calculates the mixed radix representation of the numbers 0 through $\prod_{i = 1}^n b_i$, where $b_i$ is the base for the $i$th index. (We need multiple $b$s for each radix). These are always going to be whole numbers stopping at $\prod_{i = 1}^n b_i$, so I don't have to worry about decimal points. I will call this variation *finite mixed radix*.

## Calculating the mixed radix representation of a number $x$

Consider the number $392$. Its base 10 representation means:

$$392 = 3 * 100 + 9 * 10 + 2 * 1$$
$$392 = 3 * 10 * 10 + 9 * 10 + 2 * 1$$
$$392 = 2 * 1 + 9 * 1 * 10 + 3 * 1 * 10 * 10$$

We can consider "uniform" radix to be a special case of finite mixed radix. So, if $x = 392$, then, assuming a limit of 9999:

$$x = 2 + 9b_1 + 3b_1 b_2 + 0b_1b_2_b3 + 0b_1b_2b_3b_4$$

where $b_1 = b_2 = b_3 = b_4 = 10$.

In general:

$$x = d_0 + d_1b_1 + d_2b_1b_2 + d_3b_1b_2b_3 + \ldots + d_nb_1b_2 \cdots b_n$$

Applying Horner's lemma:

$$x = d_0 + b_1(d_1 + b_2(d_2 + b_3(d_3 + (\ldots b_n(d_n) \ldots ))))$$

