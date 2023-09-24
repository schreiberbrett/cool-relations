from collections import Counter
from typing import TypeVar, Hashable, Generic, List, Set, Optional
from dataclasses import dataclass

# https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.330.7430&rep=rep1&type=pdf

H = TypeVar('H', bound=Hashable)

@dataclass
class Result(Generic[H]):
    most_common_value: H
    had_value: Optional['Result[H]']
    lacked_value: Optional['Result[H]']

def composition(x: Optional[Result[H]]) -> List[Set[H]]:
    if x is None:
        return []
    

    l = composition(x.had_value)
    r = composition(x.lacked_value)

    return [s | set([x.most_common_value]) for s in l] + r

def min_height_decomposition(sets: List[Set[H]]) -> Optional[Result[H]]:
    counter = Counter[H]()
    for s in sets:
        for h in s:
            counter[h] += 1

    most_common = counter.most_common(1)
    if len(most_common) == 0:
        return None
    
    (most_common_value, _) = most_common[0]

    return Result(
        most_common_value = most_common_value,

        had_value = min_height_decomposition(
            [s - set([most_common_value]) for s in sets if most_common_value in s]
        ),

        lacked_value = min_height_decomposition(
            [s for s in sets if most_common_value not in s]
        )
    )


if __name__ == '__main__':
    x = min_height_decomposition([
        {'a', 'b', 'c', 'd'     },
        {'a',      'c'          },
        {'a', 'b'               },
        {'a',           'd', 'e'}
    ])

    print(composition(x))