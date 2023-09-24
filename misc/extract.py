from collections import Counter
from typing import Optional, TypeVar, Hashable, Generic, List, Tuple
from dataclasses import dataclass

A = TypeVar('A', bound=Hashable)
B = TypeVar('B')


@dataclass
class Base(Generic[A, B]):
    no_max: List[List[Tuple[A, B]]]

@dataclass
class Rec(Generic[A, B]):
    most_common: A
    args: List[B]
    had: 'Result[A, B]'
    lacked: 'Result[A, B]'

Result = Rec[A, B] | Base[A, B]


def extract(l: List[List[Tuple[A, B]]]) -> Result[A, B]:
    counter = Counter[A]()
    for x in l:
        for (a, _) in x:
            counter[a] += 1
            break

    most_common = counter.most_common()

    if len(most_common) == 0:
        return Base(l)
    
    (a, count) = most_common[0]
    if count == 1:
        return Base(l)
    
    had: List[List[Tuple[A, B]]] = []
    lacked: List[List[Tuple[A, B]]] = []
    args: List[B] = []
    for x in l:
        b, plucked = pluck(a, x)
        if b is None:
            lacked.append(plucked)
        else:
            had.append(plucked)
            args.append(b)

    x = extract(had)

    return Rec(
        a,
        args,
        extract(had),
        extract(lacked)
    )


def pluck(a: A, assoc_list: List[Tuple[A, B]]) -> Tuple[Optional[B], List[Tuple[A, B]]]:
    for i, (key, value) in enumerate(assoc_list):
        if key == a:
            # Found the desired element, return the associated value and the updated list
            updated_assoc_list = assoc_list[:i] + assoc_list[i+1:]
            return value, updated_assoc_list

    # If the key wasn't found, return None for the associated value and the original list
    return None, assoc_list