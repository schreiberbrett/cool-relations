from typing import List, TypeVar

T = TypeVar('T')

def concrete_product(l: List[List[T]]) -> List[List[T]]:
    l1, l2, l3, l4 = l
    result = []
    for e1 in l1:
        for e2 in l2:
            for e3 in l3:
                for e4 in l4:
                    result.append([e1, e2, e3, e4])

    return result


