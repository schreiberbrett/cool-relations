from typing import Dict, Iterable, List, Tuple, TypeVar, cast
from bs4 import BeautifulSoup, Tag

A = TypeVar('A')
B = TypeVar('B')
def to_dict(l: List[Tuple[A, B]]) -> Dict[A, List[B]]:
    result: Dict[A, List[B]] = {}

    for (a, b) in l:
        if a not in result:
            result[a] = []
        
        result[a].append(b)

    return result


filename_for = {
    'Python': 'main.py',
    'Haskell': 'Main.hs',
    'Typescript': 'main.ts',
    'C': 'main.c'
}

Language = str

with open('myazedi.html', 'r') as f:
    contents = f.read()

    soup = BeautifulSoup(contents, 'html.parser')

    pres: Iterable[Tag] = soup.find_all('pre')

    code_segments = [(pre.attrs['class'], pre.text) for pre in pres if 'class' in pre.attrs]

    code_segments_by_class = to_dict(code_segments)

    for classname, segments in code_segments_by_class.items():
        if classname in filename_for:
            with open(filename_for[classname]) as f:
                f.writelines(segments)

