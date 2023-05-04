from typing import Dict, Hashable, Iterable, List, Tuple, TypeVar
from bs4 import BeautifulSoup, Tag

H = TypeVar('H', bound=Hashable)
T = TypeVar('T')
def to_dict(l: List[Tuple[H, T]]) -> Dict[H, List[T]]:
    result: Dict[H, List[T]] = {}

    for (a, b) in l:
        if a not in result:
            result[a] = []
        
        result[a].append(b)

    return result


filename_for = {
    'language-python': 'main.py',
    'language-haskell': 'Main.hs',
    'language-typescript': 'main.ts',
    'language-c': 'main.c',
    'language-scheme': 'main.scm'
}

Language = str

with open('bezae/bezae.html', 'r') as f:
    contents = f.read()

    soup = BeautifulSoup(contents, 'html.parser')

    pres: Iterable[Tag] = soup.find_all('code')

    code_segments = [
        (pre.attrs['class'][0], pre.text)
        for pre in pres
        if 'class' in pre.attrs
    ]

    code_segments_by_class = to_dict(code_segments)

    for classname, segments in code_segments_by_class.items():
        if classname in filename_for:
            with open(f'bezae-code/{filename_for[classname]}', 'w') as f:
                f.writelines(segments)

