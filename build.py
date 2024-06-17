import os
import sys
import subprocess

current_lang = None

file = {}

file['racket'] = open('code.rkt', 'w', encoding='utf-8')
file['java']   = open('Code.java', 'w', encoding='utf-8')
file['python'] = open('code.py', 'w', encoding='utf-8')

# Import statements

file['racket'].write('''
#lang racket

(require "../faster-minikanren/main.rkt")
(require "../faster-minikanren/numbers.rkt")
''')

file['python'].write('''
from typing import List, Set, Tuple, Dict, TypeVar
from dataclasses import dataclass
                  
A = TypeVar('A')
B = TypeVar('B')
T = TypeVar('T')
''')

file['java'].write('''
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Map;
import java.util.NoSuchElementException;
''')

# Look for markdown files in all subdirectories, in any order.
# Copy code blocks into their corresponding files

for dirpath, _, filenames in os.walk('.'):
    for filename in filenames:
        if filename.endswith('.md'):
            full = os.path.join(dirpath, filename)
            file['racket'].write(';; ' + full + '\n')
            file['java'].write('// ' + full + '\n')
            file['python'].write('# ' + full + '\n')
            with open(full, 'r', encoding='utf-8') as f:
                for l in f.readlines():
                    if current_lang is None:
                        if l.startswith('```scheme'):
                            current_lang = 'racket'
                        if l.startswith('```java'):
                            current_lang = 'java'
                        if l.startswith('```python'):
                            current_lang = 'python'
                    elif l.startswith('```'):
                        file[current_lang].write('\n')
                        current_lang = None
                    else:
                        file[current_lang].write(l)


for f in file.values():
    f.close()


# If a command line argument is given, jump into that REPL

my_env = os.environ.copy()

if 'racket' in sys.argv:
    # subprocess.run('racket -i -e "(enter! \\"code.rkt\\")"')
    os.startfile('code.rkt')