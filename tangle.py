import sys
import os

extensions = {
    "```python\n": ".py",
    "```haskell\n": ".hs",
    "```js\n": ".js",
    "```html\n": ".html",
    "```c\n": ".c",
    "```scheme\n": ".mk.rkt"
}

def copy_code_blocks(filename):
    try:
        with open(filename + '.md', 'r') as markdown_file:
            ext = None
            output_files = {}
            for line in markdown_file:                
                if ext is None and line in extensions:
                    ext = extensions[line]
                    if ext not in output_files:
                        output_files[ext] = open(filename + ext, 'w')
                        if ext == '.mk.rkt':
                            output_files[ext].write('#lang racket\n\n')
                elif ext is not None and line == '```\n':
                    output_files[ext].write('\n')
                    ext = None
                elif ext is not None:
                    output_files[ext].write(line)

            for x in output_files.values():
                x.close()
    except FileNotFoundError:
        print(f"Error: File '{filename}.md' not found.")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python script.py <filename>.md")
        exit()
    
    filename, ext = sys.argv[1][:-3], sys.argv[1][-3:]
    
    if ext != '.md':
        print("Usage: python script.py <filename>.md")
        exit()
    
    copy_code_blocks(filename)

