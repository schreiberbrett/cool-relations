import sys

def extract_markdown_block():
    in_minikanren_block = False

    for line in sys.stdin:
        if line.strip() == "```scheme":
            in_minikanren_block = True
            continue
        elif in_minikanren_block and line.strip() == "```":
            in_minikanren_block = False
            continue

        if in_minikanren_block:
            sys.stdout.write(line)

if __name__ == "__main__":
    extract_markdown_block()

