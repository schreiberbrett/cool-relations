# Usage: `make`
# Repeated usage: `make clean`, then `make`
# It doesn't work with just `make` because it sees the folders are already there.
# I want to improve this by checking the files, not the folders.

bezae-code = \
	bezae-code/main.py \
	bezae-code/Main.hs \
	bezae-code/main.ts \
	bezae-code/main.c \
	bezae-code/main.scm

literate-html = \
	literate-articles/associative-cartesian-product.html \
	literate-articles/rational-numbers.html

literate-code = \
	literate-articles/associative-cartesian-product.py \
	literate-articles/rational-numbers.c

scribble-html = \
	scribble-htmls/main.html \
	scribble-htmls/miniKanren-and-multivariate-horner-schemes.html \
	scribble-htmls/relation-drag-racing.html \
	scribble-htmls/sets-in-miniKanren.html

.PHONY: all
all: $(scribble-html) $(literate-html) $(literate-code) $(bezae-code)

scribble-htmls/%.html: scribble-src/%.scrbl
	scribble --dest scribble-htmls $<

literate-articles/%.py: literate-articles/%.lit
	lit --tangle $<

literate-articles/%.c: literate-articles/%.lit
	lit --tangle $<

literate-articles/%.html: literate-articles/%.lit
	lit --weave $<

$(bezae-code): bezae/bezae.html bezae/bezae-compiler.py
	python3 bezae/bezae-compiler.py


.PHONY: clean
clean:
	rm -f $(scribble-html)
	rm -f $(literate-code)
	rm -f $(literate-html)
	rm -f $(bezae-code)
