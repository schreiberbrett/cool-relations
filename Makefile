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
	literate-articles/associative-cartesian-product/associative-cartesian-product.html \
	literate-articles/rational-numbers/rational-numbers.html

literate-code = \
	literate-articles/associative-cartesian-product/associative-cartesian-product.py \
	literate-articles/rational-numbers/rational-numbers.c

.PHONY: all
all: scribble-htmls literate-htmls literate-code $(bezae-code)

scribble-htmls:
	scribble --dest scribble-htmls scribble-src/*.scrbl

literate-articles/rational-numbers.html: literate-articles/rational-numbers.lit
	lit --weave literate-articles/rational-numbers/rational-numbers.lit --out-dir literate-articles/rational-numbers/

 literate-articles/rational-numbers.c: literate-articles/rational-numbers/rational-numbers.lit
	lit --tangle literate-articles/rational-numbers/rational-numbers.lit --out-dir literate-articles/rational-numbers/

literate-articles/associative-cartesian-product/associative-cartesion-product.html: literate-articles/associative-cartesian-product.lit
	lit --weave literate-articles/associative-cartesian-product/associative-cartesian-product.lit --out-dir literate-articles/associative-cartesian-product/

literate-articles/associative-cartesian-product/associative-cartesian-product.py: literate-articles/associative-cartesian-product.lit
	lit --tangle literate-articles/associative-cartesian-product/associative-cartesian-product.lit --out-dir literate-articles/associative-cartesian-product/


$(bezae-code): bezae/bezae.html bezae/bezae-compiler.py
	python3 bezae/bezae-compiler.py


.PHONY: clean
clean:
	rm $(literate-code)
	rm $(literate-html)
	rm $(bezae-code)
