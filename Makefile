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

.PHONY: all
all: scribble-htmls literate-htmls literate-code $(bezae-code)

scribble-htmls:
	scribble --dest-name scribble-htmls --htmls scribble-src/main.scrbl

literate-htmls:
	cd literate-src && \
	lit --weave book.lit && \
	rm -rf ../literate-htmls && \
	mv _book ../literate-htmls

literate-code:
	mkdir -p literate-code && \
	cp literate-src/*.lit literate-code/ && \
	cd literate-code && \
	lit --tangle *.lit && \
	rm *.lit

$(bezae-code): bezae/bezae.html bezae/bezae-compiler.py
	python3 bezae/bezae-compiler.py


.PHONY: clean
clean:
	rm -rf scribble-htmls literate-htmls literate-code
	rm $(bezae-code)
