# Usage: `make`
# Repeated usage: `make clean`, then `make`
# It doesn't work with just `make` because it sees the folders are already there.
# I want to improve this by checking the files, not the folders.

all: scribble-htmls literate-htmls literate-code

scribble-htmls:
	scribble --dest-name scribble-htmls --htmls scribble-src/main.scrbl

literate-htmls:
	cd literate-src && \
	lit --weave book.lit && \
	mv _book ../literate-htmls

literate-code:
	mkdir -p literate-code && \
	cp literate-src/*.lit literate-code/ && \
	cd literate-code && \
	lit --tangle *.lit && \
	rm *.lit

clean:
	rm -rf scribble-htmls literate-htmls literate-code
