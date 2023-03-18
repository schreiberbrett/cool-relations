all: scribble-htmls literate-htmls 

scribble-htmls:
	scribble --dest-name scribble-htmls --htmls src/docs.scrbl

literate-htmls:
    lit --weave literate-src/book.lit
    # lit does not have a --dest-name flag. It can be emulated with mv
    mv _book literate-htmls

clean:
    rm -rf scribble-htmls literate-htmls


