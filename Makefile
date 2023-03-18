all: scribble-htmls literate-htmls 

scribble-htmls:
	scribble --dest-name scribble-htmls --htmls scribble-src/main.scrbl

literate-htmls:
	cd literate-src && \
	lit --weave book.lit && \
	mv _book ../literate-htmls

clean:
	rm -rf scribble-htmls literate-htmls
