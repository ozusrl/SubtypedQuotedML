.PHONY: src/main.byte src/main.native

run: src/main.byte src/_build/.ocamlinit
	./src/main.byte

src/main.byte:
	cd src && ocamlbuild -use-ocamlfind main.byte

src/main.native:
	cd src && ocamlbuild -use-ocamlfind main.native

src/_build/.ocamlinit:
	cp .ocamlinit src/_build/.ocamlinit

clean:
	rm -rf src/_build
