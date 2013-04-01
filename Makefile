.PHONY: src/main.byte src/main.native

run: src/main.byte
	./src/main.byte

native: src/main.native
	rlwrap ./src/main.native
	

src/main.byte:
	cd src && ocamlbuild -use-ocamlfind main.byte

src/main.native:
	cd src && ocamlbuild -use-ocamlfind main.native

clean:
	rm -rf src/_build
