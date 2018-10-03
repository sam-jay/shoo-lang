printer:
	ocamlbuild -use-ocamlfind src/printer.native

test:
	ocamlbuild -use-ocamlfind test/test.native

clean:
	ocamlbuild -clean

.PHONY: test clean