all:
	shoo

shoo:
	opam config exec -- \
	ocamlbuild -use-ocamlfind src/shoo.native

semant:
	ocamlbuild -use-ocamlfind src/semant.native

printer:
	ocamlbuild -use-ocamlfind src/printer.native

infer:
	ocamlbuild -use-ocamlfind src/infer.native

test:
	ocamlbuild -use-ocamlfind test/test.native

clean:
	ocamlbuild -clean

.PHONY: test clean all