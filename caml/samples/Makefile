OCAMLC=ocamlfind c -I 1
OUT=main

.PHONY: test

$(OUT):
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -ppx ../_build/src/ppx_getenv.native -c a.ml -dsource
	$(OCAMLC) -c main.ml
	$(OCAMLC) -linkpkg lexer.cmo comb.cmo a.cmo main.cmo -o $(OUT)

test: OCAMLC += -package oUnit
test:
	$(OCAMLC) -ppx ../_build/src/ppx_getenv.native -c a.ml
	$(OCAMLC) -c test.ml
	$(OCAMLC) lexer.cmo comb.cmo a.cmo test.cmo -linkpkg -o test

clean:
	$(RM) *.cm[iox] *.o $(OUT)
