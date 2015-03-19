OCAMLC=ocamlfind c -I 1
OUT=main

$(OUT):
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -ppx ../_build/src/ppx_getenv.native -c a.ml -dsource
	$(OCAMLC) -c main.ml
	$(OCAMLC) -linkpkg lexer.cmo comb.cmo a.cmo main.cmo -o $(OUT)

clean:
	$(RM) *.cm[iox] *.o $(OUT)
