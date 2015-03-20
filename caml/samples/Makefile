OCAMLC=ocamlfind c -g -I 1
OUT=demo
OUT_TEST=test

$(OUT): lexer.cmo comb.cmo a.cmo demo.cmo
	$(OCAMLC) -linkpkg $^ -o $(OUT)

test: OCAMLC += -package oUnit

lexer.cmo: lexer.ml
	$(OCAMLC) -c $<

demo.cmo: a.cmo
demo.cmo: demo.ml a.cmo
	$(OCAMLC) -c $<

a.cmo: lexer.cmo
a.cmo: a.ml
	$(OCAMLC) -ppx ../_build/src/ppx_getenv.native -c $<

test.cmo: a.cmo
test.cmo: OCAMLC += -package oUnit
test.cmo: test.ml
	$(OCAMLC) -package oUnit -c $<

test: lexer.cmo comb.cmo a.cmo test.cmo
	$(OCAMLC) $^ -linkpkg -o test

comb.cmo:

clean:
	$(RM) *.cm[iox] *.o $(OUT)
