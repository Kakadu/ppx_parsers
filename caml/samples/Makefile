OCAMLC=ocamlfind c -g -I 1
OUT=demo
OUT_TEST=test

.PHONY: all clean

all: $(OUT)

$(OUT): lexer.cmo comb.cmo a.cmo b.cmo demo.cmo
	$(OCAMLC) -linkpkg $^ -o $(OUT)

test: OCAMLC += -package oUnit

lexer.cmo: lexer.ml
	$(OCAMLC) -c $<

demo.cmo: a.cmo b.cmo
demo.cmo: demo.ml a.cmo b.cmo
	$(OCAMLC) -c $<

a.cmo: lexer.cmo #../_build/src/ppx_getenv.native
a.cmo: a.ml
	$(OCAMLC) -ppx ../_build/src/ppx_getenv.native -c $<

b.cmo: lexer.cmo
b.cmo: b.ml
	$(OCAMLC) -ppx ../_build/src/ppx_getenv.native -dsource -c $<

test.cmo: a.cmo
test.cmo: OCAMLC += -package oUnit
test.cmo: test.ml
	$(OCAMLC) -package oUnit -c $<

test: lexer.cmo comb.cmo a.cmo b.cmo test.cmo
	$(OCAMLC) $^ -linkpkg -o test

comb.cmo:
#../_build/src/ppx_getenv.native:

clean:
	$(RM) *.cm[iox] *.o $(OUT)

# TODO: when  we change ppx_...native files are not recompiled. FIX it!
