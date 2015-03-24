OCAMLC=ocamlfind c -g -I 1
OUT=demo
OUT_TEST=test
TEST_NAMES=a b c d e f g h
TEST_CMOS=$(addsuffix .cmo,$(TEST_NAMES) )

.PHONY: all clean

all: $(OUT)

define TEST_RULES
test.cmo: $(1).cmo
$(1).cmo: $(1).ml testLexer.cmo
	$(OCAMLC) -ppx ../_build/src/ppx_getenv.native -c $(1).ml
endef

$(foreach i,$(TEST_NAMES),$(eval $(call TEST_RULES,$(i)) ) )

$(OUT): testLexer.cmo comb.cmo $(TEST_CMOS) demo.cmo
	$(OCAMLC) -linkpkg $^ -o $@

test: OCAMLC += -package oUnit

testLexer.cmo: testLexer.ml
	$(OCAMLC) -c $<

demo.cmo: $(TEST_CMOS)
demo.cmo: demo.ml
	$(OCAMLC) -c $<

test.cmo: OCAMLC += -package oUnit
test.cmo: test.ml
	$(OCAMLC) -package oUnit -c $<

$(OUT_TEST): testLexer.cmo comb.cmo $(addsuffix .cmo,$(TEST_NAMES)) test.cmo
	$(OCAMLC) $^ -linkpkg -o $@

comb.cmo:
#../_build/src/ppx_getenv.native:

clean:
	$(RM) *.cm[iox] *.o $(OUT)

# TODO: when  we change ppx_...native files are not recompiled. FIX it!
