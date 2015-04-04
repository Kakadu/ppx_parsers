OCAMLC=ocamlfind c
OCAMLOPT=ocamlfind opt
COMPILER_OPTS=-g -I 1 -unsafe -unsafe-string -ccopt -O9 #-inline 9
WITH_PPX=-ppx ../_build/src/ppx_getenv.native
OUT=demo.byte
OUT_TEST=test.byte
OUT_BENCH=bench.native
TEST_NAMES=a b c d f g h i tstMutalRecursion tstFloat tstStringLit
TEST_CMOS=$(addsuffix .cmo,$(TEST_NAMES) )

.SUFFIXES: .byte .native
.PHONY: all test demo bench clean

all: demo test bench
demo: $(OUT)
test: $(OUT_TEST)
bench: $(OUT_BENCH)


define TEST_RULES
test.cmo: $(1).cmo 1/comb.cma
$(1).cmo: $(1).ml testLexer.cmo
	$(OCAMLC) $(COMPILER_OPTS) $(WITH_PPX) -c $(1).ml
endef

$(foreach i,$(TEST_NAMES),$(eval $(call TEST_RULES,$(i)) ) )

$(OUT): testLexer.cmo 1/comb.cma $(TEST_CMOS) demo.cmo
	$(OCAMLC) $(COMPILER_OPTS) -linkpkg $^ -o $@

test: $(OUT_TEST)
test: OCAMLC += -package oUnit

testLexer.cmo: testLexer.ml
	$(OCAMLC) $(COMPILER_OPTS) -c $<

testLexer.cmx: testLexer.ml
	$(OCAMLOPT) $(COMPILER_OPTS) -c $<

demo.cmo: $(TEST_CMOS)
demo.cmo: demo.ml
	$(OCAMLC) $(COMPILER_OPTS) -c $<

test.cmo: test.ml
	$(OCAMLC) $(COMPILER_OPTS) -c $<

$(OUT_TEST): testLexer.cmo 1/comb.cma $(addsuffix .cmo,$(TEST_NAMES)) test.cmo
	$(OCAMLC) $(COMPILER_OPTS) $^ -linkpkg -o $@

comb.cma:
#../_build/src/ppx_getenv.native:

$(OUT_BENCH): COMPILER_OPTS += -package extlib,core_bench -thread
$(OUT_BENCH): testLexer.cmx  1/comb.cmxa tstJSON.cmx bench.cmx
	$(OCAMLOPT) $(COMPILER_OPTS) $^ -linkpkg -o $@

bench.cmx: bench.ml tstJSON.cmx
	$(OCAMLOPT) $(COMPILER_OPTS) -c $<


tstJSON.cmx: tstJSON.ml 1/comb.cmxa testLexer.ml
	$(OCAMLOPT) $(COMPILER_OPTS) $(WITH_PPX) -c $<

clean:
	$(RM) *.cm[iox] *.o $(OUT)

# TODO: when  we change ppx_...native files are not recompiled. FIX it!
