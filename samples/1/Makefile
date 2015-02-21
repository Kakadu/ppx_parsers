OCAMLOPT=ocamlfind opt -package extlib
OUT_EXPR=expr1.native
OUT_JSON=json1.native
OUTS=$(OUT_EXPR) $(OUT_JSON)

.SUFFIXES: .cmx .cmi .ml

all: $(OUTS)

$(OUT_EXPR): comb.cmx main.cmx
	$(OCAMLOPT) $^ -o $@

$(OUT_JSON): comb.cmx jsonparser.cmx
	$(OCAMLOPT) $^ -linkpkg -o $@

main.cmx: comb.cmx
jsonparser.cmx: comb.cmx

.ml.cmx:
	$(OCAMLOPT) -c $<

clean:
	$(RM) $(OUTS) *.cmx *.cmo *.cmi