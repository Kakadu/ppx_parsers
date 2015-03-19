OCAMLOPT=ocamlfind opt -package extlib
OCAMLC=ocamlfind c -package extlib
OUT_EXPR=expr1.native
OUT_EXPR_BYTE=expr1.byte
OUT_JSON=json1.native
OUT_JSON_BYTE=json1.byte
OUTS=$(OUT_EXPR) $(OUT_JSON) $(OUT_EXPR_BYTE) $(OUT_JSON_BYTE)

.SUFFIXES: .cmx .cmo .cmi .ml

all: $(OUTS)

$(OUT_EXPR): comb.cmx main.cmx
	$(OCAMLOPT) $^ -o $@

$(OUT_EXPR_BYTE): comb.cmo main.cmo
	$(OCAMLC) $^ -o $@

$(OUT_JSON): comb.cmx combErr.cmx jsonparser.cmx
	$(OCAMLOPT) $^ -linkpkg -o $@

$(OUT_JSON_BYTE): comb.cmo combErr.cmo jsonparser.cmo
	$(OCAMLC) $^ -linkpkg -o $@

main.cmx: comb.cmx
main.cmo: comb.cmo
combErr.cmx: comb.cmx
combErr.cmo: comb.cmo
jsonparser.cmx: combErr.cmx
jsonparser.cmo: combErr.cmo

.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.cmo:
	$(OCAMLC) -c $<

clean:
	$(RM) $(OUTS) *.cmx *.cmo *.cmi
