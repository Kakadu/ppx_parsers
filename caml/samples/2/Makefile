.PHONY: all
.SUFFIXES: .ml .cmx .cmi .o

all: test2

test2: main.cmx
	ocamlfind opt $< -o test2

.ml.cmx:
	ocamlfind opt -c $<
