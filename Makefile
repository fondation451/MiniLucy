CMO_I=ast.cmo lustre_printer.cmo parser.cmi parser.cmo lexer.cmo main.cmo
CMO=ast.cmo lustre_printer.cmo parser.cmo lexer.cmo main.cmo
GENERATED = lexer.ml parser.ml parser.mli
BIN=minilucy
FLAGS=

all: $(BIN)

#$(BIN): $(CMX)

$(BIN): $(CMO_I)
	ocamlc $(FLAGS) -o $(BIN) $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c  $<

.mll.ml:
	ocamllex -q $<

.mly.ml:
	menhir --infer -v $<

.mly.mli:
	menhir --infer -v $<

clean:
	rm -f *.cm[io] *.o *~ $(BIN) $(GENERATED) parser.automaton parser.conflicts .depend

#.depend depend: $(GENERATED)
#	rm -f .depend
#	ocamldep *.ml *.mli > .depend

#include .depend
