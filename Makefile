LUS_I=ast_type.cmo ast.cmo ast_lustre.cmo ast_object.cmo lustre_printer.cmo type.cmo clocking.cmo normalize.cmo schedule.cmo object.cmo to_c.cmo export_source.cmo parser.cmi parser.cmo lexer.cmo
LUS=ast_type.cmo ast.cmo ast_lustre.cmo ast_object.cmo lustre_printer.cmo type.cmo clocking.cmo normalize.cmo schedule.cmo object.cmo to_c.cmo export_source.cmo parser.cmo lexer.cmo
ELUS_I=east.cmo elustre_printer.cmo eparser.cmi eparser.cmo elexer.cmo
ELUS=east.cmo elustre_printer.cmo eparser.cmo elexer.cmo
COMMON_I=main.cmo
COMMON=main.cmo
GENERATED_LUS=lexer.ml parser.ml parser.mli parser.automaton parser.conflicts
GENERATED_ELUS=elexer.ml eparser.ml eparser.mli eparser.automaton eparser.conflicts
BIN=minilucy
FLAGS=

all: $(BIN)

test:
	./auto_test.sh minilucy

#$(BIN): $(CMX)

$(BIN): $(LUS_I) $(ELUS_I) $(COMMON_I)
	ocamlc $(FLAGS) -o $(BIN) $(LUS) $(ELUS) $(COMMON)

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
	rm -f *.cm[io] *.o *~ $(BIN) $(GENERATED_LUS) $(GENERATED_ELUS) .depend

#.depend depend: $(GENERATED)
#	rm -f .depend
#	ocamldep *.ml *.mli > .depend

#include .depend
