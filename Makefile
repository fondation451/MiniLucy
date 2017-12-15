LUS_I=ast_type.cmo ast.cmo ast_lustre.cmo ast_object.cmo ast_schedule.cmo lustre_printer.cmo type.cmo clocking.cmo normalize.cmo schedule.cmo object.cmo to_c.cmo to_caml.cmo export_source.cmo parser.cmi parser.cmo lexer.cmo
LUS=ast_type.cmo ast.cmo ast_lustre.cmo ast_object.cmo ast_schedule.cmo lustre_printer.cmo type.cmo clocking.cmo normalize.cmo schedule.cmo object.cmo to_c.cmo to_caml.cmo export_source.cmo parser.cmo lexer.cmo
ELUS_I=east.cmo elustre_printer.cmo eparser.cmi eparser.cmo elexer.cmo etype.cmo easttoast.cmo
ELUS=east.cmo elustre_printer.cmo eparser.cmo elexer.cmo etype.cmo easttoast.cmo
COMMON=main.cmo
GENERATED_LUS=lexer.ml parser.ml parser.mli parser.automaton parser.conflicts
GENERATED_ELUS=elexer.ml eparser.ml eparser.mli eparser.automaton eparser.conflicts
GENERATED_COMMON=.depend a.out out
BIN=minilucy
FLAGS=

.PHONY: test

all: $(BIN)

remake: clean all

test:
	./auto_test.sh

test_c:
	./auto_test.sh -clang minilucy

#$(BIN): $(CMX)

$(BIN): $(LUS_I) $(ELUS_I) $(COMMON)
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
	rm -f *.cm[io] *.o *~ $(BIN) $(GENERATED_LUS) $(GENERATED_ELUS) $(GENERATED_COMMON)
	rm -f tests/syntax/good/*.c

#.depend depend: $(GENERATED)
#	rm -f .depend
#	ocamldep *.ml *.mli > .depend

#include .depend
