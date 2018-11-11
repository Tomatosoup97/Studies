.PHONY: all clean test

# Find Menhir.
ifndef MENHIR
  MENHIR := $(shell ../find-menhir.sh)
endif

MENHIRFLAGS     := --explain --dump

OCAMLBUILD      := ocamlbuild -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)"

MAIN            := calc

all:
	$(OCAMLBUILD) $(MAIN).native

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean

test: all
	@echo "The following command should print 42:"
	echo "4 + 4 * 10 - 2" | ./$(MAIN).native

