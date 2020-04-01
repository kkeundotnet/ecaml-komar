PACKAGES = extlib
FILES = parsercomb.ml ecaml.ml

NAME = ecaml
VERSION := $(shell head -n 1 VERSION)
CAMLC   = ocamlfind ocamlc   $(LIB)
CAMLOPT = ocamlfind ocamlopt $(LIB)
CAMLDEP = ocamlfind ocamldep
LIB = -package $(PACKAGES)
PP =

PREFIX=/usr/local

OBJS    = $(FILES:.ml=.cmo)
OPTOBJS = $(FILES:.ml=.cmx)

all: $(NAME)

$(NAME): $(OBJS)
	$(CAMLC) -linkpkg -o $@ $^

$(NAME).opt: $(OPTBJS)
	$(CAMLOPT) -linkpkg -o $@ $^

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(PP) -c $<
.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

install: $(NAME)
	install -m 755 $(NAME) $(PREFIX)/bin/$(NAME)

clean:
	-rm -f *.cm[ioxa] *.cmx[as] *.o *.a *~
	-rm -f .depend
	-rm -f $(NAME) $(NAME).opt

depend: .depend

.depend: $(FILES)
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

-include .depend
