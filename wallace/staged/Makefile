CAMLC=ocamlc
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
CAMLOPT=ocamlopt
CAMLDEP=ocamldep

INCLUDE=-I $(TOYDIR)
COMPFLAGS=-g $(INCLUDE)

TOYDIR=../toy

TOY=$(TOYDIR)/toy.cma

OBJS=common.cmo parser.cmo lexer.cmo idGen.cmo types.cmo translate.cmo print.cmo eval.cmo translateToToy.cmo simplify.cmo main.cmo

LEXER_SOURCE=lexer.mll
LEXER_TARGET=lexer.ml

PARSER_SOURCE=parser.mly
PARSER_TARGET=parser.ml parser.mli

TARGET=staged

$(TARGET): $(TOY) $(OBJS)
	$(CAMLC) $(COMPFLAGS) $^ -o $@

depend: beforedepend
	rm -f .generated-dependencies
	$(CAMLDEP) *.mli *.ml > .generated-dependencies

$(TOY):
	cd $(TOYDIR); make depend
	cd $(TOYDIR); make toy.cma

clean::
	rm -f $(TARGET) $(LEXER_TARGET) $(PARSER_TARGET) *.cmi *.cmx *.cma *.cmo *.o staged
	cd $(TOYDIR) && make clean

# ---------------------------------------------------------------------------------------------------------------------
# Lexer

$(LEXER_TARGET): $(LEXER_SOURCE)
	rm -f $(LEXER_TARGET)
	$(CAMLLEX) $^

beforedepend:: $(LEXER_TARGET)

clean::
	rm -f $(LEXER_TARGET)

# ---------------------------------------------------------------------------------------------------------------------
# Parser

$(PARSER_TARGET): $(PARSER_SOURCE)
	rm -f $(PARSER_TARGET)
	$(CAMLYACC) $^

beforedepend:: $(PARSER_TARGET)

clean::
	rm -f $(PARSER_TARGET)

# ---------------------------------------------------------------------------------------------------------------------
# Suffixes.

.SUFFIXES: .mli .cmi .ml .cmo .cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(OCOMPFLAGS) -c $<

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),depend)
ifneq ($(MAKECMDGOALS),doc)
include .generated-dependencies
endif
endif
endif
