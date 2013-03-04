#*********************************************************************************************************************#
#                                                                                                                     #
#                                                    Wallace                                                          #
#                                                                                                                     #
#                               François Pottier, projet Cristal, INRIA Rocquencourt                                  #
#                                                                                                                     #
#    Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.    #
#                                                                                                                     #
#*********************************************************************************************************************#
# $Header: /home/pauillac/formel1/fpottier/cvs/gromit/Makefile,v 1.13 2000/02/11 16:15:35 fpottier Exp $

# ---------------------------------------------------------------------------------------------------------------------
# Choices for the executables.

CAMLC=ocamlc
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
CAMLDEP=ocamldep
CAMLOPT=ocamlopt
CAMLMKTOP=ocamlmktop
CAMLWEB=ocamlweb

# ---------------------------------------------------------------------------------------------------------------------
# Compile options.

COMPFLAGS=-g

# ---------------------------------------------------------------------------------------------------------------------
# Building Gromit.

gromit: syntax.cmo parser.cmo lexer.cmo ordering.cmo signature.cmo generator.cmo gromit.cmo
	$(CAMLC) $(COMPFLAGS) $^ -o $@

parser.ml parser.mli: parser.mly
	@rm -f parser.ml parser.mli
	$(CAMLYACC) $^

lexer.ml: lexer.mll
	@rm -f $@
	$(CAMLLEX) $^

clean::
	rm -f gromit .generated-dependencies
	rm -f parser.ml parser.mli lexer.ml
	rm -f *.cmi *.cmx *.cmo *.ppo *.o *~ #*#

depend: parser.ml parser.mli lexer.ml
	rm -f .generated-dependencies
	$(CAMLDEP) *.mli *.ml > .generated-dependencies

# ---------------------------------------------------------------------------------------------------------------------
# Building the documentation.

SOURCES=syntax.ml signature.ml ordering.mli ordering.ml generator.mli generator.ml lexer.mli gromit.ml

doc: gromit.dvi

gromit.dvi: $(SOURCES) gromit.tex
	$(CAMLWEB) -o gromit-auto-generated.tex --quiet --no-preamble --latex-sects $(SOURCES) 
	latex gromit; latex gromit

clean::
	rm -f gromit-auto-generated.tex
	rm -f *.dvi *.log *.aux *.toc
	rm -rf auto

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
