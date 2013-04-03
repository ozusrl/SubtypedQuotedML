#*********************************************************************************************************************#
#                                                                                                                     #
#                                                    Wallace                                                          #
#                                                                                                                     #
#                               François Pottier, projet Cristal, INRIA Rocquencourt                                  #
#                                                                                                                     #
#    Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.    #
#                                                                                                                     #
#*********************************************************************************************************************#
# $Header: /home/pauillac/formel1/fpottier/cvs/wallace/Makefile,v 1.25 2000/02/11 16:15:46 fpottier Exp $

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

COMPFLAGS=-g -unsafe

# ---------------------------------------------------------------------------------------------------------------------
# What is to be built, by default.

TARGET=wallace.cma

# ---------------------------------------------------------------------------------------------------------------------
# Building.

OBJS=standard.cmo ground.cmo leafSet.cmo condSet.cmo leafSetMap.cmo rowMap.cmo label.cmo paigeTarjan.cmo core.cmo gSet.cmo listSet.cmo baltree.cmo simpleSetMap.cmo trieSetMap.cmo gMap.cmo fixSet.cmo fixMap.cmo unorderedListMap.cmo setBasedMap.cmo patricia.cmo print.cmo textPrinter.cmo teXPrinter.cmo

$(TARGET): $(OBJS)
	$(CAMLC) -a $(COMPFLAGS) $^ -o $@

clean::
	rm -f $(TARGET) .generated-dependencies
	rm -f *.cmi *.cmx *.cmo *.ppo *.o *~ #*#

depend:
	rm -f .generated-dependencies
	$(CAMLDEP) *.mli *.ml > .generated-dependencies

# ---------------------------------------------------------------------------------------------------------------------
# Building the documentation.

SOURCES=standard.mli standard.ml ground.ml leafSet.ml condSet.ml leafSetMap.ml rowMap.ml label.ml paigeTarjan.mli paigeTarjan.ml core.mli core.ml gSet.ml listSet.mli listSet.ml baltree.mli baltree.ml simpleSetMap.mli simpleSetMap.ml trieSetMap.mli trieSetMap.ml gMap.ml fixSet.mli fixSet.ml fixMap.mli fixMap.ml unorderedListMap.mli unorderedListMap.ml setBasedMap.mli setBasedMap.ml patricia.mli patricia.ml print.mli print.ml textPrinter.ml teXPrinter.ml

doc: map.ps wallace.dvi

map.ps: depend
	rm -f map.ps; cat .generated-dependencies | ocamldot | dot -Tps > map.ps

wallace.dvi: $(SOURCES) wallace.tex
	$(CAMLWEB) -o wallace-auto-generated.tex --quiet --no-preamble --latex-sects $(SOURCES) 
	latex wallace; latex wallace; latex wallace

clean::
	rm -f wallace-auto-generated.tex map.ps
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

