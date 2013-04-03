#*********************************************************************************************************************#
#                                                                                                                     #
#                                                    Wallace                                                          #
#                                                                                                                     #
#                               François Pottier, projet Cristal, INRIA Rocquencourt                                  #
#                                                                                                                     #
#    Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.    #
#                                                                                                                     #
#*********************************************************************************************************************#
# This pseudo-makefile builds Gromit and Wallace before transferring
# control to the actual makefile.

.PHONY: staged

all:
	@(cd ../gromit; make)
	@(cd ../wallace; make)
	@make -f Makefile.local

depend:
	@(cd ../gromit; make depend; make)
	@(cd ../wallace; make depend)
	@make -f Makefile.local depend

toy.cma:
	@make -f Makefile.local toy.cma

clean:
	@make -f Makefile.local clean

realclean:
	@(cd ../gromit; make clean)
	@(cd ../wallace; make clean)
	@make -f Makefile.local clean

doc:
	@make -f Makefile.local doc

