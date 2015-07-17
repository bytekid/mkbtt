# Copyright 2008 Martin Korp, Christian Sternagel, Harald Zankl
# GNU Lesser General Public License
#
# This file is part of TTT2.
# 
# TTT2 is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.
# 
# TTT2 is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
# License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with TTT2. If not, see <http://www.gnu.org/licenses/>.

SHELL = bash
CAML = $(shell ocamlc -where)
OCAMLC = ocamlc -cc g++
OCAMLOPT = ocamlopt -cc g++
#MSAT = minisat/core
MSAT = minisat/simp
MINCLUDE = -I minisat/core -I minisat/mtl

COBJS = \
 interface_stubs.o \
 interface_aux.o \
 interface.o \

FILES = \
 interface

CMI = $(addsuffix .cmi, $(FILES))
CMO = $(addsuffix .cmo, $(FILES))
CMX = $(addsuffix .cmx, $(FILES))
CLIBS = interface_minisat minisat camlidl
CLIBFLAGS = $(addprefix -cclib -l, $(CLIBS))
CFLAGS = -fPIC -DPIC -Wall -I $(CAML) $(MINCLUDE)
CXX = g++ $(CFLAGS) 

all:
	pushd $(MSAT); make lib; popd
	camlidl -header interface.idl
	$(MAKE) interface_stubs.o interface_aux.o interface.x $(CMI) $(CMO)
	ar rcs libinterface_minisat.a $(COBJS)
	$(OCAMLC) -custom -a -o minisat.cma $(CLIBFLAGS) $(CMO)
	$(MAKE) $(CMX)
	$(OCAMLOPT) -a -o minisat.cmxa $(CLIBFLAGS) $(CMX)
	@cp -u $(MSAT)/libminisat.a .

test: all
	ocamlc -cc g++ -ccopt -L. -ccopt -L$(MSAT) minisat.cma \
         -o test test.ml
	./test

test_native: all
	ocamlopt -cc g++ -ccopt -L. -ccopt -L$(MSAT) minisat.cmxa \
         -o test_native test.ml
	./test_native

clean:
	rm -f *.o *.h *.cmi *.cmo 
	rm -f interface{.ml,.mli,_stubs.c} libinterface_minisat.a
	rm -f test

dist_clean: clean
	pushd $(MSAT); make clean; popd
	rm -f *.cma *.cmx *.cmxa *.a test_native


# build rules
%.o: %.c
	ocamlc -cc cc -ccopt "-fPIC -DPIC -Wall" -c $<

%.x: %.C
	g++ $(CFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<
        
%.cmo: %.ml
	$(OCAMLC) -c $<
        
%.cmx: %.ml
	$(OCAMLOPT) -c $<

.PHONY: all test
