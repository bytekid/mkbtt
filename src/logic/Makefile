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
BASE=$(shell pwd)
YICES32 = $(BASE)/src/yices/32bit
YICES64 = $(BASE)/src/yices/64bit
UTIL = ../util
TARGET = logic
MINISAT = build/minisat
MINISATP = build/minisat+
YICES = build/yices
FILES= \
 integer \
 rational \
 real \
 minf \
 number \
 formula \
 operators \
 a p  ass  pss assignment \
 binType \
 state monad \
 binNat \
 binInt \
 binRat \
 binReal \
 binMinf \
 binNumber \
 miniSat \
 miniSatP \
 miniSmt \
 yices \
 solver \

I_FILES = logic
SOURCES = $(addprefix build/, $(FILES))
I_SOURCES = $(addprefix build/, $(I_FILES))
INCLUDE = \
 $(addprefix -I ,$(patsubst src%,build%,$(shell find src -type d))) \
 -I $(UTIL) \

MKTOP = ocamlmktop $(INCLUDE)
OCAMLC = ocamlc -g $(INCLUDE)
OCAMLOPT = ocamlopt.opt $(INCLUDE)
CLIBS = 
LIBS = nums unix str util minisat minisatp yices

LIBS_CMA = $(addsuffix .cma, $(LIBS))
LIBS_CMXA = $(addsuffix .cmxa, $(LIBS))
CMI = $(addsuffix .cmi, $(I_SOURCES))
MLI = $(addsuffix .mli, $(I_SOURCES))
CMO = $(addsuffix .cmo, $(SOURCES))
CMO2 = $(addsuffix .cmo, build/$(TARGET))
CMX = $(addsuffix .cmx, $(SOURCES))
CMX2 = $(addsuffix .cmx, build/$(TARGET))

OCAMLC = ocamlc -g $(INCLUDE) $(CLIBS)
OCAMLOPT = ocamlopt.opt $(INCLUDE)

all: Makefile
	#$(MAKE) bytecode
	$(MAKE) native

build: 
	@if [[ ! -e build ]]; then mkdir build; fi;
	@cp -ur $(filter-out src/CVS, $(wildcard src/*)) build/
	pushd $(MINISAT); make; popd
	@cp -ur $(MINISAT)/libminisat.a $(MINISAT)/libinterface_minisat.a \
        $(MINISAT)/minisat.cma $(MINISAT)/minisat.cmxa $(MINISAT)/minisat.a .
	pushd $(MINISATP); make; popd
	@cp -ur $(MINISATP)/libminisatp.a \
        $(MINISATP)/minisatp.cma $(MINISATP)/minisatp.cmxa $(MINISATP)/minisatp.a .
	pushd $(YICES); make; popd
	@cp -ur $(YICES)/libinterface_yices.a \
        $(YICES)/yices.cma $(YICES)/yices.cmxa $(YICES)/yices.a .


build_clean:
	@rm -f $(filter-out build/$(TARGET).cmi, \
         $(shell ls build/*.ml build/*.cmi build/*.cmo build/*.cmx build/*.o))

bytecode: Makefile build $(CMO) $(CMI) $(CMO2)
	$(OCAMLC) -custom \
         -ccopt -L$(BASE) -ccopt -L$(YICES32)/lib -ccopt -L$(YICES64)/lib \
         -a -o $(TARGET).cma $(CMO) $(CMO2)
	@cp -u build/$(TARGET).cmi .

clean:
	@rm -rf _build _log build/ 
	@rm -rf test.o test.cmx test.cmo test.cmi $(TARGET).top

clear:
	@rm -f build/*.cmi

dist_clean: clean
	@rm -rf doc/
	@rm -f test test_native 
	@rm -f *.a *.cmi *.cmxa *.cma

doc: build
	@if [[ ! -e doc ]]; then mkdir doc; fi;
	@ocamldoc -html -short-functors -d doc $(INCLUDE) \
        ./src/logic.mli

help:
	@cat README

native: Makefile build $(CMX) $(CMI) $(CMX2)
	$(OCAMLOPT) \
         -ccopt -L$(BASE) -ccopt -L$(YICES32)/lib -ccopt -L$(YICES64)/lib \
         -a -o $(TARGET).cmxa $(CMX) $(CMX2)
	@cp -u build/$(TARGET).cmi .

test: #dist_clean bytecode
	ocamlc -g -cc g++ -I ../util -o test $(LIBS_CMA) $(TARGET).cma \
	test.ml
	./test

test_native: dist_clean native
	ocamlopt.opt -cc g++ -I ../util -o test_native $(LIBS_CMXA) $(TARGET).cmxa \
         test.ml
	./test_native

top: Makefile build bytecode
	$(MKTOP) -cc g++ -o $(TARGET).top $(LIBS_CMA) $(TARGET).cma

%.cmi: %.mli
	$(OCAMLC) -c $<
        
%.cmo: %.ml
	$(OCAMLC) -c $<
        
%.cmx: %.ml
	$(OCAMLOPT) -c $<

.PHONY: all build bytecode clean dist_clean doc native test test_native top
