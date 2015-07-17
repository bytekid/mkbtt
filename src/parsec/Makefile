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
SHELL=/bin/bash

UTIL=../util
TARGET=parsec
FILES=
I_FILES=parsec

SOURCES=$(addprefix build/, $(FILES))
I_SOURCES=$(addprefix build/, $(I_FILES))
INCLUDE= \
 -I $(UTIL) \
 $(addprefix -I ,$(patsubst src%,build%,$(shell find src -type d))) \

MKTOP=ocamlmktop $(INCLUDE)
OCAMLC=ocamlc -g $(INCLUDE)
OCAMLOPT=ocamlopt.opt $(INCLUDE)
CLIBS=
LIBS=str unix util

LIBS_CMA=$(addsuffix .cma, $(LIBS))
LIBS_CMXA=$(addsuffix .cmxa, $(LIBS))
CMI=$(addsuffix .cmi, $(I_SOURCES))
MLI=$(addsuffix .mli, $(I_SOURCES))
CMO=$(addsuffix .cmo, $(SOURCES))
CMO2=$(addsuffix .cmo, build/$(TARGET))
CMX=$(addsuffix .cmx, $(SOURCES))
CMX2=$(addsuffix .cmx, build/$(TARGET))

all: Makefile
	$(MAKE) bytecode
	$(MAKE) native

build: clear
	@if [[ ! -e build ]]; then mkdir build; fi;
	@cp -ur $(filter-out src/CVS, $(wildcard src/*)) build/

build_clean:
	@rm -f $(filter-out build/$(TARGET).cmi, \
        $(shell ls build/*.ml build/*.cmi build/*.cmo build/*.cmx build/*.o))

bytecode: Makefile build $(CMO) $(CMI)
	$(OCAMLC) -pack -o build/$(TARGET)x.cmo $(CMO)
	$(MAKE) $(CMO2)
	$(OCAMLC) -a $(CLIBS) build/$(TARGET)x.cmo $(CMO2) -o $(TARGET).cma
	@cp -u build/$(TARGET).cmi .
	$(MAKE) clear

clean:
	@rm -rf build/ _build _log
	@rm -rf test.o test.cmx test.cmo test.cmi $(TARGET).top

clear:
	@rm -f build/*.cmi

dist_clean: clean
	@rm -rf doc/
	@rm -f test test_native 
	@rm -f *.cmi *.cma *.a *.cmxa

doc: build
	@if [[ ! -e doc ]]; then mkdir doc; fi;
	@ocamldoc -html -short-functors -d doc $(INCLUDE) \
         $(UTIL)/src/util.mli \
         ./src/parsec.mli \

help:
	@cat README

native: Makefile build $(CMX) $(CMI)
	$(OCAMLOPT) -pack -o build/$(TARGET)x.cmx $(CMX) 
	$(OCAMLOPT) -c build/$(TARGET).ml
	$(OCAMLOPT) -a $(CLIBS) build/$(TARGET)x.cmx $(CMX2) -o $(TARGET).cmxa
	@cp -d build/$(TARGET).cmi .
	$(MAKE) clear

test: bytecode
	ocamlc -I $(UTIL) -o test nums.cma $(LIBS_CMA) $(TARGET).cma test.ml
	./test test.xml

test_native: native
	ocamlopt.opt -I $(UTIL) -o test_native $(LIBS_CMXA) $(TARGET).cmxa test.ml
	./test_native

top: Makefile build bytecode
	$(MKTOP) -o $(TARGET).top $(LIBS_CMA) $(TARGET).cma

%.cmi: %.mli
	$(OCAMLC) -c $<
        
%.cmo: %.ml
	$(OCAMLC) -c $<
        
%.cmx: %.ml
	$(OCAMLOPT) -for-pack Parsecx -c $<
        
.PHONY: all build bytecode clean dist_clean doc native test test_native top
