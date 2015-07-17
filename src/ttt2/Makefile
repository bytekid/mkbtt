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
STATIC=-ccopt -static

UTIL=../util
PARSEC=../parsec
REWRITING=../rewriting
AUTOMATA=../automata
LOGIC=../logic
PROCESSORS=../processors
TARGET=ttt2
FILES= \
 strategy/syntax \
 strategy/parser \
 strategy/lexer \
 strategy/status \
 strategy/processor \
 strategy/proof \
 strategy/state \
 strategy/monad \
 strategy/nontermination \
 strategy/termination \
 strategy/transformation \
 strategy/modifier \
 strategy/predicate \
 strategy/main \
 input/srsSyntax \
 input/srsParser \
 input/srsLexer \
 input/trsSyntax \
 input/trsParser \
 input/trsLexer \
 input/confSyntax \
 input/confParser \
 input/confLexer \
 input/wstParser \
 input/xml \
 answer \
 kernel \

I_FILES=ttt2
L_FILES=input/confLexer input/srsLexer input/trsLexer strategy/lexer
P_FILES=input/confParser input/srsParser input/trsParser strategy/parser

SOURCES=$(addprefix build/, $(FILES))
I_SOURCES=$(addprefix build/, $(I_FILES))
L_SOURCES=$(addprefix build/, $(L_FILES))
P_SOURCES=$(addprefix build/, $(P_FILES))
INCLUDE=\
 -I $(UTIL) -I $(PARSEC) -I $(REWRITING) -I $(AUTOMATA) -I $(LOGIC) -I $(PROCESSORS) \
 $(addprefix -I ,$(patsubst src%,build%,$(shell find src -type d))) \

MKTOP = ocamlmktop
OCAMLC = ocamlc -g $(INCLUDE)
OCAMLOPT = ocamlopt.opt -g $(INCLUDE)
CLIBS = 
LIBS = nums str unix util parsec rewriting automata minisat minisatp yices logic processors

LIBS_CMA = $(addsuffix .cma, $(LIBS))
LIBS_CMXA = $(addsuffix .cmxa, $(LIBS))
CMI = $(addsuffix .cmi, $(I_SOURCES))
MLI = $(addsuffix .mli, $(I_SOURCES))
CMO = $(addsuffix .cmo, $(SOURCES))
CMO2 = $(addsuffix .cmo, build/$(TARGET))
CMX = $(addsuffix .cmx, $(SOURCES))
CMX2 = $(addsuffix .cmx, build/$(TARGET))

all:
	$(MAKE) exec_native

build:
	@if [[ ! -e build ]]; then mkdir build; fi;
	@cp -ur $(filter-out src/CVS, $(wildcard src/*)) build/

build_clean:
	@rm -f $(filter-out build/$(TARGET).cmi, \
        $(shell ls build/*.ml build/*.cmi build/*.cmo build/*.cmx build/*.o))

bytecode: build parse $(CMO) $(CMI)
	$(OCAMLC) -pack -o build/$(TARGET)x.cmo $(CMO)
	$(MAKE) $(CMO2)
	$(OCAMLC) -a $(CLIBS) build/$(TARGET)x.cmo $(CMO2) -o $(TARGET).cma
	@cp -u build/$(TARGET).cmi .

clean:
	@rm -rf build/ _build _log
	@rm -rf *.o *.cmx *.cmo *.cmi $(TARGET).top

clear:
	@rm -f build/*.cmi

dist_clean: clean
	@rm -rf doc/
	@rm -f test test_native 
	@rm -f *.cma *.a *.cmxa

doc: build
	@if [[ ! -e doc ]]; then mkdir doc; fi;
	@ocamldoc -html -short-functors -d doc $(INCLUDE) \
         $(UTIL)/src/util.mli \
         $(PARSEC)/src/parsec.mli \
         $(REWRITING)/src/rewriting.mli \
         $(AUTOMATA)/src/automata.mli \
         $(LOGIC)/src/logic.mli \
         $(PROCESSORS)/src/processors.mli \
	./src/ttt2.mli \

exec:
	ocamlopt.opt $(STATIC) -cc g++ -I $(UTIL) -I $(PARSEC) -I $(REWRITING) -I $(AUTOMATA) -I $(LOGIC) -I $(PROCESSORS) -o $(TARGET) $(LIBS_CMXA) $(TARGET).cmxa bootstrap.ml

exec_bytecode:
	$(MAKE) bytecode
	ocamlc -g -cc g++ -I $(UTIL) -I $(PARSEC) -I $(REWRITING) -I $(AUTOMATA) -I $(LOGIC) -I $(PROCESSORS) -o $(TARGET) $(LIBS_CMA) $(TARGET).cma main.ml

exec_native:
	$(MAKE) native
	$(MAKE) exec

help:
	@cat README

native: build parse $(CMX) $(CMI)
	$(OCAMLOPT) -pack -o build/$(TARGET)x.cmx $(CMX) 
	$(OCAMLOPT) -c build/$(TARGET).ml
	$(OCAMLOPT) -a $(CLIBS) build/$(TARGET)x.cmx $(CMX2) -o $(TARGET).cmxa
	@cp -d build/$(TARGET).cmi .

parse:
	for i in $(addsuffix .mly, $(P_SOURCES)); do ocamlyacc $$i; done
	rm -f $(addsuffix .mli, $(P_SOURCES))
	for i in $(addsuffix .mll, $(L_SOURCES)); do ocamllex $$i; done

test: bytecode
	ocamlc -I $(UTIL) -I $(PARSEC) -I $(REWRITING) -I $(AUTOMATA) -I $(LOGIC) -I $(PROCESSORS) -o test $(LIBS_CMA) $(TARGET).cma test.ml
	./test

test_native: native
	ocamlopt.opt -I $(UTIL) -I $(PARSEC) -I $(REWRITING) -I $(AUTOMATA) -I $(LOGIC) -I $(PROCESSORS) -o test_native $(LIBS_CMXA) $(TARGET).cmxa test.ml
	./test_native

top: build bytecode
	$(MKTOP) -o $(TARGET).top $(LIBS_CMA) $(CMO)

%.cmi: %.mli
	$(OCAMLC) -c $<
        
%.cmo: %.ml
	$(OCAMLC) -c $<
        
%.cmx: %.ml
	$(OCAMLOPT) -for-pack Ttt2x -c $<
        
.PHONY: all build bytecode clean dist_clean doc native test test_native top
