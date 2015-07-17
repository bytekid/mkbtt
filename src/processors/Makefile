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
UTIL=../util
PARSEC=../parsec
REWRITING=../rewriting
AUTOMATA=../automata
LOGIC=../logic
TARGET = processors
FILES= \
 rewriting/label \
 rewriting/prelude \
 rewriting/signature \
 rewriting/monad \
 xmlOutput \
 rewriting/position \
 rewriting/term \
 rewriting/context \
 rewriting/substitution \
 rewriting/rule \
 rewriting/trs \
 rewriting/filtering \
 rewriting/graph \
 rewriting/projection \
 rewriting/diagram \
 rewriting/aclogic \
 rewriting/rewritingx \
 logic/coefficient \
 logic/arcticCoefficient \
 problem \
 \
 modifier/restore \
 \
 nontermination/loop \
 nontermination/contained \
 nontermination/loopSat \
 nontermination/unfolding \
 nontermination/variables \
 \
 predicate/isApplicative \
 predicate/isCollapsing \
 predicate/isConstructor \
 predicate/isDummy \
 predicate/isDuplicating \
 predicate/isErasing \
 predicate/isFlat \
 predicate/isFull \
 predicate/isGround \
 predicate/isInnermost \
 predicate/isLeftGround \
 predicate/isLeftLinear \
 predicate/isLinear \
 predicate/isOverlapping \
 predicate/isOverlay \
 predicate/isOutermost \
 predicate/isRelative \
 predicate/isRightGround \
 predicate/isRightLinear \
 predicate/isShallow \
 predicate/isSrs \
 predicate/isStandard \
 predicate/isStronglyNonOverlapping \
 predicate/isTrs \
 \
 termination/bounds/categorization \
 termination/bounds/automatax \
 termination/bounds/enrichment \
 termination/bounds/bounds \
 termination/csf \
 termination/dg/adg \
 termination/dg/cdg \
 termination/dg/edg \
 termination/dg/tdg \
 termination/dg/odg \
 termination/dg/acdg \
 termination/dg/dg \
 termination/dp \
 termination/subtermCriterion \
 termination/interpretations/interpretation \
 termination/interpretations/matrix \
 termination/interpretations/monomial \
 termination/interpretations/polynomial \
 termination/interpretations/fixedBaseElementary \
 termination/orderings/precedence \
 termination/labeling/semanticLabeling \
 termination/interpretations/polynomialInterpretation \
 termination/interpretations/arcticInterpretation \
 termination/interpretations/matrixInterpretation \
 termination/interpretations/fixedBaseElementaryInterpretation \
 termination/orderings/lpo \
 termination/orderings/weights \
 termination/orderings/kbo \
 termination/orderings/tkbo \
 termination/orderings/acrpo \
 termination/orderings/ackbo \
 termination/sccs \
 termination/sizeChangeTermination \
 termination/trivial \
 \
 transformation/cp \
 transformation/dpify \
 transformation/labeling/rootLabeling \
 transformation/labeling/quasiRootLabeling \
 transformation/linear \
 transformation/reflect \
 transformation/reverse \
 transformation/rt \
 transformation/split \
 transformation/st \
 transformation/typeIntroduction \
 transformation/udpac \
 transformation/uncurry \
 transformation/uncurryx \
 transformation/ur \
 \
 transformation/dup \
 transformation/star \
 \
 confluence/nonconfluence \
 confluence/ruleLabeling \
 confluence/decreasing \
 confluence/shift \

I_FILES = processors

SOURCES = $(addprefix build/, $(FILES))
I_SOURCES=$(addprefix build/, $(I_FILES))
INCLUDE = \
 -I $(UTIL) -I $(PARSEC) -I $(REWRITING) -I $(AUTOMATA) -I $(LOGIC) \
 $(addprefix -I ,$(patsubst src%,build%,$(shell find src -type d))) \

MKTOP = ocamlmktop -g $(INCLUDE)
OCAMLC = ocamlc -g $(INCLUDE)
OCAMLOPT = ocamlopt.opt -g $(INCLUDE)
CLIBS = 
LIBS = nums str unix util parsec rewriting automata minisat minisatp yices logic

LIBS_CMA = $(addsuffix .cma, $(LIBS))
LIBS_CMXA = $(addsuffix .cmxa, $(LIBS))
CMI = $(addsuffix .cmi, $(I_SOURCES))
MLI = $(addsuffix .mli, $(I_SOURCES))
CMO = $(addsuffix .cmo, $(SOURCES))
CMO2 = $(addsuffix .cmo, build/$(TARGET))
CMX = $(addsuffix .cmx, $(SOURCES))
CMX2 = $(addsuffix .cmx, build/$(TARGET))

all: Makefile
	#$(MAKE) bytecode
	$(MAKE) native

build: 
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
         $(PARSEC)/src/parsec.mli \
         $(REWRITING)/src/rewriting.mli \
         $(AUTOMATA)/src/automata.mli \
         $(LOGIC)/src/logic.mli \
         ./src/processors.mli \

help:
	@cat README

native: Makefile build $(CMX) $(CMI)
	$(OCAMLOPT) -pack -o build/$(TARGET)x.cmx $(CMX) 
	$(OCAMLOPT) -c build/$(TARGET).ml
	$(OCAMLOPT) -a $(CLIBS) build/$(TARGET)x.cmx $(CMX2) -o $(TARGET).cmxa
	@cp -d build/$(TARGET).cmi .

test: bytecode
	ocamlc -g -I $(UTIL) -I $(PARSEC) -I $(REWRITING) -I $(AUTOMATA) -I $(LOGIC) -o test $(LIBS_CMA) $(TARGET).cma test.ml
	./test

test_native: native
	ocamlopt.opt -I $(UTIL) -I $(PARSEC) -I $(REWRITING) -I $(AUTOMATA) -I $(LOGIC) -o test_native $(LIBS_CMXA) $(TARGET).cmxa test.ml
	./test_native

top: Makefile build bytecode
	$(MKTOP) -o $(TARGET).top $(LIBS_CMA) $(CMO)

%.cmi: %.mli
	$(OCAMLC) -c $<
        
%.cmo: %.ml
	$(OCAMLC) -c $<
        
%.cmx: %.ml
	$(OCAMLOPT) -for-pack Processorsx -c $<
        
.PHONY: all build bytecode clean dist_clean doc native test test_native top
