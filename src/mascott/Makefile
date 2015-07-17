SHELL=/bin/bash
MAC=mascott
UTIL=util
TRS =rewriting
PARSEC=parsec
PROCS=processors
AUTO=automata
TTT =ttt2
LOGIC=logic
SRC=src
EXEC=mascott
DEP=$(shell pwd)/.depend
LIBPRE=..

COMPONENTS=\
 ./termination
INCLUDE=-I $(LIBPRE)/$(UTIL) -I $(LIBPRE)/$(PARSEC) -I $(LIBPRE)/$(TRS) \
	-I $(LIBPRE)/$(LOGIC) -I $(LIBPRE)/$(AUTO) -I $(LIBPRE)/$(PROCS) \
	-I $(LIBPRE)/$(TTT) -I $(SRC)
OCAMLFLAGS=$(INCLUDE) -cc g++
OCAMLC=ocamlc $(OCAMLFLAGS)
INCLUDEC=-I . $(INCLUDE)
OCAMLCC= ocamlc -g $(INCLUDEC)
OCAMLOPT=ocamlopt $(OCAMLFLAGS)
OCAMLDEP=ocamldep
LIBS = nums str unix util parsec rewriting automata minisat minisatp yices logic processors ttt2

LIBS_CMA = $(addsuffix .cma, $(LIBS))
LIBS_CMXA = $(addsuffix .cmxa, $(LIBS))
MLI_FILES= u.mli aCPosition.mli termx.mli equation.mli dioMio.mli \
	dioComplete.mli theory.mli \
	aCLogicNoFakeAux.mli aCLogicNoFake.mli \
	aCULogic.mli \
	selectionStrategy.mli completion.mli types.mli \
	bipartiteMatchingAutomata.mli aCPString.mli aCDiscTree.mli \
	rulex.mli \
	aCLogic.mli aCRewrite.mli \
	trsx.mli \
	world.mli selectionParser.mli \
	tPTP.mli tPTPInput.mli \
	statistics.mli \
	completionProcessx.mli nodex.mli indexedNode.mli \
	externalTermination.mli interfaceTTT.mli termination.mli \
	indexedSet.mli nodeSetAux.mli \
	nodeTermIndex.mli nodeState.mli \
	sReduce.mli prime.mli mixed.mli cPC.mli \
	mKBtt.mli choiceState.mli trsRenaming.mli \
	selectionStrategyAux.mli chooseHaruhiko.mli history.mli\
	control.mli conversion.mli equationalProofTree.mli certOutput.mli \
	main.mli
ML_FILES= u.ml aCPosition.ml termx.ml equation.ml \
	dioComplete.ml theory.ml \
	dioMio.ml aCLogicNoFakeAux.ml aCLogicNoFake.ml\
	aCULogic.ml \
	selectionStrategy.ml completion.ml types.ml \
	binMatrixSolver3.ml binMatrixSolver2.ml binMatrixSolver.ml \
	bipartiteMatchingAutomata.ml aCPString.ml aCDiscTree.ml \
	rulex.ml aCLogic.ml aCRewrite.ml \
	trsx.ml \
	selectionParser.ml \
	world.ml \
	selectionLexer.ml \
	tPTP.ml tPTPInput.ml \
	statistics.ml \
	completionProcessx.ml nodex.ml indexedNode.ml \
	externalTermination.ml interfaceTTT.ml termination.ml \
	indexedSet.ml nodeSetAux.ml \
	nodeTermIndex.ml nodeState.ml \
	sReduce.ml prime.ml mixed.ml cPC.ml \
	mKBtt.ml choiceState.ml trsRenaming.ml theories.ml\
	selectionStrategyAux.ml chooseHaruhiko.ml \
	history.ml control.ml \
	conversion.ml equationalProofTree.ml certOutput.ml \
	main.ml 
ML_NAMES=$(basename $(ML_FILES))
MLI_NAMES=$(basename $(MLI_FILES))
CMO=$(addsuffix .cmo, $(ML_NAMES))
CMI=$(addsuffix .cmi, $(MLI_NAMES))
CMX=$(addsuffix .cmx, $(ML_NAMES))

all: Makefile
	$(MAKE) bytecode
	$(MAKE) native

bytecode: $(addprefix $(SRC)/, $(CMI)) $(addprefix $(SRC)/, $(CMO)) 
	pushd $(SRC); ./bin/parsergen.sh; popd;
	$(MAKE) flat
	$(OCAMLC) -pack -o $(MAC)x.cmo $(addprefix $(SRC)/, $(CMO))
	$(MAKE) $(SRC)/$(MAC).cmi; $(MAKE) $(SRC)/$(MAC).cmo
	$(OCAMLC) -a $(CLIBS) $(MAC)x.cmo $(SRC)/$(MAC).cmo -o $(MAC).cma
	@cp -u src/$(MAC).cm* .

native: $(addprefix $(SRC)/, $(CMI)) $(addprefix $(SRC)/, $(CMX))
	pushd $(SRC); ./bin/parsergen.sh; popd;
	$(MAKE) flat
	$(MAKE) $(SRC)/$(MAC).cmi; $(MAKE) $(SRC)/$(MAC).cmx
	$(OCAMLOPT) -pack -o $(MAC)x.cmx $(addprefix $(SRC)/, $(CMX)) 
	$(OCAMLOPT) -a $(CLIBS) $(MAC)x.cmx $(SRC)/$(MAC).cmx -o $(MAC).cmxa

dist_clean:
	pushd $(SRC); for f in *.o *.cm[xoi]; do \
         if [[ -e $$f ]]; then rm $$f; fi; \
        done;
	@rm -f *.a *.cmi *.cmxa *.cma *.o *.cmo *.cmx

flat:
	pushd $(SRC); for d in $(COMPONENTS); do \
         for f in `find $$d -name "*.mli"`; do cp -u $$f ./; done; \
         for f in `find $$d -name "*.ml"`; do cp -u $$f ./; done; \
         for f in `find $$d -name "*.mll"`; do cp -u $$f ./; done; \
         for f in `find $$d -name "*.mly"`; do cp -u $$f ./; done; \
        done; 


unflat:
	pushd $(SRC)/$(MAC); for d in $(COMPONENTS); do \
         for f in `find $$d -name "*.ml"`; do \
          g=`basename $$f`; \
          if [[ -e $$g ]]; then rm $$g; fi; \
         done; \
         for f in `find $$d -name "*.mli"`; do \
          g=`basename $$f`; \
          if [[ -e $$g ]]; then rm $$g; fi; \
         done; \
         for f in `find $$d -name "*.mll"`; do \
          g=`basename $$f`; \
          if [[ -e $$g ]]; then rm $$g; fi; \
         done; \
         for f in `find $$d -name "*.mly"`; do \
          g=`basename $$f`; \
          if [[ -e $$g ]]; then rm $$g; fi; \
         done; \
        done; \

%.cmi: %.mli
	$(OCAMLCC) -c $<

%.cmo: %.ml
	$(OCAMLCC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) -for-pack Mascottx -c $<
