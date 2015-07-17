SHELL=/bin/bash
MKBTT=mkbtt
VERSION=`date +"%Y-%m-%d"`
ARCHIVE   = $(MKBTT)-$(VERSION)
UTIL=util
TRS =rewriting
PARSEC=parsec
PROCS=processors
AUTO=automata
TTT =ttt2
LOGIC=logic
MAC=mascott
SRC=src
EXEC=mkbtt
DEP=$(shell pwd)/.depend
ONLINE=mkbtt-$(VERSION).tar.gz
OPATH=colo1-c703.uibk.ac.at:/home/www/colo1-c703/software/mkbtt

COMPONENTS=\
 ./cpc \
 ./termination \
 ./slothrop \
 ./termindexing \
 ./isomorphisms
INCLUDE=-I ../$(UTIL) -I ../$(PARSEC) -I ../$(TRS) -I ../$(LOGIC) -I ../$(AUTO) -I ../$(PROCS) -I ../$(TTT)  -I ../$(MAC)
OCAMLFLAGS=$(INCLUDE) -cc g++
OCAMLC=ocamlc $(OCAMLFLAGS)
OCAMLOPT=ocamlopt $(OCAMLFLAGS)
OCAMLDEP=ocamldep
LIBS = nums str unix util parsec rewriting automata minisat minisatp yices logic processors ttt2 mascott

LIBS_CMA = $(addsuffix .cma, $(LIBS))
LIBS_CMXA = $(addsuffix .cmxa, $(LIBS))
MLI_FILES= u.mli termx.mli equation.mli rulex.mli trsx.mli \
	selectionStrategy.mli completion.mli \
	termIndex.mli pathString.mli trie.mli naiveIndex.mli \
	codeTree.mli pString.mli discriminationTree.mli indexWrapper.mli \
	types.mli world.mli tPTP.mli tPTPInput.mli \
        statistics.mli externalTermination.mli interfaceTTT.mli \
	completionProcessx.mli renamingClass.mli \
	nodex.mli indexedNode.mli nodeTermIndexx.mli \
	nodeSet.mli waldmeisterGoalNodex.mli indexedWNode.mli \
	goalState.mli trsRenaming.mli termPermutation.mli \
	nodeState.mli processIsomorphism.mli cPCCache.mli \
	prime.mli blocked.mli connected.mli all.mli cPC.mli termination.mli \
	mKBtt.mli oMKBtt.mli inferences.mli choiceState.mli \
	selectionStrategyAux.mli chooseHaruhiko.mli \
	check.mli history.mli conversion.mli equationalProofTree.mli nodeGoal.mli \
	waldmeisterGoal.mli existentialGoal.mli goalWrapper.mli control.mli \
	trace.mli certOutput.mli
ML_FILES= u.ml termx.ml equation.ml rulex.ml trsx.ml \
	selectionStrategy.ml selectionParser.mli selectionParser.ml \
	completion.ml \
	termIndex.ml pathString.ml trie.ml naiveIndex.ml \
	codeTree.ml pString.ml discriminationTree.ml indexWrapper.ml \
	types.ml world.ml \
	selectionLexer.ml \
	tPTP.ml tPTPInput.ml \
	statistics.ml slPrefs.ml \
        externalTermination.ml slDebug.ml slHeap.ml slTerms.ml slTptp.ml \
        slPrint.ml slCp.ml slRewriting.ml slLearning.ml slStatus.ml \
        interfaceTTT.ml slChecker.ml \
        slOrient.ml slConjectures.ml slHuet.ml\
        slMain.ml\
	completionProcessx.ml renamingClass.ml \
	nodex.ml indexedNode.ml nodeTermIndexx.ml nodeSet.ml \
	waldmeisterGoalNodex.ml indexedWNode.ml goalState.ml \
	trsRenaming.ml termPermutation.ml nodeState.ml processIsomorphism.ml \
	cPCCache.ml blocked.ml prime.ml connected.ml all.ml cPC.ml \
	termination.ml \
	mKBtt.ml oMKBtt.ml inferences.ml \
	choiceState.ml selectionStrategyAux.ml chooseHaruhiko.ml \
	check.ml history.ml conversion.ml equationalProofTree.ml\
	nodeGoal.ml waldmeisterGoal.ml existentialGoal.ml goalWrapper.ml \
	control.ml trace.ml certOutput.ml main.ml 
ML_NAMES=$(basename $(ML_FILES))
CMO_FILES=$(addsuffix .cmo, $(ML_NAMES))
CMX_FILES=$(addsuffix .cmx, $(ML_NAMES))

all: libs bytecode native

bytecode:
	pushd $(SRC)/$(MKBTT); ./bin/parsergen.sh; popd;
	$(MAKE) flat
	pushd $(SRC)/$(MKBTT); $(OCAMLC) -o $(EXEC) $(LIBS_CMA) \
         $(MLI_FILES) $(ML_FILES) 
	$(MAKE) unflat
	cp $(SRC)/$(MKBTT)/$(EXEC) .

native:
	pushd $(SRC)/$(MKBTT); ./bin/parsergen.sh; popd;
	$(MAKE) flat
	pushd $(SRC)/$(MKBTT); $(OCAMLOPT) -o $(EXEC) $(LIBS_CMXA) $(ML_FILES) 
	$(MAKE) unflat
	cp $(SRC)/$(MKBTT)/$(EXEC) .

clean:
	pushd $(SRC)/$(MKBTT); for f in *.o *.cm[xoi]; do \
	if [[ -e $$f ]]; then rm $$f; fi; \
	done

dist_clean:
	pushd $(SRC)/$(UTIL); make dist_clean; popd;
	pushd $(SRC)/$(PARSEC); make dist_clean; popd;
	pushd $(SRC)/$(TRS); make dist_clean; popd;
	pushd $(SRC)/$(LOGIC); make dist_clean; popd;
	pushd $(SRC)/$(AUTO); make dist_clean; popd;
	pushd $(SRC)/$(PROCS); make dist_clean; popd;
	pushd $(SRC)/$(TTT); make dist_clean; popd;
	pushd $(SRC)/$(MAC); make dist_clean; popd;
	$(MAKE) clean;
	find . -iname CVS -type d | xargs rm -rf

dist:
	$(MAKE) dist_clean
	tar --exclude=CVS -czf $(ARCHIVE).tar.gz Makefile README src
	
flat:
	pushd $(SRC)/$(MKBTT); for d in $(COMPONENTS); do \
         for f in `find $$d -name "*.mli"`; do cp -u $$f ./; done; \
         for f in `find $$d -name "*.ml"`; do cp -u $$f ./; done; \
         for f in `find $$d -name "*.mll"`; do cp -u $$f ./; done; \
         for f in `find $$d -name "*.mly"`; do cp -u $$f ./; done; \
        done; 

libs:
	pushd $(SRC)/$(UTIL); make bytecode; make native; popd;
	pushd $(SRC)/$(PARSEC); make bytecode; make native; popd;
	pushd $(SRC)/$(TRS); make bytecode; make native; popd;
	pushd $(SRC)/$(LOGIC); make bytecode; make native; popd;
	pushd $(SRC)/$(AUTO); make bytecode; make native; popd;
	pushd $(SRC)/$(PROCS); make bytecode; make native; popd;
	pushd $(SRC)/$(TTT); make bytecode; make native; popd;
	pushd $(SRC)/$(MAC); make bytecode; make native; popd;

unflat:
	pushd $(SRC)/$(MKBTT); for d in $(COMPONENTS); do \
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

online:
	$(MAKE) all
	$(MAKE) dist_clean
	mkdir mkbtt$(VERSION)
	cp -r src README Makefile input callaprove callmuterm mkbtt$(VERSION)
	tar cfz $(ONLINE) mkbtt$(VERSION)
	scp $(ONLINE) swinkler@$(OPATH)/download/
	rm -rf $(ONLINE)
	rm -rf mkbtt$(VERSION)
