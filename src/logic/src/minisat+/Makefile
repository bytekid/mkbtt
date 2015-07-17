#/**************************************************************************************************
#
#accessMinisatP.c - modified Main.C from (C) Niklas Een, Niklas Sörensson, 2004
#
#**************************************************************************************************/

CAML=$(shell ocamlc -where)
# Sources, Headers ans Objects for Minisat+
CSRCS = $(wildcard minisatp/*.C)
CHDRS = $(wildcard minisatp/*.h) accessMinisatP.h
COBJS = $(addsuffix .o, $(basename $(CSRCS)))\
 minisatp/ADTs/Global.o\
 minisatp/ADTs/FEnv.o\
 minisatp/ADTs/File.o\
 accessMinisatP_stubs.o\
 accessMinisatP.o

# Compilers and directory for caml headers
CXX = g++
CC = cc

# flags to compile minisat+
CFLAGS = -Wall -ffloat-store 
CFLAGS += \
 -Iminisatp \
 -Iminisatp/ADTs \
 -include Global.h \
 -include accessMinisatP.h \
 -D_FILE_OFFSET_BITS=64

COPTIMIZE = -O3 #-fomit-frame-pointer # -falign-loops=4 -falign-functions=16 -foptimize-sibling-calls -finline-functions -fcse-follow-jumps -fcse-skip-blocks -frerun-cse-after-loop -frerun-loop-opt -fgcse

CFLAGS += $(COPTIMIZE) -D NDEBUG -D NO_GMP #usual rx minisat+
OBJS =  minisatPlus.cmo
OBJX =  minisatPlus.cmx

.PHONY : all clean depend

FLAGS= -cc g++ -cclib -lcamlidl -cclib -lminisatp
INCLUDE = -I ../../../util
OCAMLC=ocamlc $(FLAGS) $(INCLUDE) 
OCAMLOPT=ocamlopt $(FLAGS) $(INCLUDE)

TARGET=minisatp

libminisat.a : $(COBJS) $(OBJS) $(OBJX)
	ar rc libminisatp.a $(COBJS)
	ranlib libminisatp.a
	$(OCAMLC) -a -o $(TARGET).cma -custom $(OBJS) -cclib -lminisatp
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OBJX) -cclib -lminisatp

build:
	@echo Building $(EXEC) "("$(WAY)")"

clean:
	@rm -f $(COBJS) depend.mak libminisatp.a
	@rm -f *.mli *.cmi *.cmo *.cmx
	@rm -f accessMinisat{_stubs.c,.h}


## Build rules
%.cmo : %.ml 
	$(OCAMLC) -c $<

%.cmx : %.ml 
	$(OCAMLOPT) -c $<

accessMinisatP_stubs.o: accessMinisatP_stubs.c
	$(CC) -c -Wall -I $(CAML) -I. accessMinisatP_stubs.c

%.o : %.C
	@echo Compiling: $<
	@$(CXX) $(CFLAGS) -I. -c -o $@ $<


## Make dependencies
depend:	depend.mak

depend.mak:	 $(CHDRS) $(CSRCS)
	@echo Making dependencies...
	@$(CXX) -MM $(CSRCS) $(CFLAGS) > depend.mak
	@cp depend.mak /tmp/depend.mak.tmp
	@sed "s/o:/op:/" /tmp/depend.mak.tmp >> depend.mak
	@sed "s/o:/od:/" /tmp/depend.mak.tmp >> depend.mak
	@sed "s/o:/or:/" /tmp/depend.mak.tmp >> depend.mak
	@sed "s/o:/ox:/" /tmp/depend.mak.tmp >> depend.mak
	@rm /tmp/depend.mak.tmp

include depend.mak
