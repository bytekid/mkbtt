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

COBJS = \
 yinterface_stubs.o \
 yinterface_aux.o \

FILES= \
 yinterface \

YICES32 = ./32bit
YICES64 = ./64bit
CMI = yinterface.cmi
CMO = $(addsuffix .cmo, $(FILES))
CMX = $(addsuffix .cmx, $(FILES))
CLIBS = interface_yices yices
CLIBFLAGS = $(addprefix -cclib -l, $(CLIBS))
CFLAGS = -FPIC -DPIC -Wall -I $(CAML) -I $(YICES32)/include -I $(YICES64)/include
CC = cc $(CFLAGS)
CXX = g++ $(CFLAGS)

all: 
	camlidl -header yinterface.idl
	$(MAKE) $(COBJS) $(CMI) $(CMO)
	ar rcs libinterface_yices.a $(COBJS)
	$(OCAMLC) -custom -a -o yices.cma $(CLIBFLAGS) $(CMO)
	$(MAKE) $(CMX)
	$(OCAMLOPT) -a -o yices.cmxa $(CLIBFLAGS) $(CMX)

clean:
	rm -f yinterface.{ml,mli,h} yinterface_stubs.c \
         *.cmo *.cmi *.cmx *.cma *.cmx *.cmxa *.a *.o test_native test

#build rules
%.o: %.c
	$(CC) -c $< 

%.cmo: %.ml
	$(OCAMLC) -c $< 

%.cmi: %.mli
	$(OCAMLC) -c $< 

%.cmx: %.ml
	$(OCAMLOPT) -c $<

.PHONY: all clean commit compile compile_native \
 lib $(TARGET).cma $(TARGET).cmxa \
 depend doc native online test update
