/*Copyright 2008 
 Martin Korp, Christian Sternagel, Sarah Winkler, Harald Zankl */
/* GNU Lesser General Public License */
/** Interface functions to access the Minisat Solver from OCaml.
    Compile with
      $ g++ -c -Wall -I./minisat/core -I./minisat/mtl accessMinisat.c
    including the minisat stuff to obtain a .o file that can be linked
    with OCaml 
**/

// inclusions needed for minisat
#include <ctime>
#include <cstring>
#include <stdint.h>
#include <errno.h>

#include <signal.h>
#include <zlib.h>
#include <cmath>

#include "Solver.h"
#include "Sort.h"
// include interface definitions for access from OCaml
#include "interface.h" 

// convenient for some output
#include<iostream>

using namespace std;

void* get_solver (){
 Solver *s = new Solver ();
 return s;
}

void add(void* s, int lita[], int len){
 Solver *t = (Solver*) s;
 int var;
 vec<Lit> lits;
 for (int i = 0; i < len; ++i) {
  var = abs(lita[i]) - 1;
  while (var >= t->nVars()) t->newVar();
  lits.push((lita[i] > 0) ? Lit(var) : ~Lit(var));
 }
 t->addClause(lits);
 return;
}

int is_satisfiable_aux(void *s){
 Solver *t = (Solver*) s;
 return (t->solve());
}

int val_of (void *s, int i){
 Solver *t = (Solver*) s;
 //printf ("i=%d, nvars=%d\n", i, t->nVars());
 assert (i < t->nVars());
 return ((*t).model[i] != l_False);
}

void delete_solver (void *s){
 Solver *t = (Solver*) s;
 delete t;
 return;
}
