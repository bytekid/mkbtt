/*Copyright 2008 
 Martin Korp, Christian Sternagel, Sarah Winkler, Harald Zankl */
/* GNU Lesser General Public License */
#include <caml/camlidlruntime.h>

#include"interface.h"

int is_sat(void *s){
 enter_blocking_section ();
 int sat = is_satisfiable_aux (s);
 leave_blocking_section ();
 return sat;
}
