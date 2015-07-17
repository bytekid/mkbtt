/* File generated from accessMinisatP.idl */

#include <stddef.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#ifdef Custom_tag
#include <caml/custom.h>
#include <caml/bigarray.h>
#endif
//#include <caml/camlidlruntime.h>


#include "accessMinisatP.h"

int gn_var=0;

value camlidl_accessMinisatP_initSolving(
	value _v_n_var,
	value _v_n_con,
	value _v_cmd)
{
  int n_var; /*in*/
  int n_con; /*in*/
  int cmd; /*in*/
  n_var = Int_val(_v_n_var);
  gn_var=n_var; //remember number of variables for memory allocation in solve
  n_con = Int_val(_v_n_con);
  cmd = Int_val(_v_cmd);
  initPbSolving(n_var, n_con, cmd);
  //printf("Calling initsolving with %i variables, %i constraints and command %i", n_var, n_con, cmd);
  return Val_unit;
}

value camlidl_accessMinisatP_addConstraint(
	value _v_ps,
	value _v_cs,
	value _v_rhs,
	value _v_ineq)
{
  //char (*ps)[30]; /*in*/
  char** ps;
  int *cs = NULL; /*in*/
  int n_p; 
  int rhs; /*in*/
  int ineq; /*in*/
  value _v_ps_tmp, _v_cs_tmp;

  rhs = Int_val(_v_rhs);
  ineq = Int_val(_v_ineq);

  _v_ps_tmp = _v_ps;
  _v_cs_tmp = _v_cs;

  int i=0;

  //Determine ist length - maybe list length should be required as a parameter
  while ((_v_ps_tmp != Val_int(0)) && (_v_cs_tmp != Val_int(0))){ //while no list end reached
    i++;
    _v_cs_tmp = Field(_v_cs_tmp, 1);	
    _v_ps_tmp = Field(_v_ps_tmp, 1);   
  }
  n_p=i;

  // fail if there are not as many variables as coefficients
  if ((_v_ps_tmp != Val_int(0)) || (_v_cs_tmp != Val_int(0)))
    caml_failwith("invalid constraint: lists have different lengths"); //causes Failure "..."

  //allocate array space
  ps = malloc(n_p * sizeof(*ps));
  cs = malloc(n_p * sizeof(int)); //hope that strings are not longer than 30
  
  //printf("In addConstraint. list should have %i members.\n", n_p);

  _v_ps_tmp = _v_ps;
  _v_cs_tmp = _v_cs;

  for (i=0; i<n_p; i++){
    cs[i] = Int_val(Field(_v_cs_tmp, 0)); //first part of cons cell contains list element ...
    ps[i] = malloc(30);
    strncpy(ps[i], String_val(Field(_v_ps_tmp, 0)), 29);
    //printf("   Filled\n");
   
    _v_cs_tmp = Field(_v_cs_tmp, 1);	// second component contains rest of list
    _v_ps_tmp = Field(_v_ps_tmp, 1);
    //printf("  Component %i: %i * %s\n", i, cs[i], ps[i]);
  }
  addConstraint(ps, cs, n_p, rhs, ineq);
  return Val_unit;
}

value camlidl_accessMinisatP_solve( )
{
  CAMLparam0();
  char** result=NULL; /*in*/
  int _res=0;
  //mlsize_t _c1;
  //mlsize_t _c2;
  //value _v3;
  int i;

  result = malloc(gn_var * sizeof(*result));	//allocate memory
  for (i=0; i<gn_var; i++)
	result[i] = malloc(30);

  _res = solvePb(result);			//fill result array

  CAMLlocal2(list, temp);
  temp = Val_int(0);                   			/* empty list */

if (_res >= 0)	// SATISFIABLE - put true variables in list 
  {
  int i;
  for (i=0; i<_res; i++){
     list = caml_alloc(2, 0);              		// Allocate a cons cell 
     Store_field(list, 0, caml_copy_string(result[i]));    	// store next int value 
     Store_field(list, 1, temp);              		// rest of list 
     temp=list;
     }
    list=temp;
    //printf ("satisfiable \n");
  }
else		// UNSATISFIABLE or UNKNOWN - create a singleton list [-1] or [-2]
  {
/*
 //printf ("UNSAT  \n");	
     list = caml_alloc(2, 0);  				
     Store_field(list, 0, Val_int(_res));    
     Store_field(list, 1, temp);  
    //printf ("unsatisfiable \n");
*/    caml_failwith("unsatisfiable or so");  
  }

CAMLreturn (list);
}

