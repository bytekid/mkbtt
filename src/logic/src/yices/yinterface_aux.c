/* Copyright 2008 Martin Korp, Christian Sternagel, Harald Zankl
 * GNU Lesser General Public License
 *
 * This file is part of TTT2.
 * 
 * TTT2 is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * TTT2 is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with TTT2. If not, see <http://www.gnu.org/licenses/>.
 */

#include <caml/camlidlruntime.h>

#include"yices_c.h"

/* helper function that returns value correctly */
long helper_get_int_value(yices_model m, yices_var_decl x){
 long val = -1;
 yices_get_int_value(m, x, &val);
 return val;
}//end 

long helper_get_num_value(yices_model m, yices_var_decl x){
 long val = -1;
 long val2 = -1;
 yices_get_arith_value(m, x, &val, &val2);
 return val;
}//end 

long helper_get_dnum_value(yices_model m, yices_var_decl x){
 long val = -1;
 long val2 = -1;
 yices_get_arith_value(m, x, &val, &val2);
 return val2;
}//end 

int helper_get_value(yices_model m, yices_var_decl x){
 return yices_get_value(m, x) != l_false;
}//end

int yices_check_aux(yices_context c){
 int r;
 enter_blocking_section ();
 r = yices_check (c);
 leave_blocking_section ();
 return r;
}
