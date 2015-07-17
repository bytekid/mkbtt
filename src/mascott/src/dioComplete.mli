(* 

   basis [a0,...,am] [b0,...b,n]  

   computes a basis for the the following
   linear diophantine equation:

   Sum_1^m a_i * x_i  =  Sum_1^n b_j * y_j

*)

val basis : int list -> int list -> (int list * int list) list
