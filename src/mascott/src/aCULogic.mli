(*** VALUES ******************************************************************)
val theory: Theory.t ref

val unify :
 U.Term.t * U.Term.t -> U.Substitution.t list option U.Monad.t
(** [unify (u,v)] returns a complete set of unifiers [ss] of the terms 
[u] and [v]. *)

