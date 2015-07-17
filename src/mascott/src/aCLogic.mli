(*** VALUES ******************************************************************)
val t_solve : float ref

val ac_root : U.Term.t -> Rewriting.Function.t option U.Monad.t

val matches : 
 U.Term.t * U.Term.t -> U.Substitution.t list option U.Monad.t
(** [matches (u,v)] returns a complete set of matching substitutions [ss] 
such that for every [s] in [ss] [v s = u], provided that [u] and [v] are
variable disjoint (no occur check!). *)

val unify :
 U.Term.t * U.Term.t -> U.Substitution.t list option U.Monad.t
(** [unify (u,v)] returns a complete set of unifiers [ss] of the terms 
[u] and [v]. *)

val unify_cache : 
 U.Term.t * U.Term.t -> U.Substitution.t list option U.Monad.t
(** same as [unify (u,v)], but with normalized renaming and caching. *)
