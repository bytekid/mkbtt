(*** VALUES **************************************************************)
val print_sub : U.Substitution.t -> string U.Monad.t

val print_subs : U.Substitution.t list -> unit U.Monad.t

val remove_common_args : U.Term.t list -> U.Term.t list -> (U.Term.t list * U.Term.t list)

val abstract : U.Term.t -> U.Term.t -> ( (U.Term.t * U.Term.t) list * (Rewriting.Variable.t list) * (Rewriting.Variable.t list) * U.Substitution.t) U.Monad.t

val more_abstract : U.Term.t -> U.Term.t -> ( (U.Term.t * U.Term.t) list * (Rewriting.Variable.t list) * (Rewriting.Variable.t list) * U.Substitution.t) U.Monad.t

val to_semigroup : Rewriting.Function.t -> (Rewriting.Variable.t * int) list list -> (U.Term.t list array array * Rewriting.Variable.t list) U.Monad.t

val str_assignments : (Rewriting.Variable.t * int) list list -> string U.Monad.t

val subsets : (U.Term.t list * U.Term.t list) -> (Rewriting.Variable.t * int) list list -> int list list

val sum_up : int list list -> U.Term.t list array array -> Rewriting.Variable.t list * Rewriting.Variable.t list -> Rewriting.Function.t -> U.Substitution.t list

val sum : U.Term.t list array array -> Rewriting.Variable.t list * Rewriting.Variable.t list -> (Rewriting.Function.t * Rewriting.Function.t) -> U.Substitution.t

val make_idempotent : U.Substitution.t -> U.Substitution.t

val make_all_idempotent : U.Substitution.t list -> U.Substitution.t list

val sub_constraints : U.Substitution.t -> (U.Term.t * U.Term.t) list -> (U.Term.t * U.Term.t) list

val acu_unify_fterm : U.Term.t -> U.Term.t -> (Rewriting.Function.t * Rewriting.Function.t) -> (U.Term.t * U.Term.t) list list

val str_vars : Rewriting.Variable.t list -> string U.Monad.t

val print_constraints : (U.Term.t * U.Term.t) list -> unit U.Monad.t

val list_to_stringm : ('a -> string U.Monad.t) -> 'a list -> string U.Monad.t

val feterm : Rewriting.Function.t -> Rewriting.Function.t -> U.Term.t list -> U.Term.t

val dio_solve : (Rewriting.Variable.t list) -> (Rewriting.Variable.t list) -> (Rewriting.Variable.t * int) list list

