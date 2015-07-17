(*** TYPES **************************************************************)
type t

(*** VALUES *************************************************************)
val of_term : U.Term.t -> t U.Monad.t

val to_stringm : t -> string U.Monad.t

val is_empty : t -> bool

val next : t -> t

val after : t -> t

val top_and_next : t -> (Types.Label.t * t) option

val top_fun_label : t -> Types.Label.t option

val top_label : t -> Types.Label.t

val ac_subterms : t -> U.Term.t list

val ac_term : t -> U.Term.t

val pos : t -> Rewriting.Position.t
