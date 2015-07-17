(*** TYPES ***************************************************************)
type symbol_theory =
 | AC of Rewriting.Function.t
 | ACU of (Rewriting.Function.t * Rewriting.Function.t) (* f, zero *)

type t = symbol_theory list

val recognize_acu : U.Trs.t -> t U.Monad.t

val is_acu_symbol : Rewriting.Function.t -> t -> bool

val unit_for : Rewriting.Function.t -> t -> Rewriting.Function.t

val recognize_ac : Equation.t list -> (Equation.t list * Rewriting.Function.t list)

val ac : Rewriting.Function.t -> Equation.t list

val assoc : Rewriting.Function.t -> Equation.t

val comm : Rewriting.Function.t -> Equation.t
