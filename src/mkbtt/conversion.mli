type t =
   Empty of U.Term.t
 | Step of U.Term.t * Nodex.t_rule * Rewriting.Position.t * U.Term.t
;;

val start_term : t list -> U.Term.t

val end_term : t list -> U.Term.t

val for_goal : t list World.Monad.t

val for_goal_with_lemmas : t list list World.Monad.t

val for_trs : U.Trs.t -> (U.Rule.t * t list) list World.Monad.t
