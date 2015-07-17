type t =
   Empty of U.Term.t
 | Step of U.Term.t * Nodex.t_rule * Rewriting.Position.t * U.Term.t
;;

val start_term : t list -> U.Term.t

val end_term : t list -> U.Term.t

val for_goal : CompletionProcessx.t -> t list World.Monad.t

val for_goal_with_lemmas : CompletionProcessx.t -> t list list World.Monad.t

