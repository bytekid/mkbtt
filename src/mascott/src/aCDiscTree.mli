(*** TYPES **************************************************************)
type 'a t = 'a Types.ACDiscTree.t
(*** VALUES *************************************************************)
val empty : 'a t

val insert : (U.Term.t * (int * bool)) -> (int * bool) t -> (int * bool) t U.Monad.t

val variants : U.Term.t -> (int * bool) t -> ((int * bool) list) U.Monad.t

val encompassments : (U.Term.t * ACPosition.t list) -> (int * bool) t -> (((int * bool) * ACPosition.t)list) U.Monad.t

val encompassments_below_root : (U.Term.t * ACPosition.t list) -> (int * bool) t -> (((int * bool) * ACPosition.t)list) U.Monad.t
