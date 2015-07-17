(*** VALUES **************************************************************)
val map_union : (Types.Node.t -> 'a list) -> int list -> 'a list World.Monad.t

val exists : (Types.Node.t -> bool) -> int list -> bool World.Monad.t

val map : (Types.Node.t -> 'a World.Monad.t) -> int list -> 'a list World.Monad.t

val fold : (Types.Node.t -> 'a -> 'a ) -> int list -> 'a -> 'a World.Monad.t

val to_stringm : int list -> string World.Monad.t
