type t = 
   Refl of U.Term.t
 | Sym of t
 | Trans of t * t
 | Assm of U.Rule.t * U.Substitution.t
 | Cong of Rewriting.Function.t * t list

val plant_and_grow : Conversion.t list -> t World.Monad.t
