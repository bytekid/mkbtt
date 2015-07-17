val trs : CompletionProcessx.t -> U.Trs.t -> 
  (U.Rule.t * (U.Term.t * Rewriting.Position.t * U.Rule.t * bool * U.Term.t) list) list World.Monad.t
 
val trs2 : 
 CompletionProcessx.t -> U.Trs.t -> 
  (U.Trs.t * (U.Rule.t * (U.Term.t * Rewriting.Position.t * U.Rule.t * bool * U.Term.t) list) list) World.Monad.t
