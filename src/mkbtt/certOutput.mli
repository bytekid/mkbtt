val fprintfx_completeness : CompletionProcessx.t -> U.Trs.t -> unit World.Monad.t

val fprintfx_conversion : Conversion.t list -> unit World.Monad.t

val fprintfx_proof_trees : EquationalProofTree.t list -> unit World.Monad.t

val fprintfx_disproof : CompletionProcessx.t -> U.Trs.t -> unit World.Monad.t

val fprintfx_subsumption_proof : Conversion.t list list -> unit World.Monad.t
