

(*** VALUES **************************************************************)

val t_rewrite1 : float ref

val t_rewrite2 : float ref

val t_match : float ref

val mcount : int ref

val mrepcount : int ref

val rewrite1 : 
 U.Term.t -> U.Rule.t -> (U.Term.t * U.Substitution.t) list U.Monad.t
(** [rewrite1 t rl] returns the list of all terms than can be obtained by 
AC-rewriting [t] with rule [rl] at the root, encapsulated in a monad. This
overapproximates the cases where the right hand side of a rule, but not 
the left-hand side can be reduced according to Bachmair: Completion for 
Rewriting modulo a Congruence. (Actually, possibly existent single term 
obtained by standard-rewriting [t] at the root could be omitted - 
currently these cases are doubly covered by rewrite1 and rewrite2.)*)

val rewrite2 : 
 U.Term.t -> ACPosition.t -> U.Rule.t -> 
 (U.Term.t * U.Substitution.t) list U.Monad.t
(** [rewrite2 t rl] returns the list of all pairs [(t,p)] such that [t] 
can be obtained by AC-rewriting [t] with rule [rl] at position [p] below 
the root, or by standard-rewriting [t] at the root. This corresponds to 
all cases where both the left- and right-hand side of a rule can be 
reduced, according to Bachmair. *) 

val reducts : U.Term.t -> ACPosition.t list -> U.Rule.t -> 
 (U.Term.t * U.Substitution.t * ACPosition.t) list U.Monad.t
(** [reducts t rl] returns the list of all direct reducts of [t] using
 the rule [rl]. *)

val reducible_below_root : U.Term.t -> ACPosition.t list -> U.Rule.t -> bool U.Monad.t
(** [reducible_below_root t rl] checks whether [t] can be reduced at
 a non-root position using rule [rl]. *)

val reducible_at : U.Term.t -> ACPosition.t -> U.Rule.t -> bool U.Monad.t
(** [reducible_at t p rl] checks whether [t] can be reduced at position [p]
 using rule [rl]. *)

val rewrite_with_at : U.Term.t -> U.Rule.t -> ACPosition.t ->
 (U.Term.t * U.Substitution.t) list U.Monad.t

val normalize : U.Trs.t -> U.Term.t -> U.Term.t U.Monad.t

val nf_with : U.Trs.t -> U.Term.t -> ACPosition.t -> (U.Rule.t *  (int * bool) ACDiscTree.t) -> U.Term.t list U.Monad.t

val narrow : U.Term.t -> ACPosition.t list -> U.Rule.t -> 
 (U.Term.t * ACPosition.t * (U.Substitution.t * U.Term.t * U.Term.t)) list U.Monad.t
(** [narrow t rl] returns the list of all triples [(u,p,(sigma.tpsigma))] 
 such that [u] can be obtained by narrowing [t] with rule [rl] at 
 non-variable position [p], where [sigma] is the used substitution and 
 [tpsigma] the instiated subterm of [t], all encapsulated in a monad. *)

val narrow_below_root : U.Term.t -> ACPosition.t list -> U.Rule.t ->
 (U.Term.t * ACPosition.t * (U.Substitution.t * U.Term.t * U.Term.t)) list U.Monad.t
(** [narrow_below_root t rl] does the same as [narrow t rl] but restricted
 to positions below the root. *)

val matches : (U.Term.t * U.Term.t) ->  U.Substitution.t list option U.Monad.t

val joinable : (U.Term.t * U.Term.t) -> U.Trs.t -> bool U.Monad.t
