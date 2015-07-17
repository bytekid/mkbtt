(* Copyright 2010 Sarah Winkler
 * GNU Lesser General Public License
 *
 * This file is part of MKBtt.
 * 
 * MKBtt is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * MKBtt is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with MKBtt. If not, see <http://www.gnu.org/licenses/>.
 *)

module VSub : Util.Replacement.SIGNATURE 
  with type domain = Rewriting.Variable.t 
  and type range = Rewriting.Variable.t

(*** VALUES ******************************************************************)
val var : U.Term.t -> Rewriting.Variable.t
(* returns [v] if [t] is of shape [Var v] *)

val my_compare : U.Term.t -> U.Term.t -> int
(** Total compare function for flat terms such that constants are smaller 
than compound terms which in turn are smaller than variables. To compare 
variables, Variable.compare is used. To compare function symbols, Fun.compare
is employed. Terms with the same AC-root symbol are compared by 
lexicographically comparing their arguments. *)

val flatten : U.Term.t -> U.Term.t U.Monad.t
(** [flatten t] returns a flattened AC-equivalent representation of [t]
and orders subterm lists according to [compare]. *)

val normalize : U.Term.t -> U.Term.t * U.Substitution.t
(** [normalize t] renames [t] with normalized variables. *)

val normalize_rule : U.Rule.t -> U.Rule.t * U.Substitution.t
(** [normalize r] returns [r, rho] for normalized-renamed variant [r] and
variable renaming [rho] *)

val normalize_flatten_both : U.Term.t -> U.Term.t -> ((U.Term.t * U.Term.t) * bool) U.Monad.t
(** [normalize_both s t] returns renames the given terms with normalized 
variables. Before normalization [s] and [t] are compared to ensure a 
unique result. *)

val skolemization : U.Term.t -> U.Substitution.t U.Monad.t
(** [skolemization t] returns a substitution replacing every variable in 
 [t] by a fresh constant. *)

val skolemize : U.Term.t -> U.Term.t -> (U.Term.t * U.Term.t) U.Monad.t
(** [skolemize s t] replaces all variables in the given terms by fresh
constants. *)

val nonvar_pos_proper_subterms : 
 U.Term.t -> (U.Term.t * Rewriting.Position.t) list
(** [nonvar_pos_subterms t] returns the list of all non-variable positions
together with the respective subterms. Does not respect AC symbols! *)

val subterm : U.Term.t -> ACPosition.t -> U.Term.t

val rename_to : VSub.t -> U.Term.t -> U.Term.t -> VSub.t
(** [rename_to rho s t] checks whether the renaming substitution [rho] can 
be extended to a substitution renaming s to t.*)

val funs_pos : U.Term.t -> ACPosition.t list U.Monad.t
(** [funs_pos t] returns a list of all function symbol AC-positions in [t]
 *)


val funs_pos_below_root : U.Term.t -> ACPosition.t list U.Monad.t
(** [funs_pos t] returns a list of all function symbol AC-positions in [t]
below the root
 *)

val funs_pos_nonsymm : U.Rule.t -> ACPosition.t list U.Monad.t
(** [funs_pos rl] returns a list of all function symbol AC-positions in 
 the left-hand side of [rl] for which there is no symmetric position *)

val funs_pos_extended : U.Rule.t -> U.Term.t -> ACPosition.t list U.Monad.t

val context_subterm : U.Term.t -> ACPosition.t -> U.Context.t * U.Term.t

val replace : ACPosition.t -> U.Term.t -> U.Term.t -> U.Term.t

val ac_equivalent : U.Term.t -> U.Term.t -> bool U.Monad.t

val readable : U.Term.t -> U.Term.t U.Monad.t

val unflatten : U.Term.t -> U.Term.t U.Monad.t

val test_signature :
  (U.Term.t * U.Term.t * U.Term.t) *
  (Rewriting.Function.t * Rewriting.Function.t * Rewriting.Function.t * 
   Rewriting.Function.t * Rewriting.Function.t) *
  U.Signature.t
(** Small signature for testing purposes. *)
