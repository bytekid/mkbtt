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
val normalize : U.Term.t -> U.Term.t * U.Substitution.t
(** [normalize t] renames [t] with normalized variables. *)

val normalize_both : U.Term.t -> U.Term.t -> (U.Term.t * U.Term.t)
(** [normalize_both s t] returns renames the given terms with normalized 
variables. Before normalization [s] and [t] are compared to ensure a 
unique result. *)

val skolemize : U.Term.t -> U.Term.t -> (U.Term.t * U.Term.t) U.Monad.t
(** [skolemize s t] replaces all variables in the given terms by fresh
constants. *)

val nonvar_pos_proper_subterms : 
 U.Term.t -> (U.Term.t * Rewriting.Position.t) list
(** [nonvar_pos_subterms t] returns the list of all non-variable positions
together with the respective subterms. *)

val rename_to : VSub.t -> U.Term.t -> U.Term.t -> VSub.t
(** [rename_to rho s t] checks whether the renaming substitution [rho] can 
be extended to a substitution renaming s to t.*)

val fresh_vars : U.Term.t -> U.Term.t U.Monad.t
(** [fresh_vars t] replaces all variables in the given terms by fresh ones. *)

val test_signature :
  (U.Term.t * U.Term.t * U.Term.t) *
  (Rewriting.Function.t * Rewriting.Function.t * Rewriting.Function.t * 
   Rewriting.Function.t * Rewriting.Function.t) *
  U.Signature.t
(** Small signature for testing purposes. *)
