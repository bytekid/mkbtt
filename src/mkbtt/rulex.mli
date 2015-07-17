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

(*** VALUES ******************************************************************)
val rewrite :
 U.Term.t -> U.Rule.t -> Rewriting.Position.t ->
 (U.Term.t * U.Substitution.t * U.Rule.t) option U.Monad.t
 (** [rewrite r t p] rewrites term [t] with rule [r] at position [p] and
 returns the resulting term together with the substitution applied to [r]
 and the renamed rule. *)

val narrow :
 U.Term.t -> U.Rule.t -> Rewriting.Position.t ->
 (U.Term.t * U.Substitution.t * U.Rule.t) option U.Monad.t
 (** [narrow t r p] narrows term [t] with rule [r] at position [p] and
 returns the resulting term together with the substitution applied to [r]
 and the instantiated rule. *)


val rewrite_subrule :
 U.Term.t -> U.Rule.t -> Rewriting.Position.t -> 
 (U.Term.t * U.Rule.t) option U.Monad.t
 (** [rewrite_subrule t r p] rewrites term [t] with rule [r] at position 
 [p]. It is assumed that [r] is lready renamed. The resulting term together 
 with the substitution applied to [r] and the instantiated rule are 
 returned. *)

