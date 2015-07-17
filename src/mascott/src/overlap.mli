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

(*** TYPES ***************************************************************)
type t

(*** VALUES *************************************************************)
val source: t -> U.Term.t

val outer_rule: t -> (U.Rule.t * CompletionProcessx.t list * (int * bool))

val inner_rule: t -> (U.Rule.t * CompletionProcessx.t list * (int * bool))

val position: t -> Termx.pos

val sub : t -> U.Substitution.t

val mom : t -> int * bool

val dad : t -> int * bool

val make :
 U.Term.t ->
 (U.Rule.t * CompletionProcessx.t list * (int * bool)) ->
 Termx.pos ->
 (U.Rule.t * CompletionProcessx.t list * (int * bool)) ->
 U.Substitution.t ->
 t

val is_nontrivial : t -> bool
