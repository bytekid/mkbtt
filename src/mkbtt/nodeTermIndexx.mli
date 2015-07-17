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

(*** VALUES *************************************************************)
val make : (Types.NodeTermIndex.t * Types.NodeTermIndex.t) World.Monad.t

val insert : U.Term.t * Types.NodeTermIndex.entry -> unit World.Monad.t

val delete : U.Term.t * Types.NodeTermIndex.entry -> unit World.Monad.t

val variant_candidates : 
 U.Term.t -> Types.NodeTermIndex.entry list World.Monad.t

val variant_candidates_in : 
 Types.NodeTermIndex.t -> 
 U.Term.t -> Types.NodeTermIndex.entry list World.Monad.t

val generalization_candidates : 
 U.Term.t -> Types.NodeTermIndex.entry list World.Monad.t

val unification_candidates : 
 U.Term.t -> Types.NodeTermIndex.entry list World.Monad.t

val encompassment_candidates :
  U.Term.t -> 
 (Types.NodeTermIndex.entry * Rewriting.Position.t) list World.Monad.t

val encompassment_candidates_in :
  Types.NodeTermIndex.t -> U.Term.t -> 
  (Types.NodeTermIndex.entry * Rewriting.Position.t) list World.Monad.t

val encompassment_candidates_below_root :
  U.Term.t -> 
  (Types.NodeTermIndex.entry * Rewriting.Position.t) list World.Monad.t

val overlap1_candidates_below_root :
  U.Term.t -> 
 (Types.NodeTermIndex.entry * Rewriting.Position.t) list World.Monad.t

val overlap1_candidates :
  U.Term.t -> 
  (Types.NodeTermIndex.entry * Rewriting.Position.t) list World.Monad.t

val overlap2_candidates :
  U.Term.t -> 
 (Types.NodeTermIndex.entry * Rewriting.Position.t) list World.Monad.t

val empty_rindex : Types.NodeTermIndex.t World.Monad.t

val rewrite_index : Types.NodeTermIndex.t World.Monad.t

val indexing_required : 
 U.Term.t -> U.Term.t -> (int * bool) -> bool World.Monad.t

val insert_one :
 Types.NodeTermIndex.t -> 
 U.Term.t * Types.NodeTermIndex.entry -> 
 Types.NodeTermIndex.t World.Monad.t

val add_node : int -> unit World.Monad.t

val remove_node : bool -> int -> unit World.Monad.t

