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

(*** TYPES **************************************************************)
type entry = int * bool

type t = entry World.M.ACDiscTree.t

(*** VALUES *************************************************************)
val make : t World.Monad.t

val insert : U.Term.t * entry -> unit World.Monad.t

(*val delete : U.Term.t * Types.NodeTermIndex.entry -> unit World.Monad.t*)

val variants : 
 U.Term.t -> entry list World.Monad.t

val variants_in : 
 t -> U.Term.t -> entry list World.Monad.t

val encompassments :
  (U.Term.t * ACPosition.t list) -> (entry * ACPosition.t) list World.Monad.t

val encompassments_in :
  t -> (U.Term.t * ACPosition.t list) -> (entry * ACPosition.t) list World.Monad.t

val encompassments_below_root :
  (U.Term.t * ACPosition.t list) -> (entry * ACPosition.t) list World.Monad.t

val empty : t World.Monad.t

val get_index : t World.Monad.t

val indexing_required : 
 U.Term.t -> U.Term.t -> (int * bool) -> bool World.Monad.t

val insert_one :
 t -> U.Term.t * entry -> t World.Monad.t

val add_node : int -> unit World.Monad.t

val single_index : int -> t World.Monad.t

val add_s_node : int -> unit World.Monad.t

val s_encompassments :
  (U.Term.t * ACPosition.t list) -> (entry * ACPosition.t) list World.Monad.t

(*val remove_node : bool -> int -> unit World.Monad.t*)

