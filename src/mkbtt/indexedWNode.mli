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

(*** SUBMODULES *********************************************************)
module CP : sig
 type t = int list
end

(*** VALUES *************************************************************)
val content :
 World.IntWNodeIndex.key -> 
 (U.Term.t * (CP.t list * CP.t list)) World.Monad.t

val to_stringm : 
 World.IntWNodeIndex.key -> string World.Monad.t

val create :
 U.Term.t ->
 CP.t list -> CP.t list ->
 WaldmeisterGoalNodex.origin list -> WaldmeisterGoalNodex.origin list -> 
 World.IntWNodeIndex.key World.Monad.t

val term : 
 World.IntWNodeIndex.key -> U.Term.t World.Monad.t

val bcontains_prefix : 
 bool -> CP.t -> World.IntWNodeIndex.key -> bool World.Monad.t

val inter_contains_process : 
 CP.t -> World.IntWNodeIndex.key -> bool World.Monad.t

val has_non_empty_processes : 
 World.IntWNodeIndex.key -> bool World.Monad.t

val restrict_to_process : 
 CP.t -> World.IntWNodeIndex.key -> unit World.Monad.t

val remove_processes : 
 CP.t list -> World.IntWNodeIndex.key -> unit World.Monad.t

val split : 
 CP.t list -> World.IntWNodeIndex.key -> unit World.Monad.t

val add_processes1 : 
 CP.t list * WaldmeisterGoalNodex.origin -> World.IntWNodeIndex.key -> 
 unit World.Monad.t

val add_processes2 :
 CP.t list * WaldmeisterGoalNodex.origin -> World.IntWNodeIndex.key ->
 unit World.Monad.t

val process_inter : 
 World.IntWNodeIndex.key -> CP.t list World.Monad.t

val borigin : 
 bool -> World.IntWNodeIndex.key -> 
 WaldmeisterGoalNodex.origin list World.Monad.t
