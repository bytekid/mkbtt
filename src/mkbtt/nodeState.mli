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
val open_nodes : int list World.Monad.t
val closed_nodes : int list World.Monad.t
val non_empty_open_nodes_exist : bool World.Monad.t
val all_nodes : int list World.Monad.t

val remove_open : int -> unit World.Monad.t
val add_open : int list -> unit World.Monad.t
val set_open : int list -> unit World.Monad.t
val index_add_closed_node : int -> unit World.Monad.t
val remove_closed : int -> unit World.Monad.t

val er_contain_closed : Nodex.CP.t -> int list World.Monad.t
val remove_processes : CompletionProcessx.t list -> unit World.Monad.t
val restrict_to_process : CompletionProcessx.t -> unit World.Monad.t
val split_state : CompletionProcessx.t list -> unit World.Monad.t
val single_rindex : int -> Types.NodeTermIndex.t World.Monad.t
val project_r_closed : CompletionProcessx.t -> U.Trs.t World.Monad.t
val project_e_closed : 
 CompletionProcessx.t -> Equation.t list World.Monad.t
val get_projections : CompletionProcessx.t -> (Equation.t list * U.Trs.t * U.Trs.t) World.Monad.t
val get_projections_with_class : 
 CompletionProcessx.t -> 
 ((Equation.t * (int * int)) list * 
 (U.Rule.t * (int * int)) list * 
 (U.Rule.t * (int * int)) list) World.Monad.t
