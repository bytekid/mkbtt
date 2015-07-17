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

(** Functions related to node state in global monad.
 @author Sarah Winkler
 @since  2010/10/12 *)

(*** OPENS ***************************************************************)
open Util;;

(*** SUBMODULES (1) ******************************************************)
module Term = U.Term;;
module W = World;;
module N = IndexedNode;;
module NI = NodeTermIndexx;;
module Monad = W.Monad;;

(*** OPENS ***************************************************************)
open Monad;;
open World;;

(*** FUNCTIONS ***********************************************************)

let log2 s = (*Format.printf "%s\n" s;*) W.log 2 s

let open_nodes = W.get_node_state >>= fun c -> return c.open_nodes

let closed_nodes = W.get_node_state >>= fun c -> return c.closed_nodes

let non_empty_open_nodes_exist =
 open_nodes >>= fun o ->
 filter N.has_non_empty_labels o >>= fun o' ->
 return (not(List.is_empty o'))
;;

let all_nodes =
 W.get_node_state >>= fun c ->
 return (List.union c.closed_nodes c.open_nodes)
;;

let set_open ns =  
 W.get_node_state >>= fun c ->
 W.set_node_state {c with open_nodes = ns}
;;

let add_open ns =
 W.get_node_state >>= fun c -> 
 W.set_node_state {c with open_nodes = List.union ns c.open_nodes}
;;

let remove_open n =
 W.get_node_state >>= fun c -> 
 W.set_node_state {c with open_nodes = List.remove n c.open_nodes}
;;

let remove_closed n =
 W.get_node_state >>= fun c ->
 W.set_node_state {c with closed_nodes = List.remove n c.closed_nodes}
;;

let index_add_closed_node n =
 (*log2 "index_add_closed_node" >>*)
 NI.add_node n >>
 W.get_node_state >>= fun c -> 
 W.set_node_state {c with closed_nodes=List.union [n] c.closed_nodes}
;;

let split_state s =
 W.get_node_state >>= fun c ->
 iter (N.split s) c.open_nodes >>
 iter (N.split s) c.closed_nodes >>
 CompletionProcessx.split_state s >>
 GoalState.split s
;;

(*let rewrite_index = M.NodeTermIndex.rewrite_index*)

let restrict_to_process p =
 W.get_node_state >>= fun c ->
 iter (N.restrict_to_process p) c.open_nodes >>
 iter (N.restrict_to_process p) c.closed_nodes >>
 GoalState.restrict_to_process p
;;

let remove_processes ps =
 W.get_node_state >>= fun c ->
 iter (N.remove_processes ps) c.open_nodes >>
 iter (N.remove_processes ps) c.closed_nodes >>
 CompletionProcessx.remove ps >>
 GoalState.remove_processes ps
;;

let project_r_closed p = 
 closed_nodes >>= NodeSet.project_r_closed p
;;

let project_e_closed p = 
 closed_nodes >>= NodeSet.project_e_closed p
;;

let er_contain_closed p =
 closed_nodes >>= filter (N.er_contains_closed p)
;;

let get_projections p =
 all_nodes >>= fun ns -> NodeSet.project_e p ns >>= fun e ->
 NodeSet.project_r p ns >>= fun r ->
 NodeSet.project_c p ns >>= fun c ->
 return (e,r,c)
;;

let get_projections_with_class p =
 all_nodes >>= fun ns -> 
 NodeSet.project_e_with_class p ns >>= fun e ->
 NodeSet.project_r_with_class p ns >>= fun r ->
 NodeSet.project_c_with_class p ns >>= fun c ->
 return (e,r,c)
;;

let single_rindex i =
 let add (t,v) s idx =
  NI.indexing_required t s v >>= fun b ->  
  if b then NI.insert_one idx (t,v)
  else return idx
 in
 N.data i >>= fun (s, t) ->
 NI.empty_rindex >>=
 add (s,(i, true)) t >>= add (t,(i, false)) s
;;

