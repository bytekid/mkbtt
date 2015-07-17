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
module NSA = NodeSetAux;;
module N = IndexedNode;;
module NI = NodeTermIndex;;
module Monad = W.Monad;;

(*** OPENS ***************************************************************)
open Monad;;
open World;;

(*** FUNCTIONS ***********************************************************)

let union3 xs ys = List.union (List.union xs ys)

let log2 s = (*Format.printf "%s\n" s;*) W.log 2 s

let open_nodes = W.get_node_state >>= fun c -> return c.open_nodes

let closed_nodes = W.get_node_state >>= fun c -> return c.closed_nodes

let ext_nodes = W.get_node_state >>= fun c -> return c.ext_nodes

let s_nodes = W.get_node_state >>= fun c -> return c.s_nodes

let fixed_rules = W.get_node_state >>= fun c -> return c.fixed_rules

let non_empty_open_nodes_exist =
 open_nodes >>= fun o ->
 filter N.has_non_empty_labels o >>= fun o' ->
 return (not(List.is_empty o'))
;;

let all_nodes =
 W.get_node_state >>= fun c ->
 return (union3 c.closed_nodes c.open_nodes c.s_nodes)
;;

let set_open ns =  
 W.get_node_state >>= fun c ->
 W.set_node_state {c with open_nodes = ns}
;;

let add_open ns =
 W.get_node_state >>= fun c -> 
 W.set_node_state {c with open_nodes = List.union ns c.open_nodes}
;;

let add_closed ns =
 W.get_node_state >>= fun c ->
 W.set_node_state {c with closed_nodes = List.union ns c.closed_nodes}
;;

let remove_open n =
 W.get_node_state >>= fun c -> 
 W.set_node_state {c with open_nodes = List.remove n c.open_nodes}
;;

let add_fixed_rule rule =
 W.get_node_state >>= fun c ->
 W.set_node_state {c with fixed_rules = U.Trs.add rule c.fixed_rules}
;;

let index_add_extended ns =
 (*iter NI.add_node ns >>*)
(* IndexedSet.to_stringm ns >>= fun s ->
 log2 ("adding extended: "^s) >>*)
 W.get_node_state >>= fun c ->
 W.set_node_state {c with ext_nodes = List.union ns c.ext_nodes}
;;

let set_s_theory ns =
 W.get_node_state >>= fun c ->
 W.set_node_state {c with s_nodes = ns} >>
 iter NI.add_s_node ns
;;

let index_add_closed_node n =
 NI.add_node n >>
 W.get_node_state >>= fun c -> 
 W.set_node_state {c with closed_nodes=List.union [n] c.closed_nodes}
;;

let split_state s =
 W.get_node_state >>= fun c ->
 iter (N.split s) c.open_nodes >>
 iter (N.split s) c.closed_nodes >>
 iter (N.split s) c.ext_nodes >>
 iter (N.split s) c.s_nodes >>
 CompletionProcessx.split_state s 
;;

let restrict_to_process p =
 W.get_node_state >>= fun c ->
 iter (N.restrict_to_process p) c.open_nodes >>
 iter (N.restrict_to_process p) c.closed_nodes >>
 iter (N.restrict_to_process p) c.ext_nodes >>
 iter (N.restrict_to_process p) c.s_nodes 
;;

let remove_processes ps =
 W.get_node_state >>= fun c ->
 iter (N.remove_processes ps) c.open_nodes >>
 iter (N.remove_processes ps) c.closed_nodes >>
 iter (N.remove_processes ps) c.ext_nodes >>
 iter (N.remove_processes ps) c.s_nodes >>
 CompletionProcessx.remove ps
;;

let project_r_closed p = 
 closed_nodes >>= NodeSetAux.project_r_closed p
;;

let project_r_closed_unprotected p =
 closed_nodes >>= NodeSetAux.project_r_closed_unprotected p
;;


let project_e_closed p = 
 closed_nodes >>= NodeSetAux.project_e_closed p
;;

let er_contain_closed p =
 closed_nodes >>= filter (N.er_contains_closed p)
;;

let s_rules =
 W.get_processes >>= fun c ->
 s_nodes >>= NodeSetAux.project_r_closed (List.hd c.all)
;;
