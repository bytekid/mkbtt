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

(** Maintains a node set. 
 @author Sarah Winkler
 @since  2010/11/18 *)

(** Functions for  Waldmeister Goal nodes indexed by an integer *)
(*** OPENS ***************************************************************)
open Util;;

(*** SUBMODULES (1) ******************************************************)
module H = Hashtbl;;
module Rewriting = Processors.Rewritingx;;
module Term = Rewriting.Term;;
module Rule = Rewriting.Rule;;
module W = World;;
module Monad = W.Monad;;
module CP = CompletionProcessx;;
module IW = W.IntWNodeIndex;;
module WNode = WaldmeisterGoalNodex;;

(*** OPENS ***************************************************************)
open Monad;;
open World;;

(*** FUNCTIONS ***********************************************************)
let by_id i = 
 W.get_goal_state >>= fun c -> return (IW.find_elt i (fst c.windex))
;;

let id n = 
 W.get_goal_state >>= fun c -> return (IW.find_key n (fst c.windex))
;;

let by_term t = 
 W.get_goal_state >>= fun c -> 
 return (try Some (H.find (snd c.windex) t) with Not_found -> None)
;;

let id_lift f i = by_id i >>= fun n -> return (f n);;
let id_liftm f i = by_id i >>= f;;

let store n =
 W.get_goal_state >>= fun c ->
 let i, ids' = IW.fresh (fst c.windex) in
 let ids' = IW.add i n ids' in
 H.add (snd c.windex) (WNode.term n) i;
 W.set_goal_state {c with windex = (ids', snd c.windex)}
;;

let restore f i =
 by_id i >>= fun n ->
 W.get_goal_state >>= fun c ->
 let n' = f n in
 let ids' = IW.replace i n' (fst c.windex) in
 W.set_goal_state {c with windex = (ids', snd c.windex)}
;;

let borigin b = id_lift (WNode.borigin b);;
let content = id_lift WNode.content;;
let split s = restore (WNode.map (CP.apply_split s));;
let to_stringm = id_liftm WNode.to_stringm;;
let term = id_lift WNode.term;;

let create t p0 p1 o0 o1 = 
 by_term t >>= function
 | Some i -> restore (WNode.merge p0 p1 o0 o1) i >> return i
 | None -> let n = WNode.create t p0 p1 o0 o1 in store n >> id n
;;

let add_processes1 ps = restore (WNode.add_processes1 ps);;
let add_processes2 ps = restore (WNode.add_processes2 ps);;
let process_inter = id_lift WNode.process_inter;;
let bcontains_prefix b p = id_lift (WNode.bcontains_prefix b p);;
let inter_contains_process p = id_lift (WNode.inter_contains_process p);;
let has_non_empty_processes = id_lift WNode.has_non_empty_processes;;
let restrict_to_process p = restore (WNode.restrict_to_process p)
let remove_processes ps = restore (WNode.remove_processes ps)

