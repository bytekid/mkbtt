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

(** Some auxiliary functions to deal with node sets.
@author Sarah Winkler
@since  2009/01/16 *)


(*** OPENS ***************************************************************)
open Util;;
(*** SUBMODULES **********************************************************)
module Trs = U.Trs;;
module W = World;;
module CP = CompletionProcessx;;
module N = IndexedNode;;
module Monad = W.Monad;;

open Monad;;
open Completion;;

(*** FUNCTIONS ***********************************************************)
let by_id = lift Option.the <.> N.by_id

let map_union f is =
 let fold res i =
  by_id i >>= fun n ->
  return ((f n) @ res)
 in
 foldl fold [] is
;;

let map_union_with_class f is =
 let fold res i =
  (lift Option.the <.> N.pattern_by_id) i >>= fun p ->
  by_id i >>= fun n ->
  return ((List.map (fun x -> (x,p))(f n)) @ res)
 in
 foldl fold [] is
;;

let map_union_with_classx f is =
 let fold res i =
  (lift Option.the <.> N.pattern_by_id) i >>= fun p ->
  by_id i >>= fun n ->
  let orient b = if b then id else Pair.flip in
  return ((List.map (fun (x,b) -> (x, orient b p)) (f n)) @ res)
 in
 foldl fold [] is
;;

let fold f = foldr (fun i a -> by_id i >>= fun n -> return (f n a))

let exists p s = fold (fun x b -> (p x) || b) false s

(***** Projections *****)
let project_e p = map_union (Nodex.project_e p);;

let project_e_with_class p = map_union_with_class (Nodex.project_e p)

let project_r p ns = 
 map_union (Nodex.project_r p) ns >>= fun trs -> 
 return (Trs.of_list trs)
;;

let project_r_with_class p = map_union_with_classx (Nodex.project_rx p)

let project_c p ns =  
 map_union (Nodex.project_c p) ns >>= fun trs -> return (Trs.of_list trs)
;;

let project_c_with_class p = map_union_with_classx (Nodex.project_cx p) 
 
let project_r_closed p n =
 map_union (Nodex.project_r_closed p) n >>= fun trs -> 
 return (Trs.of_list trs) 
;;

let project_r_open p n =
 map_union (Nodex.project_r_open p) n >>= fun trs ->
 return (Trs.of_list trs)
;;


let project_e_closed p = map_union (Nodex.project_e_closed p)


(***** Create pre-oriented nodes from TRS *****)
let of_axioms_strict trs =
 let rlist = Trs.to_list trs in
 let add xs x = 
  N.create_preoriented_axiom x >>= N.id >>= fun i -> return (i::xs) 
 in
 foldl add [] rlist >>= fun is ->
 World.get_options >>= fun o ->
 World.set_options {o with axiom_ids = is} >>
 return is
;;

(***** Create initial nodes from equations *****)
let of_axioms es =
 let add xs x = 
  N.create_axiom x >>= N.id >>= fun i -> 
  return (i::xs) in
 foldl add [] es 
;;


(***** Check whether process occurs in some label of node set *****)
let contains_process_open p = exists (Nodex.contains_process_open p)

(***** convert node set to string *****)
let to_string ns =
 fold (fun n s -> (Nodex.to_string n) ^ ", " ^ s) "" ns
;;

let to_stringm l =
 let app s i = N.to_stringm i >>= fun n -> return (s ^ "\n " ^ n) in
 foldl app "" l >>= fun ls -> return ("["^ls^"]")
;;
