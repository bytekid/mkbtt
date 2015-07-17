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
 @since  2010/08/31 *)

(** Functions for nodes indexed by an integer *)
(*** OPENS ***************************************************************)
open Util;;
(*** SUBMODULES (1) ******************************************************)
module Term = U.Term;;
module Rule = U.Rule;;
module W = World;;
module St = Statistics;;
module IN = W.IntNodeIndex;;
module H = Hashtbl;;
module CP = CompletionProcessx;;
module N = Nodex;;
module NI = Types.NodeTermIndex;;
module Monad = W.Monad;;

(*** OPENS ***************************************************************)
open World;;
open Monad;;

(*** SUBMODULES (2) ******************************************************)

(*** FUNCTIONS ***********************************************************)

let log2 s = W.log 2 s

let by_id i = 
 W.get_node_pool >>= fun c -> 
 return (try Some (IN.find_elt i c.ids) with Not_found -> None)
;;

let id n = W.get_node_pool >>= fun c -> return (IN.find_key n c.ids)

let by_eqn e = 
 W.get_node_pool >>= fun c -> 
 return (try Some (H.find c.equations e) with Not_found -> None)
;;

let node_by_eqn e =
 by_eqn e >>= function
  | Some i -> 
   by_id i >>= 
   (function Some n -> return (Some (i, n)) | None -> return None)
  | None -> 
   return None
;;

let pattern_by_id i =
 W.get_iso_context >>= fun c ->
 return (try Some (H.find c.node_classes i) with Not_found -> None)
;;

let store_pattern id n =
 RenamingClass.class_of_eqn (N.equation n) >>= fun c ->
 W.get_iso_context >>= fun cxt ->
 H.add cxt.node_classes id c;
 return ()
;;

let store n =
 St.inc_n_nodes >>
 W.get_node_pool >>= fun c ->
 let i, ids' = IN.fresh c.ids in
 let ids' = IN.add i n ids' in
 H.add c.equations (N.equation n) i;
 W.set_node_pool {c with ids = ids'} >>
 store_pattern i n
;;

let restorem f i =
 by_id i >>= function
 | Some  n ->
  W.get_node_pool >>= fun c ->
  f n >>= fun n' ->
  let ids' = IN.replace i n' c.ids in
  W.set_node_pool {c with ids = ids'}
 | None -> failwith "no id in indexed node restore"
;;

let restore f = restorem (return <.> f)

let create_axiom e = N.of_axiom e >>= fun n -> store n >> return n

let create_preoriented_axiom r = 
 N.of_rule r >>= fun n -> store n >> return n
;;

let of_axiom = create_axiom >=> id

(* terms assumed to be already normalized *)
let check_create s t r0 r1 e c0 c1 p =
 let eqn = Equation.of_terms s t in
  node_by_eqn eqn >>= function 
   | Some (i, n) ->
    let flip = Term.equal (fst (N.data n)) s in
    let (r0,r1,e,c0,c1) = if flip then r1,r0,e,c1,c0 else r0,r1,e,c0,c1 in
    let merge = N.merge r0 r1 e c0 c1 p in
    restorem merge i >> return i
   | None ->
    let s, t = Equation.terms eqn in
    N.create s t r0 r1 e p >>= fun n ->
    store n >> id n
;;

(* terms assumed to be already normalized *)
let create s t r0 r1 e p = check_create s t r0 r1 e [] [] [p]

let create_constraint rule c0 c1 = 
 check_create (Rule.lhs rule) (Rule.rhs rule) [] [] [] c0 c1 []
;;

let split s i = restore (N.map (CP.apply_split s)) i;;

let check_node pred s t r0 r1 e =
 node_by_eqn (Equation.of_terms s t) >>= function
  | Some (_,n) -> return (pred n r0 r1 e)
  | None -> return false
;;

(* Checks whether there exists a node n which subsumes a possible new node 
  with data s, t and labels r0, r1, e. Due to its usage upon creation of a 
  new node, c0 and c1 are not checked. (s, t) need not be normalized! *)
let is_subsumed = check_node N.subsumes

(* Tries to check whether there exists a node n having data s,t such that
 the processes in the given label sets are already dealt with.
 The criterion is used when a node is to be generated; here we check 
 whether r0, r1 and e are already contained in the node's constraints.
 However, a node might (by rewriting) already be done with although
 the following function does not recoginze it as such. (Not a problem,
 as only used as an optimisation) *)
let is_done_with = check_node N.is_done_with

let considered_for u v =
 node_by_eqn (Equation.of_terms u v) >>= function
  | Some (_,n) -> return (N.considered_for n)
  | None -> return []
;;

(* check whether node need not be created ( in rewrite), i.e.,
   either has equal terms or is subsumed or is done with *)
let is_not_necessary s t r0 r1 e =
 if Term.compare s t = 0 then return true else
 (is_subsumed s t r0 r1 e >>= fun b1 -> (
 if b1 then return true else is_done_with s t r0 r1 e))
;;

(* wrappers for node functions so that they can be called with ints *)
let id_lift f i = 
 by_id i >>= function 
  | Some n -> return (f n)
  | None -> failwith "no id in id_lift found"
;;

let id_liftm f i = 
 by_id i >>= function
  | Some x -> f x
  | None -> failwith "no id in id_liftm found"

let eo = id_lift N.eo;;
let ec = id_lift N.ec;;
let e_all = id_lift N.e_all;;
let r0o = id_lift N.r0o;;
let r1o = id_lift N.r1o;;

let reo = id_lift N.reo

let data = id_lift N.data

let parents = id_lift N.parents

let constraints = id_lift N.constraints

let brule b = id_lift (N.brule b)

let brc b = id_lift (N.brc b)

let brcec b = id_lift (N.brcec b)

let bcontent b = id_lift (N.bcontent b)

let to_string i = 
 (id_lift N.to_string i) >>= fun s ->
 return (s^" ("^(string_of_int i)^")")
;;

let to_stringm i =
 (id_liftm N.to_stringm i) >>= fun s ->
 return (s^" ("^(string_of_int i)^")")
;;

let close = restorem N.close 

let move_to_r_and_c ps = restorem (N.move_to_r_and_c ps)
  
let bupdate b n r0 r1 e = restorem (fun n -> N.bupdate b n r0 r1 e) n

let has_non_empty_labels = id_lift N.has_non_empty_labels

let has_er_labels uo = id_lift (N.has_er_labels uo)

let contains_rule_labels = id_lift N.contains_rule_labels

let er_contains_closed p = id_lift (N.er_contains_closed p)

let sum_size = id_lift (N.sum_size)

let max_size = id_lift (N.max_size)
  
let restrict_to_process p = restore (N.restrict_to_process p)

let remove_processes ps = restore (N.remove_processes ps)

