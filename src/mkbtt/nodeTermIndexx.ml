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

(** Wrapper for module IndexWrapper parameterized by NodeEntry,
    and state monad holding respective object.
 @author Sarah Winkler
 @since  2010/10/12 *)

(*** OPENS ***************************************************************)
open Util;;

(*** SUBMODULES (1) ******************************************************)
module Rewriting = Processors.Rewritingx;;
module Term = U.Term;;
module Rule = U.Rule;;
module W = World;;
module St = Statistics;;
module N = IndexedNode;;
module NI = Types.NodeTermIndex;;
module Monad = W.Monad;;

(*** OPENS ***************************************************************)
open Monad;;
open World;;

(*** FUNCTIONS ***********************************************************)

let log1 = W.log 1

let log2 s = ((*Format.printf "%s\n" s;*) W.log 2 s)

let log_index_state =
 W.get_node_state >>= fun c ->
 let rx, ux = c.term_index in
 W.Monad.liftm (NI.is_empty rx) >>= fun re ->
 W.Monad.liftm (NI.is_empty ux) >>= fun ue ->
 log2 ("Rewrite index empty: "^(if re then "true" else "false")) >>
 log2 ("Unif index empty: "^(if ue then "true" else "false"))
;;

let log_index n b = 
 N.to_stringm n >>= fun ns -> 
 let bs = if b then "true" else "false" in
 Format.printf "%s with %s indexed" ns bs;
 log2 (ns^" with "^bs^" indexed")
;;

let update_both f =
 W.get_node_state >>= fun c ->
 let rx, ux = c.term_index in
 W.Monad.liftm (f rx) >>= fun rx' ->
 W.Monad.liftm (f ux) >>= fun ux' ->
 W.set_node_state {c with term_index = (rx',ux')}
;;

let of_r f = 
 W.get_node_state >>= fun c -> liftm (f (fst c.term_index))
;;

let of_u f = 
 W.get_node_state >>= fun c -> liftm (f (snd c.term_index))
;;

let rewrite_index = 
 W.get_node_state >>= fun c -> return (fst c.term_index)
;;

let make = W.get_options >>= fun o -> return (NI.make o)

let take_time f log =
  let t_start = Unix.gettimeofday () in
  f >>= fun res ->
  log (Unix.gettimeofday () -. t_start) >>
  return res
;;


let insert v = 
 take_time (update_both (NI.insert v)) St.add_t_insert
;;
(*
let insert (t,node) =
 W.M.Termx.fresh_vars t >>= fun t' ->
 take_time (update_both (NI.insert (t',node))) St.add_t_insert
;;
*)

let delete v = 
 take_time (update_both (NI.delete v)) St.add_t_delete
;;
  
let variant_candidates t = 
 take_time (of_r (NI.variant_candidates t)) St.add_t_variants
;;

let variant_candidates_in i t = 
 let vc = W.Monad.liftm (NI.variant_candidates t i) in
 take_time vc St.add_t_variants
;;

let generalization_candidates t = of_r (NI.generalization_candidates t)

let unification_candidates t = of_u (NI.unification_candidates t)

let encompassment_candidates t =
 take_time (of_r (NI.encompassment_candidates t)) St.add_t_encs
;;

let encompassment_candidates_in i t =
 let ec = W.Monad.liftm (NI.encompassment_candidates t i) in
 take_time ec St.add_t_encs
;;

let encompassment_candidates_below_root t =
 take_time (of_r (NI.encompassment_candidates_below_root t)) St.add_t_encs
;;

let overlap1_candidates_below_root t =
 take_time (of_u (NI.overlap1_candidates_below_root t)) St.add_t_overlaps1
;;

let overlap1_candidates t = 
 take_time (of_u (NI.overlap1_candidates t)) St.add_t_overlaps1

let overlap2_candidates t = 
 take_time (of_u (NI.overlap2_candidates t)) St.add_t_overlaps2

(* to deal with rewrite indexes containing a single node *)
let empty_rindex = W.get_options >>= fun o -> return (fst (NI.make o))


let insert_one idx x = 
 take_time (W.Monad.liftm (NI.insert x idx)) St.add_t_insert
;;

(*
let insert_one idx (t,node) =
 W.M.Termx.fresh_vars t >>= fun t' ->
 take_time (W.Monad.liftm (NI.insert (t',node) idx)) St.add_t_insert
;;*)

let id = return () 

let indexing_required t s (i,b) = 
 W.get_options >>= fun o ->
 if Completion.is_ordered o then
  return (not (Term.is_var t))
 else
  let directable = Rule.is_rewrite_rule (Rule.of_terms t s) in
  N.brc b i >>= fun rs ->
  let usable =  not (List.is_empty rs) in
  return (directable && usable)
;;

let add_node n =
 N.brule true n >>= fun rule ->
 let s, t = Rule.lhs rule, Rule.rhs rule in
 (*W.M.Term.to_stringm s >>= fun ss ->
 W.M.Term.to_stringm t >>= fun ts ->
 Format.printf "Indexed %i true: %s, indexed false %s\n%!" n ss ts;*)
 let ifdo bm m = bm >>= fun b -> if b then m else id in
 (ifdo (indexing_required s t (n,true)) (insert (s,(n, true)))) >>
 ifdo (indexing_required t s (n,false)) (insert (t,(n, false)))
;;

let remove_node b n =
 N.brule b n >>= fun rule ->
 let t = Rule.lhs rule in
 (*W.M.Term.to_stringm t >>= fun st ->
 Format.printf "Remove %s associated with (%i,%s)\n%!" st n
  (if b then "true" else "false");*)
 delete (t,(n,b))
;;
