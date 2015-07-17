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
module NI = World.M.ACDiscTree;;
module Monad = W.Monad;;

(*** OPENS ***************************************************************)
open Monad;;
open World;;

(*** TYPES ***************************************************************)
type entry = int * bool

type t = entry World.M.ACDiscTree.t

(*** FUNCTIONS ***********************************************************)

let log1 = W.log 1

let log2 s = ((*Format.printf "%s\n" s;*) W.log 2 s)

let log_index n b = 
 N.to_stringm n >>= fun ns -> 
 let bs = if b then "true" else "false" in
 Format.printf "%s with %s indexed" ns bs;
 log2 (ns^" with "^bs^" indexed")
;;

let empty = NI.empty

let update f =
 W.get_node_state >>= fun c ->
 let ix = c.term_index in
 f ix >>= fun ix' ->
 W.set_node_state {c with term_index = ix'}
;;

let of_r f = 
 W.get_node_state >>= fun c -> f c.term_index
;;

let get_index = 
 W.get_node_state >>= fun c -> return c.term_index
;;

let make = return (NI.empty)

let take_time f log =
  let t_start = Unix.gettimeofday () in
  f >>= fun res ->
  log (Unix.gettimeofday () -. t_start) >>
  return res
;;

let insert v = 
 take_time (update (NI.insert v)) St.add_t_insert
;;

(*
let delete v = 
 take_time (update (NI.delete v)) St.add_t_delete
;;
*)
  
let variants t = 
 take_time (of_r (NI.variants t)) St.add_t_variants
;;

let variants_in i t = 
 let vc = NI.variants t i in
 take_time vc St.add_t_variants
;;

let encompassments t =
 take_time (of_r (NI.encompassments t)) St.add_t_encs
;;

let encompassments_in i t =
 let ec = NI.encompassments t i in
 take_time ec St.add_t_encs
;;

let encompassments_below_root t =
 take_time (of_r (NI.encompassments_below_root t)) St.add_t_encs
;;

(* to deal with rewrite indexes containing a single node *)
let empty = return NI.empty

let insert_one idx x = 
 take_time (NI.insert x idx) St.add_t_insert
;;

let id = return () 

let indexing_required t s (i,b) = 
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
 log2 ("Indexed "^(string_of_int n)^" true: "^ss^", indexed false"^ts) >>*)
 let ifdo bm m = bm >>= fun b -> if b then m else id in
 (ifdo (indexing_required s t (n,true)) (insert (s,(n, true)))) >>
 ifdo (indexing_required t s (n,false)) (insert (t,(n, false)))
;;

let single_index i =
 let add (t,v) s idx =
  indexing_required t s v >>= fun b ->  
  if b then insert_one idx (t,v)
  else return idx
 in
 N.data i >>= fun (s, t) ->
 empty >>=
 add (s,(i, true)) t >>= add (t,(i, false)) s
;;

let s_insert x =
 W.get_node_state >>= fun c ->
 let ix = c.sterm_index in
 NI.insert x ix >>= fun ix' ->
 W.set_node_state {c with sterm_index = ix'}
;;

let add_s_node n =
 N.brule true n >>= fun rule ->
 let s, t = Rule.lhs rule, Rule.rhs rule in
 (*W.M.Term.to_stringm s >>= fun ss ->
 W.M.Term.to_stringm t >>= fun ts ->
 log2 ("Indexed "^(string_of_int n)^" true: "^ss^", indexed false"^ts) >>*)
 let ifdo bm m = bm >>= fun b -> if b then m else id in
 (ifdo (indexing_required s t (n,true)) (s_insert (s,(n, true)))) >>
 ifdo (indexing_required t s (n,false)) (s_insert (t,(n, false)))
;;

let s_encompassments t =
 W.get_node_state >>= fun c ->
 NI.encompassments t (c.sterm_index)
;;

(*
let remove_node b n =
 N.brule b n >>= fun rule ->
 let t = Rule.lhs rule in
 W.M.Term.to_stringm t >>= fun st ->
 (*Format.printf "Remove %s associated with (%i,%s)" st n
  (if b then "true" else "false");*)
 delete (t,(n,b))
;;*)
