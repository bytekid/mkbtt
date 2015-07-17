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

(** Critical pair criterion PCP
@author Sarah Winkler
@since  2009/11/06
*)
(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module C = Completion;;
module CP = CompletionProcessx;;
module Term = U.Term;;
module Rule = U.Rule;;
module E = U.Elogic;;
module Pos = Rewriting.Position;;
module N = IndexedNode;;
module NI = NodeTermIndexx;;
module CC = CPCCache;;
module W  = World;;
module Monad = W.Monad;;

(*** OPENS (2) ***********************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

(* outer rule rl1 already instantiated *)
let term (u,(_,p,_),_) = Term.subterm p u

let lookup_redundant_processes o = 
 CC.lookup_redundant_processes_for_term (term o)
;;

let matches (i,b) t =
 W.get_options >>= fun o ->
 if C.perfect_filtering o then return true 
 else
  N.brule b i >>= fun rl ->
  return (E.matches t (Rule.lhs rl))
;;

let remove_rule_label t psm ((i, b), p) =
 psm >>= fun ps ->
 (*N.brule b i >>= W.M.Rule.to_stringm >>= fun rs ->
 Format.printf "Prime: %s applies at %s for %s\n%!" rs (Pos.to_string p) (CP.set_to_string ps);*)
 matches (i,b) (Term.subterm p t) >>= fun is_matching ->
 if is_matching then
  N.brc b i >>= fun ps' -> return (List.diff ps ps')
 else 
  return ps
;;

let remove_processes_that_rewrite_below_root t pset =
 NI.encompassment_candidates_below_root t >>= (*fun nodes ->
 filter (CC.is_encompassment t) nodes >>=*) fun matches ->
 List.fold_left (remove_rule_label t) (return pset) matches
;;

(* filters  processes out of process set for which this cp is redundant.
   rl1 is already instantiated. *)
let compute_nonredundant overlap pset =
 remove_processes_that_rewrite_below_root (term overlap) pset
;;

(* looks up redundant processes in hashtable, and reduced different set
   if entry was found for this overlap. otherwise, redundnats are
   computed and stored. note that due to caching, fewer redundants might
   be returned than actually possible *)
let filter_nonredundant overlap process_set =
 CC.caching_active >>= fun cache ->
 if cache then (
   lookup_redundant_processes overlap >>= function
  | None ->
    compute_nonredundant overlap process_set >>= fun nonred ->
    let red = CP.diff process_set nonred in 
    if not (CP.is_empty red) then 
     CC.store_redundant_for_term (term overlap) red >> return nonred
    else 
     return nonred
  | Some red -> return (CP.diff process_set red))
 else
  compute_nonredundant overlap process_set
;;
