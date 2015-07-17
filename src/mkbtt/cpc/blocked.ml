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
(** Critical pair criterion BCP
@author Sarah Winkler
@since  2009/11/06
*)
(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module E = U.Elogic;;
module C = Completion;;
module CP = CompletionProcessx;;
module Term = U.Term;;
module Rule = U.Rule;;
module Sub = U.Substitution;;
module N = IndexedNode;;
module NI = NodeTermIndexx;;
module CC = CPCCache;;
module W  = World;;
module Monad = W.Monad;;

(*** FUNCTIONS ***********************************************************)
let (>>=) = Monad.(>>=)
let (>>) = Monad.(>>)
let return = Monad.return

(* returns set of processes for which this overlap was already
recognized as redundant *)
let lookup_redundant_processes (_, _, sigma) =
 let f _ t psm =
  psm >>= fun ps ->
  CC.lookup_redundant_processes_for_term t >>= function
   | None -> return ps
   | Some  ps' -> return (CP.union ps ps')
 in
 Sub.fold f sigma (return []) >>= fun stored_result ->
 if List.is_empty stored_result then raise Not_found 
 else return stored_result
;;

let matches (i,b) t =
 W.get_options >>= fun o ->
 if C.perfect_filtering o then return true
 else
  N.brule b i >>= fun rl ->
  return (E.matches t (Rule.lhs rl))
;;

(***** compute processes for which equation is redundant *****)
(* get l1 or l2, according to b *)
let add_rule_label t psm ((i, b), _) = 
 psm >>= fun ps -> 
 matches (i,b) t >>= fun is_matching ->
 if is_matching then
  N.brc b i >>= fun ps' -> return (List.union ps ps')
 else
  return ps
;;

(* used for blocked criterion to check whether t=x\sigma can be reduced
   for some rule's variable x. Due to nonlinearity in indexing, have
   to check whether retrieved encompassment is really a match *)
let get_processes_that_rewrite t =
 NI.encompassment_candidates t >>= (*fun nodes ->
 filter (CC.is_encompassment t) nodes >>=*) fun matches ->
 List.fold_left (add_rule_label t) (return []) matches >>= fun res ->
 (* store this result for the term *)
 CC.caching_active >>= fun cache ->
 if cache then
  CC.store_redundant_for_term t res >> return res
 else
  (return res)
;;

(* union over all processes that can rewrite some x\sigma *)
let compute_redundant (_, _, sigma) =
 let add _ t psm =
  psm >>= fun ps -> 
  match t with
   | Term.Var _ -> psm
   | _ -> get_processes_that_rewrite t >>= fun ps' ->
    return (CP.union ps ps')
 in
 Sub.fold add sigma (return [])
;;

(* looks up redundant processes in hashtable, and reduced different set
   if entry was found for this overlap. otherwise, redundnats are
   computed and stored. note that due to caching, fewer redundants might
   be returned than actually possible *)
let remaining overlap process_set =
 compute_redundant overlap >>= fun redundants ->
 return (CP.diff process_set redundants)
;;

let filter_nonredundant overlap process_set =
 CC.caching_active >>= fun cache ->
 if cache then
  try 
    lookup_redundant_processes overlap >>= fun red ->
    return (CP.diff process_set red)
   with Not_found -> remaining overlap process_set
 else remaining overlap process_set
;;
