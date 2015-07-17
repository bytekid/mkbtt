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
module Pos = Rewriting.Position;;
module C = Completion;;
module St = Statistics;;
module CP = CompletionProcessx;;
module T = U.Term;;
module Tx = Termx;;
module NS = NodeState;;
module N = IndexedNode;;
module NI = NodeTermIndex;;
module O = Types.Overlap;;
module W  = World;;
module Monad = W.Monad;;

(*** OPENS (2) ***********************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

let processes_that_dont_rewrite_below_root t ps =
 let rewrite_with ps ((rn,rb),p) =
  if List.is_empty ps then return [] 
  else
   N.brule rb rn >>= fun rule -> N.brc rb rn >>= fun r ->
   let is = List.intersect ps r in
   W.M.Term.to_stringm t >>= fun ts ->
   W.M.Rule.to_stringm rule >>= fun rs ->
   let pos = ACPosition.to_string p in
   if List.is_empty is then 
    return ps
   else
    (W.M.ACRewrite.reducible_at t p rule >>= fun is_reducible ->
    (*if is_reducible then Format.printf "%s reducible with %s at %s\n%!" ts rs pos;*)
    return (if is_reducible then (List.diff ps r) else ps))
 in
 W.M.Termx.funs_pos t >>= fun pos ->
 NI.encompassments_below_root (t,pos) >>= fun rulenodes ->
 foldl rewrite_with ps rulenodes
;;

(* filters  processes out of process set for which this cp is redundant.
   rl1 is already instantiated. *)
let compute_nonredundant = processes_that_dont_rewrite_below_root

(* looks up redundant processes in hashtable, and reduced different set
   if entry was found for this overlap. otherwise, redundnats are
   computed and stored. note that due to caching, fewer redundants might
   be returned than actually possible *)
let filter_nonredundant o _ ps =
  let t = 
  try
   Tx.subterm (O.source o) (O.position o) 
  with Failure _ ->
   failwith "Prime: pos does not exist"
  in
  (*W.M.Term.to_stringm (O.source o) >>= fun ss ->
  W.M.Term.to_stringm t >>= fun ts ->
  Format.printf "Check subterm %s of %s\n%!" ts ss;*)
  compute_nonredundant t ps
;;
