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

(** Main control for (o)MKBtt
 @author Sarah Winkler
 @since  2010/11/15 *)

(* This module contains the main control loop for (ordered or standard) 
   multicompletion, and some auxiliary functions.  *)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module C = Completion;;
module W = World;;
module M = W.Monad;;
module St = Statistics;;
module CP = CompletionProcessx;;
module N = IndexedNode;;
module NS = NodeState;;
module NI = NodeTermIndex;;
module NSA = NodeSetAux;;
module SS = SelectionStrategy;;

module Option = Util.Option;;

(*** OPENS (2) ***********************************************************)
(*open Monad;;*)
let (>>=) = M.(>>=);;
let (>>) = M.(>>);;
let return = M.return;;
let map = M.map;;

open World;;
(*** TYPES ***************************************************************)
type success_type = Proof | Saturation

(*** FUNCTIONS ***********************************************************)

let log1 s = (*Format.printf "%s\n" s; *) W.log 1 s

let log2 s = (*Format.printf "%s\n" s; *) W.log 2 s

let log_iteration i = 
 log1 ("Starting iteration "^(string_of_int i)) >>
 NS.open_nodes >>= IndexedSet.to_stringm >>= fun  os ->
 NS.closed_nodes >>= IndexedSet.to_stringm >>= fun  cs ->
 log1 ("Open nodes: \n"^os^"\nClosed nodes:\n"^cs)
;;

let log_choose n = 
 N.to_stringm n >>= fun ns -> 
 log1 ("CHOOSE "^ns)
;;

let log_orient n = N.to_stringm n >>= fun ns -> log2 (ns^" oriented")

let log_deduce ns =
 IndexedSet.to_stringm ns >>= fun s -> 
 log2 ("After deduce & rewrite: "^s) (*>>
 NI.log_index_state*)
;;

let union = List.union

(* ------------------------------------------------------------------ *)
(*  CHECK SUCCESS                                                     *)
(* ------------------------------------------------------------------ *)

let szs_proof = (*function
 | Proof -> ( *)
  W.get_options >>= fun o -> match Option.the (C.conjecture o) with
  | C.Conjecture C.Equality -> failwith "unknown SZS status"
  | C.Conjecture C.Disequality -> failwith "unknown SZS status"
  | C.NegatedConjecture C.Equality -> return C.Uns (* COL059-1 *)
  | C.NegatedConjecture C.Disequality -> return C.Uns
  (* )
 | Saturation -> (
  St.options >>= fun o -> match C.conjecture o with
  | C.Conjecture C.Equality -> failwith "unknown SZS status"
  | C.Conjecture C.Disequality -> failwith "unknown SZS status"
  | C.NegatedConjecture C.Equality -> return C.Noc
  | C.NegatedConjecture C.Disequality -> return C.Sat
  )*)
;;

let success i =
 (*log2 "check completion finished" >>*)
 MKBtt.unfinished_processes >>= fun uc ->
 CP.complement uc >>= function
 | [] -> 
  NS.non_empty_open_nodes_exist >>= fun b ->
  return (if b then None else Some (C.Gup "Completion failed.", None))
 | p :: _ ->
  NS.all_nodes >>= NSA.project_c p >>= fun c ->
  W.M.Trsx.to_wst_stringm c >>= fun s ->
  log1 ("Constraints for winner "^(CP.to_string p)^": "^s) >>
  log1 ("successful process "^(CP.to_string p)) >>
  return (Some (C.Sat, Some p))
;;

(* ------------------------------------------------------------------ *)
(*  CHOICE CONTROL                                                    *)
(* ------------------------------------------------------------------ *)

let choose i =
 W.get_options >>= fun o ->
 let tstart = Unix.gettimeofday() in
 let sm = C.selection_macro o in
 let choose = 
  (*if sm = Some(SS.Old) then
   Choose.best_m
  else*) if sm = Some(SS.JapanSum) || sm = Some(SS.JapanMax) then
   ChooseHaruhiko.bestl ()
  else
   SelectionStrategyAux.apply (C.selection_strategy o)
 in
 choose >>= fun n ->
 St.add_t_selection (Unix.gettimeofday () -. tstart) >>
 W.get_node_state >>= fun c ->
 W.set_node_state {c with current = [n]} >>
 return n
;;

(* ------------------------------------------------------------------ *)
(*  REMOVAL OF BAD PROCESSES                                          *)
(* ------------------------------------------------------------------ *)
let remove_process =
 St.n_processes >>= fun n ->
 W.get_options >>= fun o ->
 let thresh = C.kill_processes o in
 if (n > 35) && ((thresh == 0.0) || (thresh > 2.0)) then
  (Format.printf "kill\n%!"; W.set_options (C.set_kill o (if n > 90 then 1.6 else 1.7)))
 else return () >>
 if thresh > 0.0 then
  let tstart = Unix.gettimeofday() in
  ChooseHaruhiko.bad_processes o >>= fun ps ->
  NS.remove_processes ps >>
  St.add_n_victims (List.length ps) >>
  St.add_t_process_kill (Unix.gettimeofday () -. tstart) >>
  return ()
 else
  return ()
;;


(* ------------------------------------------------------------------ *)
(*  CONTROL LOOP                                                      *)
(* ------------------------------------------------------------------ *)

let id = return ()

let rewrite_open_with_new_node n =
 NS.open_nodes >>= fun no ->
 MKBtt.rewrite_open_with no n >>= fun no' ->
 MKBtt.rewrite_open no' >>= fun no'' ->
 NS.add_open (union no' no'') >>
 NS.open_nodes >>= MKBtt.gc >>= NS.set_open
;;


let rec iterate i =
 St.inc_n_iterations >>
 log_iteration i >>
 success i >>= function
  | Some r -> return r
  | None ->
 remove_process >>
 choose i >>= fun n -> NS.remove_open n >>
 log_choose n >>
 (*log2 "REWRITE OPEN" >>*)
 MKBtt.rewrite_open [n] >>= NS.add_open >>
 (*N.has_non_empty_labels n >>= fun not_superfluous -> *)
 N.has_non_empty_unprotected_open_labels n >>= fun not_superfluous ->
 if not_superfluous then
  MKBtt.orient n >>
  log_orient n >>
  N.contains_rule_labels n >>= fun is_orientable ->
  if is_orientable then
(*   log2 "REWRITE CLOSED" >>*)
   NS.closed_nodes >>= fun nc -> 
   MKBtt.rewrite_closed_with nc n >>= fun no' ->
   MKBtt.rewrite_open no' >>= fun no'' ->
   MKBtt.gc (union no' no'') >>= NS.add_open >> MKBtt.gc nc >> (* ! *)
   NS.index_add_closed_node n >>
   (* add extended rules to set of extended rules *)
   N.extend n >>= NS.index_add_extended >>
   log2 "DEDUCE" >>
   MKBtt.deduce_rewrite n >>= MKBtt.gc >>= fun ns -> 
   NS.add_open ns >>
   (*log_deduce ns >>*)
   log2 "REWRITE OPEN" >>
   rewrite_open_with_new_node n >>
   (* *)
   iterate (i+1)
  else
   NS.index_add_closed_node n >> iterate (i+1)
 else iterate (i+1)
;;

(* ------------------------------------------------------------------ *)
(*  STARTING COMPLETION                                               *)
(* ------------------------------------------------------------------ *)

let complete eqs = 
 NSA.of_axioms eqs >>= NS.add_open >>
 NSA.of_s_theory >>= NS.set_s_theory >>
 NS.s_nodes >>= IndexedSet.to_stringm >>= fun ns ->
 log1 ("S-nodes: \n"^ns) >>
 iterate 0
;;



