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
open Completion;;

(*** SUBMODULES **********************************************************)
module C = Completion;;
module W = World;;
module Monad = W.Monad;;
module St = Statistics;;
module CP = CompletionProcessx;;
module N = IndexedNode;;
module NS = NodeState;;
module NSA = NodeSet;;
module NI = NodeTermIndexx;;
module PI = ProcessIsomorphism;;
module I = Inferences;;
module G = GoalWrapper;;
module SS = SelectionStrategy;;

module O = Util.Option;;

(*** OPENS (2) ***********************************************************)
(*open Monad;;*)
let (>>=) = Monad.(>>=);;
let (>>) = Monad.(>>);;
let return = Monad.return;;
let map = Monad.map;;

(*** TYPES ***************************************************************)
type success_type = Proof | Saturation

(*** FUNCTIONS ***********************************************************)

let log1 s = W.log 1 s

let log2 s = (*Format.printf "%s\n" s; *)W.log 2 s

let log_iteration i = 
 log1 ("Starting iteration "^(string_of_int i)) >> 
 NS.open_nodes >>= fun no -> 
 NodeSet.to_stringm (List.sort compare no) >>= fun  os ->
 NS.closed_nodes >>= NodeSet.to_stringm >>= fun  cs ->
 log1 ("Open nodes: \n"^os^"\nClosed nodes:\n"^cs) >>
 W.get_options >>= fun o -> 
 if C.has_goal o then G.to_string >>= fun s -> log1 ("Goal:\n"^s)
 else return ()
;;

let log_choose n i = 
 N.data n >>= fun ts ->
 Monad.project W.M.Term.to_stringm ts >>= fun (s,t) -> 
 log1 ((string_of_int i)^": CHOOSE "^s^"="^t)
;;

let log_orient n =  N.to_stringm n >>= fun ns -> log2 (ns^" oriented")

let log_deduce ns =
 NodeSet.to_stringm ns >>= fun s -> 
 log2 ("After deduce & rewrite: "^s) (*>>
 NI.log_index_state*)
;;

let union = List.union

(* ------------------------------------------------------------------ *)
(*  CHECK SUCCESS                                                     *)
(* ------------------------------------------------------------------ *)

let szs_proof = (*function
 | Proof -> ( *)
  W.get_options >>= fun o -> match O.the (C.conjecture o) with
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

let completion_finished i =
 (*log2 "check completion finished" >>*)
 I.unfinished_processes >>= fun uc ->
 CP.complement uc >>= function
 | [] -> 
  NS.non_empty_open_nodes_exist >>= fun b ->
  return (if b then None else Some (C.Gup "Completion failed.", None))
 | p :: _ -> (
  NS.all_nodes >>= NSA.project_c p >>= fun c ->
  W.M.Trsx.to_stringm c >>= fun s ->
  log2 ("Constraints for winner "^(CP.to_string p)^": "^s) >>
  log2 ("successful process "^(CP.to_string p)) >>= fun _ ->
  return (Some (C.Sat, Some (C.MKBttResult p))))
;;

let proof_finished i = 
 G.proven_for >>= function
  | [] ->
   I.unfinished_processes >>= CP.complement >>= ( function 
    | [] -> return None
    | p::_ -> G.decide p >>= fun r -> return (Some(r,Some(C.MKBttResult p))))
  | p::_ -> szs_proof >>= fun s -> return (Some(s,Some(C.MKBttResult p)))
;;

let success i =
 (W.get_options >>= fun o ->
 match C.mode o with
  | C.Completion -> completion_finished i 
  | _ -> proof_finished i ) >>= fun res ->
 return res
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
  else*)
  if sm = Some(SS.JapanSum) || sm = Some(SS.JapanMax) then (
  if o.preoriented then
   ChooseHaruhiko.bestlrel ()
  else 
   ChooseHaruhiko.bestl () )
  else
   SelectionStrategyAux.apply (C.selection_strategy o)
 in
 choose >>= fun n ->
 St.add_t_selection (Unix.gettimeofday () -. tstart) >>
 log_choose n i >>
 return n
;;

(* ------------------------------------------------------------------ *)
(*  ISOMORPHISM CHECKS                                                *)
(* ------------------------------------------------------------------ *)
let remove_isomorphic =
 W.get_options >>= fun o -> 
 if C.check_isomorphisms_repeatedly o then (
  CompletionProcessx.all_processes >>= 
  PI.filter_subsumed_sometimes >>= fun ps ->
  (*log2 ("Remove processes "^(CompletionProcessx.set_to_string ps)) >>*)
  NS.remove_processes ps)
 else
  return ()
;;

(* ------------------------------------------------------------------ *)
(*  REMOVAL OF BAD PROCESSES                                          *)
(* ------------------------------------------------------------------ *)
let remove_process =
 St.n_processes >>= fun n ->
 W.get_options >>= fun o ->
 let thresh = C.kill_processes o in
 if (n > 35) && ((thresh == 0.0) || (thresh > 2.0)) then 
  W.set_options (C.set_kill o (if n > 90 then 1.6 else 1.7)) 
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

let rewrite_open_with_new_node singleindex =
 NS.open_nodes >>= fun no -> 
 MKBtt.rewrite_open_once_with no singleindex >>= fun no' ->
 MKBtt.rewrite_open no' >>= fun no'' ->
 NS.add_open (union no' no'') >>
 NS.open_nodes >>= MKBtt.gc >>= NS.set_open
;;

let rec iterate i o =
 St.inc_n_iterations  >>
 log_iteration i >>
 success i >>= function
  | Some r -> return r
  | None ->
 remove_isomorphic >>
 remove_process >>
 choose i >>= fun n -> 
 NS.remove_open n >> 
 MKBtt.rewrite_open [n] >>= NS.add_open >>
 N.has_non_empty_labels n >>= fun not_superfluous ->
 (if not_superfluous then
  MKBtt.orient n >> 
  log_orient n >>
  N.contains_rule_labels n >>= fun is_orientable ->
  if is_orientable || (C.is_ordered o) then
   NS.closed_nodes >>= fun nc -> 
   NS.single_rindex n >>= fun idx ->
   MKBtt.rewrite_closed_once_with nc idx >>= fun no' ->
   MKBtt.rewrite_open no' >>= fun no'' ->
   MKBtt.gc (union no' no'') >>= NS.add_open >> MKBtt.gc nc >> (* ! *)
   G.rewrite n idx >>
   NS.index_add_closed_node n >>
   I.deduce_rewrite n >>= MKBtt.gc >>= fun ns -> 
   (*log_deduce ns >>*)
   NS.add_open ns >>
   (* filter nopen here? yes - e.g. 3.30 much faster. *)
   rewrite_open_with_new_node idx
  else
   NS.index_add_closed_node n
 else return ()) >>
 iterate (i+1) o
;;

(* ------------------------------------------------------------------ *)
(*  STARTING COMPLETION                                               *)
(* ------------------------------------------------------------------ *)

let init ns =
 W.get_options >>= fun o ->
 if C.detect_isomorphism o then
  NodeState.get_projections_with_class CP.initial >>= fun (es,_,_) ->
  PI.isomorphism_possible es >>= fun (possible,rep) ->
  let o' = {o with check_isomorphism_with = possible; 
  check_isomorphisms_repeatedly = rep} in
  W.set_options o'
 else 
  return ()
;;

let complete eqs sct goal = 
 G.set goal >>
 W.get_options >>= fun o ->
 NSA.of_axioms eqs >>= fun is -> 
 World.set_options {o with axiom_ids = is} >>
 NS.add_open is >>
 (if O.is_some sct then (
   let sct = O.the sct in
   Termination.trs_terminates sct >>= fun terminates ->
(*   W.M.Trsx.to_stringm sct >>= fun s ->
   Format.printf "Strict: %s\n%!" s;*)
   if not terminates then 
    failwith "Termination of preoriented rules not verified"
   else NSA.of_axioms_strict sct >>= NS.add_open)
  else return ()) >>
 let t' = Unix.gettimeofday () in
 NS.open_nodes >>= init >>= fun _ ->
 St.add_t_one (Unix.gettimeofday () -. t') >>= fun _ ->
 iterate 0 o
;;

