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

(** Control for node choice (first implementation).
  @author Sarah Winkler
  @since  2007/04/30 *)

(* Module provides functions to control the choice of a process/node from 
   the current node set. Corresponds to strategy mkbtt1 used in old 
   implementation. *)
    

(*** SUBMODULES **********************************************************)
module Pos = Position;;
module T = Term;;
module Fun = FunctionSymbol;;
module Var = Variable;;
module Sub = Substitution;;
module NodeSet = SomeSet.Make(Node);;
module CProcess = CompletionProcess;;
module St = Statistics;;

(*** TYPES ***************************************************************)

(*** EXCEPTIONS **********************************************************)

(*** GLOBALS *************************************************************)

(*** FUNCTIONS ***********************************************************)

(* selects node with minimal termsize *)
let fold_min_termsize n (min, set) =
 let s = Node.terms_size n in
 if s < min then (s, NodeSet.singleton n)
 else if s == min then (min, NodeSet.add n set)
 else (min, set)
;;

(* selects node with minimal termsize or oneway orientation *)
let fold_min_termsize_or_oneway n (min, b, set) =
 let s1 = Node.is_oneway n in
  let s = Node.terms_size n in
 if (not s1) && b then (min, true, set) else
 if s1 && (not b) then (s, true, NodeSet.singleton n) else
  if s < min then (s, b, NodeSet.singleton n)
  else if s == min then (min, b, NodeSet.add n set)
   else (min, b, set)
;;

let fold_min_termsize_tolerance t n (min, set) =
 let s = Node.terms_size n in
 if s < min then (s, NodeSet.singleton n)
  else let thresh = min + (truncate ((float_of_int min) *. t)) in
   if s <= thresh then (min, NodeSet.add n set)
   else (min, set)
;;

let fold_max_age n (min, node) =
 let s = Node.stamp n in
 if s < min then (s, n)
 else (min, node)
;;

let fold_deterministic n node =
 if (Node.compare n node) < 0 then n else node
;;

let fold_max_processes n (max, set) =
 let m = CProcess.cardinal (Node.open_labels n) in
 if m > max then (m, NodeSet.singleton n)
 else if m == max then (max, NodeSet.add n set)
 else (max, set)
;;

let fewest_rules no nc =
 let r = Setx.choose !CProcess.all_processes in
 let (pmin, _ ) =
  Setx.fold
   (fun p (pm, m) ->
     let c = NodeSetAux.count nc p in
     if c < m then (p, c) else (pm, m)
   )
  !CProcess.all_processes
  (r, NodeSetAux.count nc r)
 in
  CProcess.singleton pmin
;;


let fewest_rules no nc =
 let r = Setx.choose !CProcess.all_processes in
 let (pmin, _ ) =
  Setx.fold
   (fun p (pm, m) ->
     let c = NodeSetAux.count nc p in
     if c < m then (p, c) else (pm, m)
   )
  !CProcess.all_processes
  (r, NodeSetAux.count nc r)
 in
  CProcess.singleton pmin
;;

let heuristic1 ns nsc =
  let e = NodeSet.empty in
  let _, ns2 = NodeSet.fold (fold_min_termsize_tolerance 0.0) ns (100, e) in
  let _, ns3 = NodeSet.fold fold_max_processes ns2 (0, e) in
  let n = NodeSet.choose ns3 in
 (* new *)  
  let _, n' = 
   NodeSet.fold fold_max_age ns3 (!(Statistics.node_count), n) 
  in
(* just for debugging, to obtain deterministic choice *)
  (*NodeSet.fold fold_deterministic ns3 *) 
 n'
;;

let possibly_isomorphic options n =
 let rule = Node.true_rule n in
 match Completion.check_isomorphism options with
  | Completion.NoChecks -> false
  | Completion.Renamings -> 
   begin
    match Node.is_symmetric n with
     | None -> 
      let i = TrsRenaming.rule_symmetric rule in
      ignore (Node.update_symmetric n i);
     (* if i then Format.printf "Node %s is symmetric\n" (Node.to_string n);*)
      i
     | Some b -> b
   end
  | Completion.Permutations -> 
   begin
    match Node.is_symmetric n with
     | None ->
      let i = TermPermutation.rule_symmetric rule in
      ignore (Node.update_symmetric n i);
      i
     | Some b -> b
   end
;;


let heuristic2 ns nsc options =
  let e = NodeSet.empty in
  let ns' = NodeSet.filter (possibly_isomorphic options) ns in
  let ns = if NodeSet.is_empty ns' then ns else ns' in
  let _, ns2 = NodeSet.fold fold_min_termsize ns (100, e) in
(*  let ns2' = NodeSet.filter Node.is_oneway ns2 in
  let ns2 = if NodeSet.is_empty ns2' then ns2 else ns2' in
  let ns2' = NodeSet.filter (possibly_isomorphic options) ns2 in
  let ns2 = if NodeSet.is_empty ns2' then ns2 else ns2' in*)
  let _, ns3 = NodeSet.fold fold_max_processes ns2 (0, e) in
  NodeSet.choose ns3
;;

let heuristic3 ns nsc options =
  let e = NodeSet.empty in
  let agetosize = 0.05 in
  let r = Random.float 1.0 in
  if r < agetosize then
   let n = NodeSet.choose ns in
   snd (NodeSet.fold (fold_max_age) ns (Node.stamp n, n))
  else (
   let ns2 = 
    snd (NodeSet.fold (fold_min_termsize_tolerance 0.0) ns (100, e))
   in
   let _, ns3 = NodeSet.fold fold_max_processes ns2 (0, e) in
   NodeSet.choose ns3)
;;



let delete_processes ps ns nsc =
 let gamma = Hashtbl.create 5 in
 CProcess.iter
  (fun p ->
   CProcess.delete p;
   Hashtbl.replace gamma p (CProcess.empty ());
  ) ps;
 let nso' = NodeSet.apply_split ns gamma in
 let nsc' = NodeSet.apply_split nsc gamma in
 (nso', nsc')
;;

(* choose from open nodes ns. ns is assumed non-empty! *)
let rec best_m ns nsc options = 
  let ps = CProcess.fewest_rules () in
  CProcess.inc_choice_counts ps; (* this line is pure magic *)
  let ns' = NodeSet.filter (Node.contains_processes_open ps) ns in
   if NodeSet.is_empty ns' then
    let nso', nsc' = delete_processes ps ns nsc in
    best_m nso' nsc' options
   else
    heuristic1 ns' nsc (*options*) 
;;

(* choice function for KBtt/oKBtt *)
let fold_min_cost s (cmin, smin) =
 if (State.cost_value s < cmin) then 
  (State.cost_value s, s) 
 else 
  (cmin, smin)
;;

let best_s pset = 
 let _, min = 
  Setx.fold fold_min_cost pset (100000, Setx.choose pset)
 in min
;;
