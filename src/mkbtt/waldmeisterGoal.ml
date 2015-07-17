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

(** Waldmeister-like goal representation
@author Sarah Winkler
@since  2009/07/03 *)

(* Data type and functions related to goal in theorem proving
   This implementation uses just the Waldmeister approach, which maintains
   a set of reducts for the lhs and rhs of the original goal and accepts
   as soon as the intersection of these two sets of reducts is not empty.
   In the oMKBtt setting, process sets have to be associated with each 
   reduct. *)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Fun = Rewriting.Function;;
module T = U.Term;;
module Rule = U.Rule;;
module Sub = U.Substitution;;
module C = Completion;;
module W = World;;
module St = Statistics;;
module Monad = W.Monad;;
module GS = GoalState;;
module WGN = Types.WaldmeisterGoalNode;;
module WN = IndexedWNode;;
module N = IndexedNode;;
module NS = NodeState;;
module NI = NodeTermIndexx;;
module CP = CompletionProcessx;;

(*** OPENS ***************************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)
let dummy = []

let to_stringm = 
 GS.get_wgoal >>= function
  | [i] -> WN.to_stringm i
  | g -> 
 foldl (fun s i -> WN.to_stringm i >>= fun t -> return (s^", "^t)) "" g
;;

let union = List.union;;
let diff = List.diff;;
let inter = List.intersect;;


(* ------------------------------------------------------------------ *)
(*  CREATE goal                                                       *)
(* ------------------------------------------------------------------ *)
let set cs = 
 match cs with
  | [c] ->
   let s, t = Equation.terms c in
   let e = CP.initial in
   WN.create s [e] [] [WGN.Initial] [] >>= fun i ->
   WN.create t [] [e] [] [WGN.Initial] >>= fun j ->
   GS.set_wgoal [i; j]
  | _ -> 
   raise (C.GiveUp "Goal module supports only single goal.")
;;

(* ------------------------------------------------------------------ *)
(*  CHECK whether goal is proven                                      *)
(* ------------------------------------------------------------------ *)

let proven_for =
 GS.get_wgoal >>= fun g ->
 let add r n = WN.process_inter n >>= fun ps -> return (union ps r) in
 foldl add [] g  
;;


(* ------------------------------------------------------------------ *)
(*  REWRITE goal                                                      *)
(* ------------------------------------------------------------------ *)

let constants = 
 let cs r n = 
  N.data n >>= fun (s,t) -> 
  return (union (union (T.cons s) (T.cons t)) r)
 in
 NS.all_nodes >>= fun ns ->
 foldl cs [] ns >>= fun cs1 ->
 let cs r n = WN.term n >>= fun t -> return (union r (T.cons t)) in
 GS.get_wgoal >>= fun gs ->
 foldl cs [] gs >>= fun cs2 ->
 return (union cs1 cs2)
;;

 
(* In problems such as SYN080-1, one needs to use an instance of an 
   unorientable equation to reduce both terms to a common normal form.
   This function instantiates the variables on the rhs of rule which
   do not occur on the lhs by a fixed constant. *)
let instantiate rule e vs =
 let rec inst c sub = function
  | [] -> sub (* no variable to be instantiated *)
  | v :: vs ->
   let sub = Sub.add v (T.Fun(c, [])) sub in inst c sub vs
 in
 constants >>= fun cs ->
 match cs with
  | [] -> return None (* no constant in signature *)
  | c :: _ ->
   let sub = inst c Sub.empty vs in
   let l, r = Rule.lhs rule, Rule.rhs rule in
   return (Some (Rule.of_terms l (Sub.apply_term sub r)))
;;
 
let find_instance rule e =
 let l, r = Rule.lhs rule, Rule.rhs rule in
 let vs = diff (T.vars r) (T.vars l) in
 instantiate rule e vs >>= fun rule_option ->
 match rule_option with
  | None -> return rule (* no progress, but does also not hurt *)
  | Some rulesub -> return rulesub
;;

(* Checks whether variables in rhs of rule also occur on lhs *)
let lhs_vars_subset_rhs rule = 
 let vl = T.vars (Rule.lhs rule) in
 let vr = T.vars (Rule.rhs rule) in
 List.is_subset vr vl
;;

(* Rewrites term t with rule at position p, and returns the reduct t'
   together with instantiated rule (for ordered rewriting).
   If the rewrite label r is empty and the rhs of rulesub contains 
   variables which do not occur on the lhs then these vars are tried
   to be instantiated (such that the rule rrulesub terminates) *)
let rewrite_term t rule p (r,e) =
 W.M.Rulex.rewrite_subrule t rule p >>= function
  | None -> return None 
  | Some (t', rulesub) ->
   if (not (List.is_empty r)) || (lhs_vars_subset_rhs rulesub) then
    return (Some (t', rulesub))
   else
    find_instance rulesub e >>= fun rulesub ->
    W.M.Rulex.rewrite_subrule t rulesub p (* real reduct *)
;;

(* create new goal node from goal node g which gets reduced using node 
   n, provided that the process set intersection is non-empty.
   Performs both normal and ordered rewriting (termination check!), and
   instantiates the equation given by n respectively if it is not a 
   proper rewrite rule as variables on the rhs do not occur on the lhs.
   *) 
let reduct_with g ((n, b), p) =
 N.bcontent b n >>= fun (l,r,(_,r0),_,(_,e)) ->
 WN.content g >>= fun (t, (ps0, ps1)) ->
 let ps = union ps0 ps1 in
 if List.is_empty (inter (union e r0) ps) then
  return []
 else
  rewrite_term t (Rule.of_terms l r) p (r0,e) >>= function
   | None -> return []
   | Some (t', rule) -> (
    (*W.M.Term.to_stringm t' >>= fun s ->
    Format.printf "Rewriting with %s produced %s\n%!" ns s;*)
    OMKBtt.filter_eqlabels (inter e ps) rule >>= fun e' ->
    let ps0' = inter ps0 (union r0 e') in
    let ps1' = inter ps1 (union r0 e') in
    let o0, o1 = WGN.Reduct(g,n,ps0'),WGN.Reduct(g,n,ps1') in
    WN.create t' ps0' ps1' [o0] [o1] >>= fun g' ->
    return [g'])
;;

let to_str xs = 
 let rec tos = function
  [] -> "]"
  | [x] -> (string_of_int x)^"]"
  | x :: xs -> (string_of_int x)^", "^(tos xs)
 in "["^(tos xs)
;;

(* Finds all encompassemnt matching term in gn, and collects respective
   new goal nodes *)
let reducts_of_term gn idx =
 WN.term gn >>= fun t ->
 NI.encompassment_candidates_in idx t >>= fun cs ->
 let add r c = reduct_with gn c in
 foldl add [] cs >>= fun g' ->
 W.M.Term.to_stringm t >>= fun ts ->
 (*Format.printf "Reducts of %s: %s\n%!" ts (to_str g');*)
 return g'
;;

(* Return one-step reducts of all goal nodes in g *)
let reducts g idx =
 let add r t = reducts_of_term t idx >>= fun ts -> return (union ts r) in
 foldl add [] g >>= fun g' ->
 return g'
;;

(* Rewrite goal nodes in g using index idx. Return only new nodes *)
let rewrite_to_nf g idx =
 let rec rewrite g all =
  reducts g idx >>= fun g' ->
  if List.is_empty g' then return all 
  else rewrite (diff g' all) (union g' g)
 in
 rewrite g g >>= fun g' -> return (diff g' g)
;;

let gc = filter WN.has_non_empty_processes

(* Rewrite goal nodes with (new) node in singleton index sidx. If that
   yields progress, rewrite the new goal nodes also with old nodes *)
let rewrite _ sidx =
 let t = Unix.gettimeofday () in
 GS.get_wgoal >>= fun g ->
 rewrite_to_nf g sidx >>= fun g' ->
 (if List.is_empty g' then
  return ()
 else
  gc (union g' g) >>=
  GS.set_wgoal) >>= fun _ ->
 St.add_t_two (Unix.gettimeofday () -. t)
;;

let decide p =
 NS.restrict_to_process p >>
 GS.get_wgoal >>= fun g ->
 NI.rewrite_index >>= rewrite_to_nf g >>
 proven_for >>= fun ps ->
 if List.is_empty ps then return C.Sat else return C.Uns
;;

(***************************** HISTORY **********************************)

let rec find_occurrence_for p h =
 let has_p = List.exists (fun q -> CP.is_prefix_of q p) in
 match h with
  | [] -> failwith "no occurrence in goal found"
  | WGN.Initial :: _ -> WGN.Initial
  | (WGN.Reduct (_, _, ps) as r) :: _ when has_p ps -> r
  | _ :: h -> find_occurrence_for p h
;;

let one_side_history g p is_first =
 let rec one_side_history g (gs, ns) =
  WN.term g >>= fun t ->
  WN.borigin is_first g >>= fun h ->
  let o = find_occurrence_for p h in
  match o with
  | WGN.Initial -> return ((t,o) :: gs, ns)
  | WGN.Reduct (g', j, _) -> one_side_history g' ((t,o)::gs, j::ns)
 in one_side_history g ([], [])
;;

let history_of_goal g p =
 one_side_history g p true >>= fun (ts1, ns1) ->
 one_side_history g p false >>= fun (ts2, ns2) ->
 return ((ts1, ts2), union ns1 ns2)
;;

let common_reduct_history p cns =
 GS.get_wgoal >>= fun gs ->
 filter (WN.inter_contains_process p) gs >>= fun gs ->
 match gs with
  | [] -> failwith "No common reduct found for process"
  | g :: _ -> 
   history_of_goal g p >>= fun (gs, ns) ->
   return (gs, History.sort_in ns cns)
;;


let ssimplify id s t pt node =
 Printf.sprintf 
  "cnf(%i, plain, %s = %s, inference(simplify, [status(thm)], [%i, %i]))\n" 
  id (T.to_string s) (T.to_string t) pt node
;;

let sconjecture id s t f =
 Printf.sprintf
 "cnf(%i, conjecture, %s = %s, '%s')\n\n" 
 id (T.to_string s) (T.to_string t) f
;;

let both_str parent id take_left f = function
 | (s, WGN.Initial), (t, WGN.Initial) -> sconjecture id s t f
 | (s, WGN.Reduct (i, j, _)), (t, WGN.Initial)
 | (s, WGN.Initial), (t, WGN.Reduct (i, j, _)) -> ssimplify id s t parent j
 | (s, WGN.Reduct (i, j, _)), (t, WGN.Reduct (i', j', _)) ->
  ssimplify id s t parent (if take_left then j else j')
;;

let make_history id f (h1, h2) =
 let rec make_history p id str (h1, h2) take_left = 
  match h1, h2 with
  | [], _
  | _, [] -> failwith "empty history is boring" 
  | [x], [y] -> str ^ (both_str p id take_left f (x, y))
  | [x], y :: ys -> 
   let str' = str ^ (both_str p id take_left f (x, y)) in
   make_history id (id+1) str' (h1, ys) false
  | x :: x' :: xs, y :: ys ->
   let str' = str ^ (both_str p id true f (x, y)) in
   make_history id (id+1) str' (x' :: xs, y :: ys) true
 in
 make_history (-1) id "" (h1, h2) (List.length h1 > 1)
;;

(* Find "last reducts" for both components, these are the normal forms. *)
let normal_form_history p =
 GS.get_wgoal >>= fun gs ->
 let last_reduct b gs =
  filter (WN.bcontains_prefix b p) gs >>= function
   | [] -> failwith "No normal forms found"
   | g :: gs -> foldl (fun g g' -> return (if g > g' then g else g')) g gs
 in
 pair (last_reduct true, last_reduct false) gs >>= fun (g, g') ->
 one_side_history g p true >>= fun (gs, ns) ->
 one_side_history g' p false >>= fun (gs', ns') ->
 return ((gs, gs'), List.union ns ns')
;;

(* Depending on status, switch between a history justifying a common 
   reduct and a history justifying distinct normal forms *)
let history p cns = function
 | C.Uns -> common_reduct_history p cns
 | C.Sat -> normal_form_history p
 | _ -> failwith "Unexpected status when printing proof."
;;
(* Status explanation. *)
let explanation = function
 | C.Uns -> "Goal could be joined."
 | C.Sat -> "Goal was not joined in gropund-complete system."
 | _ -> failwith "Unexpected status when printing proof."
;;

(* TODO replace 0 in call of make_history - take freh nod id? *)
(* Returns a string representing the proof.
   status is result status of MKBtt, either Sat or Uns. p is the
   successful process, cns are the node history for the completed
   system (only necessary in case of Uns), and file is the name of
   the input problem. *)
let proof_output status p cns =
 history p cns status >>= fun ((h1, h2), ns) ->
 W.get_options >>= fun o ->
 let str = make_history 0 (C.filename o) (h1, h2) in
 let ns = List.rev (List.sort (fun i j -> compare i j) ns) in
 History.joint_history [] ns >>= fun h ->
 History.as_string p (C.filename o) h >>= fun hs ->
 return (hs^str, explanation status)
;;

