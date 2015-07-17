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

(** Choose a node from a node set according to s_sum/s_max
  @author Sarah Winkler
  @since  2009/12/05 *)

(* Choose a node for which 
  c(<s:t, ..., E, ...>) = (c(E), |s:t|, -#E)       (case of costl) or
  c(<s:t, ..., E, ...>) = (c(E), |s:t|)            (case of costwol) 
  is minimal. Here c(E) = min{c(p) | p e E} and the cost of a process is
  c(p) = size(E[N,p]) + size(C[N,p]) where size(R) = \Sum_{l=r e R} |l|+|r|
  and |s:t| is either |s|+|t| (for s_sum) or max{|s|,|t|} (for s_max). 
  #E is the number of labels in E. *)

(*** OPENS (1) ***********************************************************)
open Util;;
(*** SUBMODULES **********************************************************)
module H = Hashtbl;;
module Term = U.Term;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module W = World;;
module Monad = W.Monad;;
module St = Statistics;;
module CP = CompletionProcessx;;
module N = IndexedNode;;
module NSA = NodeSet;;
module NS = NodeState;;
module SS = SelectionStrategy;;

(*** OPENS (2) ***********************************************************)
open Monad;;

(*** GLOBALS *************************************************************)
let costs = H.create 30

(*** FUNCTIONS ***********************************************************)

let log1 s = Format.printf "%s\n" s; W.log 1 s

let log2 s = Format.printf "%s\n" s; W.log 2 s

(* get |l|+|r| for rule l -> r *)
let sum_rule_size r = (Term.size (Rule.lhs r)) + (Term.size (Rule.rhs r))

let max_rule_size r = max (Term.size (Rule.lhs r)) (Term.size (Rule.rhs r))

(* get \Sum_{l->r \in R} |l|+|r| for TRS R (called trs) *)
let trs_size size = Trs.fold (fun r s -> s + (size r)) 0

let trs_sum_size = trs_size sum_rule_size

let trs_max_size = trs_size max_rule_size

let eqs_size size = List.foldr (fun r s -> s + (size r)) 0

let eqs_sum_size = eqs_size Equation.size

let eqs_max_size = eqs_size Equation.max_size

let size_funs =
 W.get_options >>= fun o ->
 if Completion.selection_macro o = Some SelectionStrategy.JapanSum then
  return (trs_sum_size, eqs_sum_size)
 else
  return (trs_max_size, eqs_max_size)
;;

let data_size n = 
 W.get_options >>= fun o ->
 if Completion.selection_macro o = Some SelectionStrategy.JapanSum then
  N.sum_size n
 else
  N.max_size n
;;

(* compute cost of process c(p) *)
let pcostold (csize,esize) map p =
 let find p = try Some (H.find costs p) with Not_found -> None in
 W.get_options >>= fun o ->
 match find p with
  | Some cost -> return cost
  | None ->
   NS.all_nodes >>= fun ns -> 
   NSA.project_e p ns >>= fun ep ->
   NSA.project_c p ns >>= fun cp ->
   (* new 
   CP.get_esize p >>= fun es -> CP.get_csize p >>= fun cs ->
   let cost' = es+cs in *)
   let cost = (esize ep) + (csize cp) in 
   (H.add costs p cost;
   (*Format.printf "Real for %s: %i+%i, new: %i+%i\n%!" (CP.to_string p) (esize ep) (csize cp) es cs; *)
   (*if cost <> cost' then failwith "Buuuuh";*)
   return cost)
;;

let pcostnew (csize,esize) map p =
 let find p = try Some (H.find costs p) with Not_found -> None in
 W.get_options >>= fun o ->
 match find p with
  | Some cost -> return cost
  | None ->
   CP.get_esize p >>= fun es -> CP.get_csize p >>= fun cs ->
   let cost = es+cs in
   (H.add costs p cost;
   (*Format.printf "( %s cost %i+%i)" (CompletionProcessx.to_string p) es cs;*)
   return cost)
;;

let pcost = pcostnew

(* minimum_map f s selects x in s such that f x is minimal *)
let minimum_map f map s =
 let foldmin (min, y) x =
  f map x >>= fun fx ->
  if fx < min then return (fx, x) else return (min, y)
 in
 let x = List.hd s in
 f map x >>= fun fx ->
 foldl foldmin (fx, x) (List.tl s)
;;

(* determines cost of process set E *)
let ecost sfuns map ps = 
 if List.is_empty ps then return (10000, []) 
 else minimum_map (pcost sfuns) map ps
;;

(* cost tuple for node as called c_l by Haruhiko, extended with node age *)
let costl map n = 
 N.eo n >>= fun e ->
 N.e_all n >>= fun eall -> 
 size_funs >>= fun (cs,es) ->
 let t  = Unix.gettimeofday () in
 ecost (cs,es) map eall >>= fun (ce,p) ->
 St.add_t_one (Unix.gettimeofday () -. t) >>
 data_size n >>= fun d ->
 (*Format.printf " %i cost (%i,%i,%i)\n%!" n ce d ((-1)*(List.length e));*)
 return (ce, d, (-1)*(List.length e), p,n)
;;

(* cost tuple for node as called c_wol by Haruhiko *)
let costwol map n =
 N.eo n >>= fun e ->
 size_funs >>= fun (cs,es) -> 
 ecost (cs,es) map e >>= fun (ce, p) ->
 data_size n >>= fun d ->
 return (ce,d)
;;

let costalpha map n =
 N.eo n >>= fun e ->
 N.e_all n >>= fun eall ->
 size_funs >>= fun (cs,es) ->
 ecost (cs,es) map eall >>= fun (ce,p) ->
 data_size n >>= fun d ->
(* Format.printf " %i has cost (%i,%i,%i)\n%!" n ((-1)*(List.length e)) ce d;*)
 return ((-1)*(List.length e), ce, d, p,n)
;;

(* select node with minimal c(<s:t, ..., E, ...>) = (c(E), |s:t|) *)
let bestwol = lift snd <.> (minimum_map costwol (H.create 30))

(* select node with minimal c(<s:t, ..., E, ...>) = (c(E), |s:t|, -#E) *)
let bestl' = lift snd <.> (minimum_map costl (H.create 30))
(*let bestl' l = 
 minimum_map costl (H.create 30) l >>= fun ((v1,v2,v3,v4,v5),x) ->
 Format.printf " %i has cost (%i,%i,%i,%s,%i)\n%!" x v1 v2 v3 (CP.to_string v4) v5;
 return x
;; *)

let alpha' = lift snd <.> (minimum_map costalpha (H.create 30))

let bestl () =
 H.clear costs;
 ChoiceState.inc_count >>= fun c ->
 NS.open_nodes >>= fun no ->
 if c mod 100 = 0 then (* select old node *)
  return (List.foldl1 (fun i m -> if i < m then i else m) no)
 else
  bestl' no
;;

(* first take node affecting maximum number of processes, then process
with small E/C, then small terms (max) *)
let alpha () =
 H.clear costs;
 ChoiceState.inc_count >>= fun c ->
 NS.open_nodes >>= fun no ->
 if c mod 7 = 0 then (* select old node *)
  return (List.foldl1 (fun i m -> if i < m then i else m) no)
 else
  alpha' no
;;

let best_worst_process () =
 H.clear costs;
 size_funs >>= fun sfuns ->
 CompletionProcessx.all_processes >>= fun ps ->
 let inv_pcost m p = pcost sfuns m p >>= fun c -> return (c * (-1)) in
 minimum_map inv_pcost costs ps >>= fun (v,p) ->
 minimum_map (pcost sfuns) costs ps >>= fun good ->
 return (good,(-1*v,p))
;;
(* bad processes *)
let bad_processes o =
 let sm = Completion.selection_macro o in
 let r = Completion.kill_processes o in
 if not (sm = Some(SS.JapanSum) || sm = Some(SS.JapanMax)) then 
  H.clear costs;
 size_funs >>= fun sfuns ->
 CompletionProcessx.all_processes >>= fun ps ->
 minimum_map (pcost sfuns) costs ps >>= fun (v,_) ->
 let exc u = (float_of_int u)/.(float_of_int v) > r in
 let ck f ps p = f p >>= fun u -> return (if exc u then p::ps else ps) in
 foldl (ck (pcost sfuns costs)) [] ps >>= fun ps ->
 return ps
;;
(* -------------------------------------------------------------------- *)
(* for pre-oriented rules *)
(* -------------------------------------------------------------------- *)
let pcostrel (csize,esize) map p =
 let find p = try Some (H.find costs p) with Not_found -> None in
 W.get_options >>= fun o ->
 match find p with
  | Some cost -> return cost
  | None ->
   CP.get_esize p >>= fun es -> 
   CP.get_csize p >>= fun cs ->
   CP.get_rosize p >>= fun rs ->
   let cost = es+cs+rs in
   (H.add costs p cost;
   (*Format.printf "( %s cost %i+%i)" (CompletionProcessx.to_string p) es cs;*)
   return cost)
;;

(* determines cost of process set E *)
let ecostrel sfuns map ps =
 if List.is_empty ps then return (10000, [])
 else minimum_map (pcostrel sfuns) map ps
;;

(* cost tuple for node as called c_l by Haruhiko, extended with node age *)
let costlrel map n =
 N.eo n >>= fun e ->
 N.e_all n >>= fun eall ->
 N.r0o n >>= fun r0 -> N.r1o n >>= fun r1 ->
 let eall = CP.union r0 (CP.union r1 eall) in
 size_funs >>= fun (cs,es) ->
 let t  = Unix.gettimeofday () in
 ecostrel (cs,es) map eall >>= fun (ce,p) ->
 St.add_t_one (Unix.gettimeofday () -. t) >>
 data_size n >>= fun d ->
 (*Format.printf " %i cost (%i,%i,%i)\n%!" n ce d ((-1)*(List.length e));*)
 return (ce, d, (-1)*(List.length e), p,n)
;;


let bestl' = lift snd <.> (minimum_map costlrel (H.create 30))

let bestlrel () =
 H.clear costs;
 ChoiceState.inc_count >>= fun c ->
 NS.open_nodes >>= fun no ->
 if c mod 100 = 0 then (* select old node *)
  return (List.foldl1 (fun i m -> if i < m then i else m) no)
 else
  bestl' no
;;
