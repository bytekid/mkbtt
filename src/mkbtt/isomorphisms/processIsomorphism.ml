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

(** Switch isomorphism checks
 @author Sarah Winkler
 @since  2009/08/11
*)

(*** OPENS ***************************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Rewriting = Processors.Rewritingx;;
module Trs = U.Trs;;
module C = Completion;;
module St = Statistics;;
module NS = NodeState;;
module CP = CompletionProcessx;;
module N = IndexedNode;;
module R = TrsRenaming;;
module P = TermPermutation;;
module W = World;;
module Monad = W.Monad;;

(*** OPENS ***************************************************************)
open Monad;;
open World;;

(*** FUNCTIONS ***********************************************************)
let symmetric_for_renaming rule n trs p nodeset =
 (lift Option.the <.> N.pattern_by_id) n >>= fun (i,j) ->
 (*N.to_stringm n >>= fun s ->*)
 if i <> j then (
  (*Format.printf "Not symmetric %i, %i: %s\n%!" i j s;*)
  return false)
 else (
  (*Format.printf "Symmetric %i, %i: %s\n%!" i j s;*)
  NodeSet.project_e_with_class nodeset p >>= fun e_p ->
  NodeSet.project_r_with_class nodeset p >>= fun r_p ->
  W.M.Rule.to_stringm rule >>= fun rs ->
  let renamable = 
  try
   let theta = R.orientation_symmetric rule trs in
   let es,trs = List.map fst e_p, Trs.of_list (List.map fst r_p) in
   let b = (R.eqns_invariant es theta) && (R.trs_invariant trs theta) in
   (*if b then Format.printf "Orientation of %s is symmetric\n%!" rs
   else Format.printf "Orientation of %s is not symmetric\n%!" rs;*)
   b
  with
   | R.Not_renamable -> false
  in return renamable)
;;

let symmetric_for_permutation rule _ trs p nodeset =
 NodeSet.project_e nodeset p >>= fun e_p ->
 NodeSet.project_r nodeset p >>= fun r_p ->
 let permutable = 
 try
  let pi = P.orientation_symmetric rule trs in
  (P.eqns_invariant e_p pi) && (P.trs_invariant r_p pi) 
 with
  | P.Not_permutable -> false
 in return permutable
;;

let orientation_symmetric rule n trs p =
 let t_start = Unix.gettimeofday () in
 NS.all_nodes >>= fun nodeset ->
 let answer =
  W.get_options >>= fun options -> 
  match C.check_isomorphism options with
   | C.NoChecks -> return false
   | C.Renamings -> symmetric_for_renaming rule n trs p nodeset
   | C.Permutations -> symmetric_for_permutation rule n trs p nodeset
 in
 St.add_t_isomorphisms (Unix.gettimeofday () -. t_start) >>
 answer
;;

let print_iso (p,(e, r, c),_) (p',(e', r', c'),_) =
 let e,(r,c) = List.map fst e, Pair.map (List.map fst) (r,c) in
 let e',(r',c') = List.map fst e', Pair.map (List.map fst) (r',c') in
 project W.M.Equation.list_to_stringm (e,e') >>= fun (es,es') ->
 project (W.M.Trsx.to_stringm <.> Trs.of_list) (r,r') >>= fun (rs,rs') ->
 project (W.M.Trsx.to_stringm <.> Trs.of_list) (c,c') >>= fun (cs,cs') ->
 Format.printf "%s and %s are isomorphic:\n" 
  (CP.to_string p) (CP.to_string p');
 Format.printf " E1:%s\n R1:%s\n C1:%s\n" es rs cs;
 Format.printf " E2:%s\n R2:%s\n C2:%s\n%!" es' rs' cs';
 return ()
;;

(* checks whether an isomorphism between these two processes exists *)
let exists (p,d,s) (p',d',s') =
 if s <> s' then return false
 else
  W.get_options >>= function o ->
  match C.check_isomorphism o with
   | C.NoChecks -> return false
   | C.Renamings -> R.processes_match (p,d) (p',d')
   | C.Permutations -> return (P.processes_match (p,d) (p',d'))
;;

let compute_stamp (e, r, c) =
 let equations_stamp = List.foldl (fun s (_,(i,j)) -> s+i+j) 0 in
 let lhs_stamp = List.foldl (fun s (_,(i,_)) -> i+s) 0 in
 let rhs_stamp = List.foldl (fun s (_,(_,i)) -> i+s) 0 in
 let es = equations_stamp e in
 let rls,rrs = lhs_stamp r, rhs_stamp r in
 let cls,crs = lhs_stamp c, rhs_stamp c in
 (es,rls,rrs,cls,crs)
;;

(* returns all processes that are subsumed by others.
   n(n-1)/2 exist-checks *)
let filter_subsumed ps =
 let t_start = Unix.gettimeofday () in
 let rec filter_subsumed' = function
   [] -> return []
  | p :: ps ->
   filter (fun p' -> exists p p') ps >>= fun subsumed ->
   (*(if not (List.is_empty subsumed) 
    then iter (print_iso p) subsumed 
    else return ()) >>*)
   let ps' = List.diff ps subsumed in
   filter_subsumed' ps' >>= fun ps'' ->
   return (List.union (List.map Triple.fst subsumed) ps'')
 in
 let state p = 
  NodeState.get_projections_with_class p >>= fun state ->
  let s = compute_stamp state in
  return (p,state,s) 
 in
 map state ps >>= fun states ->
 filter_subsumed' states >>= fun res ->
 St.add_t_isomorphisms (Unix.gettimeofday () -. t_start) >>
 return res
;;

let every_second_time () = let r = Random.float 1.0 in (r < 0.5)

let every_third_time () = let r = Random.float 1.0 in (r < 0.33)

(* perform periodical isomorphism checks only sometimes, currently 
   every second iteration *)
let filter_subsumed_sometimes pset = 
 if every_second_time () then filter_subsumed pset else return []
;;

let isomorphism_possible eqs =
 W.get_options >>= function o ->
 if R.renaming_possible eqs then 
  return (C.Renamings, true)
 else if P.permutation_possible eqs then
  return (C.Permutations, true)
 else 
  return (C.NoChecks, false)
;;
