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

(** Data type and functions related to goal in theorem proving
    This implementation uses just nodes to represent the conjecture.
@author Sarah Winkler
@since  2009/07/03 *)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module C = Completion;;
module Pos = Rewriting.Position;;
module Term = U.Term;;
module Rule = U.Rule;;
module Sub = U.Substitution;;
module Rewrite = U.Rewrite;;
module Elogic = U.Elogic;;
module W = World;;
module Monad = W.Monad;;
module GS = GoalState;;
module NS = NodeState;;
module NSA = NodeSet;;
module NI = NodeTermIndexx;;
module N = IndexedNode;;
(*** OPENS (2) ***********************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

(* ------------------------------------------------------------------ *)
(*  CREATE goal                                                       *)
(* ------------------------------------------------------------------ *)
let dummy = []

(* gets skolemized equation, returns goal *)
let set cs = 
 let eqns_of c =
  GS.store_goal_equation c >>
  let s, t = Equation.terms c in
  GS.equals_x_x_is_true () >>= fun e ->
  GS.equals_s_t_is_false s t >>= fun e' ->
  NSA.of_axioms [e] >>= fun ns_true ->
  NSA.of_axioms [e'] >>= fun ns_false ->
  NS.add_open ns_true >>
  GS.set_ngoal ns_false
 in
 match cs with
 | [c] -> eqns_of c
 | _ ->  raise (C.GiveUp "Goal module supports only single goal.") 
;;

let to_stringm = 
 GS.get_ngoal >>= fun g ->
 NSA.to_stringm g
;;

(* ------------------------------------------------------------------ *)
(*  REWRITE goal                                                      *)
(* ------------------------------------------------------------------ *)

let union = List.union

let union3 x y = union (union x y)

let rewrite_node (retrieve,el,pred) (n,b) single_index =
 N.brule b n >>= fun rule ->
 retrieve single_index (Rule.rhs rule) >>= fun matches ->
 let add (cn, change) m =
  MKBtt.rewrite_node_with true (el,pred) (n,b) m >>= fun (c,change') -> 
  return (union c cn, change || change')
 in
 foldl add ([], false) matches
;;

let rewrite_goal fs gs idx =
 let add_reducts (irn, cn) n =
  rewrite_node fs (n,true) idx >>= fun (cn_lr,change_lr) ->
  rewrite_node fs (n,false) idx >>= fun (cn_rl,change_rl) ->
  let cn' = union cn_lr cn_rl in
  let change = change_lr || change_rl in
  if not change then return (union [n] irn,cn)
  else return (irn,union cn' cn)
 in
 foldl add_reducts ([], []) gs
;;

let rewrite n =
 let retrieve = NI.encompassment_candidates_in in
 let elabels l2 l3 = union l2 l3 in
 let variants t t' = Elogic.is_variant t' t in 
 rewrite_goal (retrieve, elabels,variants)
;;

let rewrite_with n single_index =
 let rec rew g =
  rewrite n g single_index >>= fun (_, created) ->
  MKBtt.gc created >>= function
    [] -> return []
  | g' -> rew g' >>= fun g'' -> return (g'' @ g')
 in GS.get_ngoal >>= rew
;;

(* in case of rule with extra variables on rhs,
   instantiate rule appropriately *)
let check_rule n b =
 N.brule b n >>= fun rule ->
 if not (Rule.is_rewrite_rule rule || Rule.is_rewrite_rule (Rule.invert rule))
 then (
  GoalState.constants >>= function
   | [] -> return ()
   | c :: _ ->
    let l,r = Rule.to_terms rule in
    let evs = List.diff (Term.vars r) (Term.vars l) in
    let sigma = Sub.of_list (List.map (fun v -> (v,Term.Fun (c,[]))) evs) in
    N.ec n >>= fun ls ->
    N.parents n >>= fun ps ->
    let _,b' = Equation.oriented_of_terms l (Sub.apply_term sigma r) in
    let bb = (b && b') || (not b' && not b) in
    N.create l (Sub.apply_term sigma r) [] [] ls (Types.Node.Instance((n,bb),ls)) >>= fun n -> 
    NS.add_open [n])
 else return ()
;;

let rewrite n single_index =
 N.to_stringm n >>= fun s ->
 check_rule n true >> check_rule n false >>
 rewrite_with n single_index >>= fun g' ->
 if List.is_empty g' then 
  return ()
 else (
  GS.get_ngoal >>= fun g ->
  MKBtt.rewrite_open g' >>= fun g'' ->
  MKBtt.gc (union3 g g' g'') >>= fun ns ->
  GS.set_ngoal ns)
;;

(* ------------------------------------------------------------------ *)
(*  CHECK whether goal is proven                                      *)
(* ------------------------------------------------------------------ *)

let proven_for =
  GS.true_is_false () >>= fun eq ->
  N.by_eqn eq >>= function
   | Some n -> N.e_all n
   | None -> return []
;;

let decide p =
 NS.project_r_closed p >>= fun trs ->
 GS.get_goal_eqs >>= fun es ->
 let joinable e = 
  let s,t = Equation.terms e in Rewrite.are_joinable s t trs 
 in
 let j = List.for_all joinable es in
 (* flip answer due to negated conjectures *)
 if j then return C.Uns else return C.Sat
;;

