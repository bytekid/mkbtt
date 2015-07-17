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

(** Termination checks
@author Sarah Winkler
@since  2008/02/19
*)

(** Checks termination for orient/ordered rewrite inference*)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Rule = U.Rule;;
module Trs = U.Trs;;
module CP = CompletionProcessx;;
module W  = World;;
module Monad = W.Monad;;

(*** OPENS ***************************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

(* find the last TRS that was checked for process p, and check if equal? *)
let trs_terminates trs =
 W.get_options >>= fun o ->
 if Completion.check_externally o then
  ExternalTermination.check trs
 else
  InterfaceTTT.check trs
;;


(* check if trs_p (the constraint system for p) terminates together
   with the rule s->t. Use options in options. *)
let add_rule_ok rule trs  = (* one way *)
 (* if lhs is variable or var(lhs) are no subset of var(rhs), forget it *)
 (*if Rule.is_rewrite_rule rule then*) (* already checked beforehand *)
  trs_terminates (Trs.add rule trs)
 (*else
  return false*)
;;

(* helper function for below wrapper: 
   checks if s rewrites to t using rules in trs in at most n steps *)
let rewrites_to_n s t trs n = 
 let rec rewrites_to' ts n =
   let reducts u = Trs.reducts u trs in
   if n = 0 then false else
   let tset = List.concat (List.map reducts ts) in
   if List.mem t tset then true else rewrites_to' tset (n - 1)
 in 
 if rewrites_to' [s] n then 
  (
  (*Printf.printf "Yes\n"; 
  Printf.printf "rewrite %s to %s with %s?\n" (Term.to_string s) (Term.to_string t) (Trs.to_string trs);*)
  true
  ) else false
;;

let rewrites_to_full s t trs =
(* Printf.printf "rewrite %s to %s with %s?\n" (Term.to_string s) (Term.to_string t) (Trs.to_string trs);*)
 let rec rewrites_to' ts =
  let reducts u = Trs.reducts u trs in
  if List.is_empty ts then false else
  let tset = List.concat (List.map reducts ts) in
  if List.mem t tset then true else rewrites_to' tset
 in
 if rewrites_to' [s] then
  (
  (*Printf.printf "Yes\n";
  Printf.printf "rewrite %s to %s with %s?\n" (Term.to_string s) (Term.to_string t) (Trs.to_string trs);*)
  true
  ) else false
;;

let rewrites_to s t trs n =
 if n < 0 then rewrites_to_full s t trs else rewrites_to_n s t trs n
;;


(* provides wrapper for the above add_rule_ok function such that
   rewriting is used to save termination checks *)
let add_rule_ok_wrapper rule trs =
 let s, t = Rule.lhs rule, Rule.rhs rule in
 W.get_options >>= fun o -> 
 let use_rew, steps = Completion.rewrite_before_termination_check o in
 if  use_rew && (rewrites_to s t trs steps) then 
  return true
 else if use_rew && (rewrites_to t s trs steps) then 
  return false
 else 
  add_rule_ok rule trs
;;


(* if node has terms s:t, checks whether 
   C[nodeset, p] u s->t,
   C[nodeset, p] u t->s
   terminate. Dependent on options o, dpsoup and dsset are used or not,
   and internal/external termination is chosen.
   (sps1, sps2) are spsoup plus subterm pairs arising from s->t, t->s
   respectively.
   Returns ((lr, rl), (spsoup'', dsset'') where
    - the first component describes in which direction s:t can be oriented
      for the process p,
    - the second component holds the (possibly modified) subterm pair
      soup and defined symbol node set *)
let preserved (st, ts) trs =
  add_rule_ok_wrapper st trs >>= fun lr ->
  add_rule_ok_wrapper ts trs >>= fun rl ->
  return (lr, rl)
;;

(* simpler version for just one direction, used in oMKBtt rewrite *)
let preserved_by_rule = add_rule_ok

(* ----------------------------------------------------------------- *)
(* --------------- FOR PARALLEL TERMINATION CHECKING --------------- *)
(* ----------------------------------------------------------------- *)

let preserved_parallel trs0 trs1 =
 W.get_options >>= fun o ->
 if Completion.check_externally o then
  ExternalTermination.check_parallel trs0 trs1
 else 
  InterfaceTTT.check_parallel trs0 trs1
;;

