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

(** Auxiliary functions and state monad for goals.
@author Sarah Winkler
@since  2010/10/25
*)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module Fun = Rewriting.Function;;
module Term = U.Term;;
module Rule = U.Rule;;
module W = World;;
module N = IndexedNode;;
module WN = IndexedWNode;; 
module G = Types.Goal;;
module H = Hashtbl;;
module Monad = W.Monad;;
module Sig = W.M.Sig;;

(*** OPENS (2) ***********************************************************)
open Monad;;
open World;;

(*** FUNCTIONS ***********************************************************)
let get_true c = c.true_symbol;;
let set_true t c = W.set_goal_state {c with true_symbol = Some t};;
let get_false c = c.false_symbol;;
let set_false t c = W.set_goal_state {c with false_symbol = Some t};;
let get_equal c = c.equal_symbol;;
let set_equal t c = W.set_goal_state {c with equal_symbol = Some t};;

let get_symbol fget fset n a = 
 W.get_goal_state >>= fun c ->
 match fget c with
  | None ->   Sig.create_fun a n >>= fun s -> fset s c >> return s
  | Some s -> return s
;;

let true_symbol () = get_symbol get_true set_true "true" 0

let false_symbol () = get_symbol get_false set_false "false" 0

let false_term () = false_symbol () >>= fun f -> return (Term.Fun(f,[]))

let equal_symbol () = get_symbol get_equal set_equal "equal" 2

let equals_x_x_is_true () =
 equal_symbol () >>= fun eq ->
 Sig.fresh_var >>= fun x ->
 true_symbol () >>= fun t ->
 let eq_x_x = Term.Fun(eq,[Term.Var x; Term.Var x]) in
 return (Equation.create_normalized eq_x_x (Term.Fun(t,[])))
;;

let true_is_false () = 
 true_symbol () >>= fun t ->
 false_symbol () >>= fun f -> 
 return (Equation.of_terms (Term.Fun(t,[])) (Term.Fun(f,[])))
;;

let equals_s_t_is_false s t =
 equal_symbol () >>= fun eq ->
 false_symbol () >>= fun f ->
 let eq_s_t = Term.Fun(eq,[s; t]) in
 return (Equation.of_terms eq_s_t (Term.Fun(f,[])))
;;

let true_node () = 
 equals_x_x_is_true () >>= 
 N.by_eqn >>= function
  | Some n -> return n
  | None -> failwith "true node not found"
;;

let split s =
 W.get_goal_state >>= fun c ->
 match c.goal with
  | G.NodeGoal g -> iter (N.split s) g
  | G.WaldmeisterGoal g -> iter (WN.split s) g
  | G.ExistentialGoal g -> iter (fun (n, ps) -> N.split s n) g
  | G.NoGoal -> return ()
;;

let restrict_to_process p =
 W.get_goal_state >>= fun c ->
 let restrict = N.restrict_to_process p in
 match c.goal with
  | G.NodeGoal g -> iter restrict g
  | G.WaldmeisterGoal g -> iter (WN.restrict_to_process p) g
  | G.ExistentialGoal g -> iter (fun (n, ps) -> restrict n) g
  | G.NoGoal -> return ()
;;

let remove_processes ps =
 W.get_goal_state >>= fun c ->
 let remove = N.remove_processes ps in
 match c.goal with
  | G.NodeGoal g -> iter remove g
  | G.WaldmeisterGoal g -> iter (WN.remove_processes ps) g
  | G.ExistentialGoal g -> iter (fun (n, ps) -> remove n) g
  | G.NoGoal -> return ()
;;

let get = W.get_goal_state;;
let set c g = W.set_goal_state {c with goal = g};;

let get_goal = get >>= fun c -> return c.goal

let get_ngoal =
 get >>= fun c ->
 match c.goal with
  | G.NodeGoal g -> return g
  | _ -> failwith "No node goal to be returned"
;;

let get_egoal =
 get >>= fun c ->
 match c.goal with
  | G.ExistentialGoal g -> return g
  | _ -> failwith "No existential goal to be returned"
;;

let get_wgoal =
 get >>= fun c ->
 match c.goal with
  | G.WaldmeisterGoal g -> return g
  | _ -> failwith "No Waldmeister goal to be returned"
;;

let get_goal_eqs =
 get >>= fun c ->
 return c.goal_eqs
;;

let set_goal g = get >>= fun c -> set c g;;
let set_ngoal g = get >>= fun c -> set c (G.NodeGoal g);;
let set_wgoal g = get >>= fun c -> set c (G.WaldmeisterGoal g);;
let set_egoal g = get >>= fun c -> set c (G.ExistentialGoal g);;
let windex = get >>= fun c -> return c.windex;;

let store_goal_equation e = 
 get >>= fun c ->
 W.set_goal_state {c with goal_eqs = e :: c.goal_eqs}
;;

let constants = 
 get_goal_eqs >>= fun es ->
 let cs = List.flat_map (Rule.cons <.> Equation.to_rule) es in
 return (List.unique cs)
;;
