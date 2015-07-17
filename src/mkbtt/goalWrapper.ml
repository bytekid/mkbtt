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

(** Wrapper for different goal implementations. 
 @author Sarah Winkler
 @since  2010/04/28 *)

(* Wrapper to have one module coping with existential as well as 
 universal goals and different implementations thereof. *)

(*** SUBMODULES **********************************************************)
module C = Completion;;
module EGoal = ExistentialGoal;;
module WGoal = WaldmeisterGoal;;
module T = Types;;
module W = World;;
module Monad = W.Monad;;
module GS = GoalState;;

(*** OPENS  **************************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

let set g =
 W.get_options >>= fun o ->
 match g with
 | None -> ((*Format.printf "no Goal set\n%!";*) GS.set_goal T.Goal.NoGoal)
 | Some goal -> ( 
  match goal with
   | C.Universal eqs -> (
    match C.goal_implementation o with
     | C.NodeGoal -> NodeGoal.set eqs
     | C.WaldmeisterGoal -> WGoal.set eqs)
   | C.Existential eqs -> EGoal.set eqs)
;;
(*
let dummy = 
 St.options >>= fun o ->
 match C.goal o with
 | C.Universal _ -> (
  match C.goal_implementation o with
  | C.NodeGoal -> return (GoalState.NodeGoal Goal.dummy)
  | C.WaldmeisterGoal -> return (GoalState.WaldmeisterGoal WGoal.dummy))
 | C.Existential _ -> return (GoalState.ExistentialGoal EGoal.dummy)
;;
*)
let proven_for =
 GS.get_goal >>= function
 | T.Goal.NoGoal -> return []
 | T.Goal.NodeGoal _ -> NodeGoal.proven_for
 | T.Goal.WaldmeisterGoal _ -> WGoal.proven_for
 | T.Goal.ExistentialGoal _ -> EGoal.proven_for
;;

let rewrite n nidx =
 GS.get_goal >>= function
 | T.Goal.NoGoal -> return ()
 | T.Goal.NodeGoal g -> NodeGoal.rewrite n nidx 
 | T.Goal.WaldmeisterGoal g ->  WGoal.rewrite n nidx 
 | T.Goal.ExistentialGoal g -> EGoal.rewrite n nidx 
;;

let decide p =
 GS.get_goal >>= function
 | T.Goal.WaldmeisterGoal _ ->  WGoal.decide p
 | T.Goal.ExistentialGoal _ -> EGoal.decide p
 | _ -> NodeGoal.decide p
;;


let to_string =
 GS.get_goal >>= function
 | T.Goal.NoGoal -> return "(no goal)"
 | T.Goal.NodeGoal _ -> NodeGoal.to_stringm
 | T.Goal.WaldmeisterGoal _ ->  WGoal.to_stringm
 | T.Goal.ExistentialGoal _ -> EGoal.to_stringm
;;

let proof_output szs p cns =
 GS.get_goal >>= function
 | T.Goal.WaldmeisterGoal _ ->  WGoal.proof_output szs p cns
 | T.Goal.ExistentialGoal _ -> EGoal.proof_output p
 | _ -> raise (C.GiveUp "No proof output for this goal implementation.")
;;
