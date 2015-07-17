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

(** Interface to parser for TPTP format.
 @author Sarah Winkler
 @since  2009/03/28 *)

(*** OPENS ********************************************************************)
open Util;;

(*** MODULES *************************************************************)
module M = U.Monad;;
module S = U.Signature;;
module Parser = U.Parser;;
module Term = U.Term;;
module StringInput = Parsec.StringInput;;
module Error = Parsec.Error;;
module Var = Rewriting.Variable;;
module Fun = Rewriting.Function;;
module C = Completion;;
module T = TPTP;;
module Th = Theory;;
module L = U.Label;;

(*** EXCEPTIONS **********************************************************)
exception Disequality_not_allowed
exception Equality_not_allowed

(*** GLOBALS *************************************************************)
(*** FUNCTIONS ***********************************************************)

let (>>=) = M.(>>=);;
let (>>) = M.(>>);;
let (<?>) = Util.(<?>);;

(*--------------- FIX GOALS FOR COMPLETION PROCEDURE -------------------*)

let fix_zero_conjectures = 
 M.return ([], None)


let fix_one_conjecture = function
  | (T.Equal eqn, _) -> M.return ([], Some eqn)
  | (T.NotEqual eqn, _) -> M.return ([], Some eqn)
;;

(* Return tuple with list of additional axioms and goal *)
let fix_conjectures = function
 | [] -> fix_zero_conjectures
 | [c] -> fix_one_conjecture c
 | _ -> raise (C.GiveUp "Multiple conjectures not supported.")
;;


let rec split axs conjs = function
 | [] -> fix_conjectures conjs >>= fun (axs', goal) -> 
  M.return (axs @ axs', goal)
 | (T.Equal e, T.Axiom) :: cs
 | (T.Equal e, T.Hypothesis) :: cs -> split (e :: axs) conjs cs
 | (e, T.Conjecture c) :: cs -> split axs ((e, c) :: conjs) cs
 | _ -> raise (C.GiveUp "Axioms with inequalities not supported.")
;;

let replace (es,gl) f =
 M.set_theory f U.Label.AC >>= fun g ->
 let rec rep_term = function
  | Term.Fun (f',ts) when f = f' -> Term.Fun (g,List.map rep_term ts)
  | Term.Fun (g,ts) -> Term.Fun (g,List.map rep_term ts)
  | t -> t
 in
 let es' = List.map (Equation.map rep_term) es in
 M.return (es', Option.map (Equation.map rep_term) gl)
;;

let of_file f =
 let cs, s = TPTP.load f in 
 M.set s >> split [] [] cs >>= fun (eqs, g) ->
 let eqs', fs = Th.recognize_ac eqs in
 M.foldl replace (eqs',g) fs >>= fun res ->
 let label f = M.set_theory f L.AC >>= fun fac -> M.return (Th.AC fac) in
 M.map label fs >>= fun th ->
 M.return (res, th)
;;

