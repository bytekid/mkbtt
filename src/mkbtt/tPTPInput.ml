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
module E = Equation;;

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
 M.return ([], C.Universal [], C.Conjecture C.Equality)

(* do we need this function? such such problems in TPTP *)
let fix_positive_conjecture = function
  | T.Equal eqn ->
   if E.is_ground eqn then 
    let goal = C.Universal [eqn] in
    M.return ([], goal, C.Conjecture C.Equality)
   else 
    E.skolemize eqn >>= fun eqn ->
    let goal = C.Universal [eqn] in
    M.return ([], goal, C.Conjecture C.Equality)
  | T.NotEqual eqn ->
   if E.is_ground eqn then
    M.fail "Positive != goals are assumed to be non-ground."
   else 
    let goal = C.Existential [eqn] in
    M.return ([], goal, C.Conjecture C.Disequality)
;;

let fix_negative_conjecture = function
  | T.Equal eqn ->
   if E.is_ground eqn then (* 8 problems *)
    (* don't know what to do here in general.
      in most cases, such goals s = t occur together with u != t, such 
      that this can be easily transformed into s != u.
      But e.g. in COL059-1 this is not the case, and Waldmeister seems to 
      simply join the terms, no narrowing *)
    let c = C.NegatedConjecture C.Equality in
    let goal = C.Universal [eqn] in
    M.return ([], goal, c)
   else (* 2 problems *)
    M.fail "NegatedConjectures with equality not supported."
  | T.NotEqual eqn -> 
   if E.is_ground eqn then 
    let goal = C.Universal [eqn] in
    M.return ([], goal, C.NegatedConjecture C.Disequality)
   else (* 76 problems, like COL086-1 *)
    (* Waldmesiter always outputs UNS on these problems, but GUP
      e.g. on COL005-1 which is SAT and TMO e.g. on COL071,3-1 *)
   (*Format.printf "Ex goal %s recognized\n" (E.to_string eqn);*)
    let goal = C.Existential [eqn] in
    M.return ([], goal, C.NegatedConjecture C.Disequality)
;;

let fix_one_conjecture = function
 | c, T.Plain -> fix_positive_conjecture c
 | c, T.Negated -> fix_negative_conjecture c
;;

let fix_two_conjectures c1 c2 =
 match c1, c2 with
 (* CASE 1: Two negated conjectures, one equality, one ground inequality
            Example: LCL133-1 *)
 | (T.Equal e, T.Negated), (T.NotEqual i, T.Negated)
 | (T.NotEqual i, T.Negated), (T.Equal e, T.Negated) 
   when E.is_ground i ->
   let goal = C.Universal [i] in
   M.return ([e], goal, C.NegatedConjecture C.Disequality)
 | _ -> M.fail "Multiple conjectures not supported."
;;

(* Return tuple with list of additional axioms and goal *)
let fix_conjectures = function
 | [] -> fix_zero_conjectures
 | [c] -> fix_one_conjecture c
 | [c1; c2] -> fix_two_conjectures c1 c2
 | _ -> M.fail "Multiple conjectures not supported."
;;

let rec split axs conjs = function
 | [] -> fix_conjectures conjs >>= fun (axs', goal, ctype) -> 
  M.return (axs @ axs', goal, ctype)
 | (T.Equal e, T.Axiom) :: cs
 | (T.Equal e, T.Hypothesis) :: cs -> split (e :: axs) conjs cs
 | (e, T.Conjecture c) :: cs -> split axs ((e, c) :: conjs) cs
 | _ -> M.fail "Axioms with inequalities not supported."
;;

let of_file f =
 let cs, s = TPTP.load f in 
 M.set s >> split [] [] cs
;;

