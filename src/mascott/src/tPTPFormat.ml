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

(** Types for TPTP input problem
 @author Sarah Winkler
 @since  2009/01/16 *)


(*** SUBMODULES **********************************************************)
(*** TYPES ***************************************************************)
type term =
 | Var of string
 | Fun of string * term list
;;

type clause_status = 
    Axiom | Hypothesis | Conjecture | NegatedConjecture
;;

type formula = 
    Equality of term * term
  | DisEquality of term * term
;;

type element = 
   InputClause of string * clause_status * formula
 | Include of string * string (* dir, file *)
;;

(*** EXCEPTIONS **********************************************************)
(*** GLOBALS *************************************************************)
(*** FUNCTIONS ***********************************************************)

