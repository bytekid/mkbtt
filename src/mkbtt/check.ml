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

(** Check whether TRS completes input theory.
  @author Sarah Winkler
  @since  2008/11/30 *)

(* Functions to check whether a given TRS completes the equational theory 
   of the input problem. Termination is checked with the specified 
   options.                                                              *)

(*** SUBMODULES **********************************************************)
module Term = U.Term;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module W = World;;
module Monad = W.Monad;;
module Trsx = W.M.Trsx;;
module Eq = Equation;;

(*** OPENS ***************************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

let log2 = W.log 2

let rec normal_form t trs =
 match Trs.reducts t trs with
   [] -> t
 | u :: _ -> normal_form u trs
;;

let equal_normal_form s t trs =
 Term.compare (normal_form s trs) (normal_form t trs) = 0
;;

let reduces_to_equal_terms trs eq =
 let s, t = Eq.terms eq in
 if equal_normal_form s t trs then return true 
 else
  W.M.Term.to_stringm s >>= fun ss -> 
  W.M.Term.to_stringm t >>= fun ts ->
  log2 ("Not joinable: "^ss^" and "^ts) >>
  return false
;;

let join_axioms original_eqs completed = 
 for_all (reduces_to_equal_terms completed) original_eqs
;;

let termination = InterfaceTTT.check

let cp_joinable trs b cp =
 if equal_normal_form (fst cp) (snd cp) trs then b
 else (
 Printf.printf 
  "Non-joinable CP %s = %s\n" 
  (Term.to_string (fst cp)) 
  (Term.to_string (snd cp));
 (*Printf.printf 
  "originates from overlap (%s, %s, %s)\n"
  (Rule.to_string r1)
  (Position.to_string p)
  (Rule.to_string r2);*)
 Printf.printf
  "Normal form of CP %s = %s\n"
  (Term.to_string (normal_form (fst cp) trs))
  (Term.to_string (normal_form (snd cp) trs));
 false
 )
;;

let confluence trs = 
 Trsx.critical_pairs trs >>= fun cps ->
 return (List.fold_left (cp_joinable trs) true (List.map Eq.terms cps))
;;
