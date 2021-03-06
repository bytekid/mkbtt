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

(** Auxiliary functions mainly related to rules
 @author Sarah Winkler
 @since  2010/08/13 *)

(*** OPENS ********************************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module M = U.Monad;;
module Term = U.Term;;
module Rule = U.Rule;;
module Sub = U.Substitution;;
module E = U.Elogic;;

(*** TYPES ***************************************************************)
(*** EXCEPTIONS **********************************************************)
(*** GLOBALS *************************************************************)
(*** FUNCTIONS ***********************************************************)
let (>>=) = M.(>>=)

let return = M.return 

(* Rewrites a term.
  t: term to be rewritten
  rule: to be used, assumed to be already renamed
  p: position in t
 Returns reduct t', substitution sigma, renamed rule *)

let rewrite_renamed t p rule =
  let l', r' = Rule.lhs rule, Rule.rhs rule in
  let t' = Term.subterm p t in
  let sigma = try Some (E.match_term t' l') with E.Not_matchable -> None in
  match sigma with
   | None -> return None
   | Some sigma ->
    let u = Sub.apply_term sigma r' in
    let t' = Sub.apply_term sigma t in
    return (Some (Term.replace p u t', sigma, rule))
;;

let rewrite t rule p = Rule.rename rule >>= rewrite_renamed t p


(* Narrows a term.
  t: term to be narrowed
  rule: to be used
  p: position in t
 Returns Some(narrowed term t', substitution sigma) if unifiable, 
 None otherwise *)
let narrow t rule p =
 Rule.rename rule >>= (fun rule ->
  let l', r' = Rule.lhs rule, Rule.rhs rule in
  let t' = Term.subterm p t in
  try
   let sigma = E.unify t' l' in
   let u = Sub.apply_term sigma r' in
   let t' = Sub.apply_term sigma t in
   return (Some(Term.replace p u t', sigma, rule))
  with E.Not_unifiable -> return None
 )
;;

(* on the contrary to Rule.rewrite, this function also returns the
   matching substitution; in contrast to rewrite it also checks
   rhs for variables to substitute (no real rewrite rules)  
let ordrewrite t rule p =
 Equation.renaming (Equation.of_rule rule) >>= (fun rule ->
 let l', r' = Rule.lhs rule, Rule.rhs rule in
 let t' = Term.subterm p t in
 let sigma = Elogic.unify t' l' in
 let u = Sub.apply_term sigma r' in
 let t' = Sub.apply_term sigma t in
 return (Term.replace p u t', sigma, rule))
;; *)

(* rewrites term t with rule at position p, returning the reduced term
   as well as the instantiated rule (necessary for oMKBtt).
   rule is already renamed.                                             *)
let rewrite_subrule t rule p =
 rewrite_renamed t p rule >>= function
  | None -> return None
  | Some (t, sigma, rule) ->
   let l, r = Rule.map (Sub.apply_term sigma) rule in
   return (Some (t, Rule.of_terms l r))
;;

