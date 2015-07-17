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

(** Auxiliary functions mainly related to TRSs
 @author Sarah Winkler
 @since  2010/08/13 *)

(*** OPENS ********************************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module F = Format;;
module Pos = Rewriting.Position;;
module M = U.Monad;;
module Term = U.Term;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module Sub = U.Substitution;;
module Elogic = U.Elogic;;

(*** FUNCTIONS ***********************************************************)
let (>>=) = M.(>>=)

let return = M.return 

let overlaps trs =
 let trs = Trs.to_list trs in
 let rulepairs = List.product trs trs in
 let overlaps = Rule.overlaps in
 M.map (fun (r,s) -> overlaps r s) rulepairs >>= fun os ->
 return (List.concat os)
;;

let cps (rule1, p, rule2) =
 Rule.rename rule2 >>= fun rule2 ->
 let l1 = Rule.lhs rule1 in
 let l2 = Rule.lhs rule2 in
 try
  let mgu = Elogic.unify (Term.subterm p l1) l2 in
  let s = Sub.apply_term mgu l1 in
  let c1, c2 = Rule.rewrite s p rule2, Rule.rewrite s Pos.root rule1 in
  return (Equation.of_terms c1 c2)
 with
  | Elogic.Not_unifiable -> failwith "Trs.critical_pairs: not unifiable"
;;

let critical_pairs trs = overlaps trs >>= fun os -> M.map cps os

let to_equations trs =
 Trs.fold (fun r eqs -> (Equation.normalized_of_rule r) :: eqs) [] trs
;;

 let fprintf_wstm fmt r =
  let pt = F.fprintf fmt in
  let pvars vs = pt "@[(VAR %!";
   M.fprintf M.fprintf_var "@ %!" fmt vs >>= fun _ ->
   M.return (pt ")@]\n%!")
  in
  let rules r =
   F.fprintf fmt "@[(RULES %!"; Trs.fprintfm fmt r >>= fun _ ->
   M.return (pt ")@]\n%!") in
  pvars (Trs.vars r) >>= fun _ ->  rules r
;;

let to_wst_stringm r =
 fprintf_wstm F.str_formatter r >>= (M.return <.> F.flush_str_formatter)
;;
