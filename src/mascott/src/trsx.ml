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
module Pos = Rewriting.Position;;
module M = U.Monad;;
module Term = U.Term;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module Sub = U.Substitution;;
module Elogic = U.Elogic;;
module F = Format;;
module L = U.Label;;

(*** OPENS ********************************************************************)
open M;;

(*** FUNCTIONS ***********************************************************)
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
 Trs.fold (fun r eqs -> (Equation.of_rule r) :: eqs) [] trs
;;

let of_equations = List.foldr (Trs.add <.> Equation.to_rule) Trs.empty

let unflatten r =
 let rec term = function
  | Term.Fun(f,ts) -> 
   is_theory L.AC f >>= fun f_is_ac ->
   if f_is_ac && (List.length ts > 2) then(
    term (List.hd ts) >>= fun x -> 
    term (Term.Fun(f, List.tl ts)) >>= fun t ->
    return (Term.Fun(f,[x;t])))
   else 
    map term ts >>= fun ts -> return (Term.Fun(f,ts))
  | t -> return t
 in
 let rule r = 
  project term (Rule.to_terms r) >>= fun r -> 
  return (Util.uncurry Rule.of_terms r)
 in
 (lift Trs.of_list <.> (map rule)) (Trs.to_list r)
;;

let remove_labels r =
 let rec term = function
  | Term.Fun(f,ts) ->
   is_theory L.AC f >>= fun f_is_ac ->
   (if f_is_ac then drop_theory f else return f) >>= fun f ->
    map term ts >>= fun ts -> return (Term.Fun(f,ts))
  | t -> return t
 in
 let rule r =
  project term (Rule.to_terms r) >>= fun r ->
  return (Util.uncurry Rule.of_terms r)
 in
 (lift Trs.of_list <.> (map rule)) (Trs.to_list r)
;;


 let fprintf_wstm fmt r =
  let pt = F.fprintf fmt in
  let pvars vs = pt "@[(VAR %!"; 
   M.fprintf M.fprintf_var "@ %!" fmt vs >>= fun _ ->
   M.return (pt ")@]\n%!") in
  let pth_for fmt f = M.get_theory f >>= fun l ->
   F.fprintf fmt "@[("; L.fprintf fmt (L.make_theory l);
   (F.fprintf fmt " "; M.fprintf_fun fmt f >>= fun _ ->
   M.return (F.fprintf fmt ")@]"))
  in
  let ptheory fs =
   pt "@[(THEORY %!"; M.fprintf pth_for "@ " fmt fs >>= fun _ ->
   M.return (pt ")@]\n%!")
  in
  let psig fs = M.filter M.is_some_theory fs >>= function
   [] ->  return () | fs -> ptheory fs in
  let rules r = 
   F.fprintf fmt "@[(RULES %!"; Trs.fprintfm fmt r >>= fun _ ->  
   M.return (pt ")@]\n%!") in
  unflatten r >>= fun r ->
  pvars (Trs.vars r) >>= fun _ ->  psig (Trs.funs r) >>= fun _ ->  rules r
;;

let to_wst_stringm r =
 fprintf_wstm F.str_formatter r >>= (M.return <.> F.flush_str_formatter)
;;


let is_reducible t trs =
 Termx.funs_pos t >>= fun pos ->
 flat_map (ACRewrite.reducts t pos) (Trs.to_list trs) >>= function
  | [] -> return false
  | _  -> return true
;;


let rec normal_form' t trs = 
 Termx.funs_pos t >>= fun pos -> 
 let reducts t = flat_map (ACRewrite.reducts t pos) trs in
 reducts t >>= function
   [] -> return t 
 | (u,_,_) :: _ -> normal_form' u trs
;;

let normal_form t = normal_form' t <.> Trs.to_list

(*
let reduce trs =
 let trs = Trs.to_list trs in
 let reduce trs rule =
  apply return (Util.flip normal_form' trs) (Rule.to_terms rule) >>=
  uncurry Equation.oriented_of_terms >>= fun (eq,b) ->
  let s,t = Equation.terms eq in
  return (if b then Rule.of_terms t s else Rule.of_terms s t)
 in map (reduce trs) trs >>= 
 foldl (fun rs r -> reduce rs r >>= fun r -> return (r::rs)) [] >>=
 (return <.> Trs.of_list <.> (List.filter Rulex.distinct_terms))
;;
*)
let readable trs =  unflatten trs >>= remove_labels
