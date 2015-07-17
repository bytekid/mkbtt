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

(** Some auxiliary functions to deal with node sets.
@author Sarah Winkler
@since  2009/01/16 *)


(*** OPENS ***************************************************************)
open Util;;
(*** SUBMODULES **********************************************************)
module Rule = U.Rule;;
module Trs = U.Trs;;
module W = World;;
module Eq = Equation;;
module NS = IndexedSet;;
module CP = CompletionProcessx;;
module Nx = Nodex;;
module N = IndexedNode;;
module Monad = W.Monad;;

open Monad;;

(*** FUNCTIONS ***********************************************************)

(***** Projections *****)
let project = NS.map_union

let project_e p = project (Nodex.project_e p)

let project_r p ns = 
 project (Nodex.project_r p) ns >>= fun trs -> return (Trs.of_list trs)
;;

let project_c p ns =  
 project (Nodex.project_c p) ns >>= fun trs -> return (Trs.of_list trs)
;;
 
let project_r_closed p n =
 project (Nodex.project_r_closed p) n >>= fun trs -> 
 return (Trs.of_list trs) 
;;

let project_r_closed_unprotected p n =
 project (Nodex.project_r_closed_unprotected p) n >>= fun trs ->
 return (Trs.of_list trs)
;;

let project_e_closed p = project (Nodex.project_e_closed p)

(*
let nodeset_trs_union f ns =
 NS.fold
  (fun n trs -> let r = f n in Trs.union r trs)
   ns
   Trs.empty
;;

let nodeset_set_union f ns =
 NS.fold
  (fun n trs -> let r = f n in Setx.union r trs)
   ns
   (Setx.empty ())
;;*)

(***** Create initial nodes from equations *****)


let of_axioms axs = 
 let norm = (Monad.project Nx.s_normalize) <.> Eq.terms in
 let add xs x = 
  norm x >>= fun (s,t) ->
   if s <> t then 
    (N.create_axiom <.> (Util.uncurry Eq.of_terms)) (s,t) >>=
    N.id >>= fun i -> return (i::xs)
   else
    return xs
 in 
 foldl add [] axs >>= fun is ->
 map (N.brule true) is >>= fun rls ->
 let es = List.map Equation.of_rule rls in
 W.get_options >>= fun o ->
 W.set_options {o with axioms = es}>>
 return is
;;

let of_s_theory =
 W.get_options >>= (return <.> Completion.s_theory) >>= fun s ->
 let norm rl =
  let l,r = Rule.to_terms rl in 
  W.M.Equation.oriented_of_terms l r >>= fun (e,b) ->
  let l,r = if b then Eq.terms e else Pair.flip (Eq.terms e) in
  return (Rule.of_terms l r)
 in
 let create x = norm x >>= N.create_s_axiom in
 let add l x = create x >>= N.id >>= fun i -> N.close i >> return (i::l) in
 foldl add [] (Trs.to_list s) 
;;

let acs = 
 let create e =
  let s,t = Equation.terms e in
  N.create_deduce s t [] Types.Node.Axiom
 in
 flat_map (map create <.> Theory.ac)
;;

(***** Check whether process occurs in some label of node set *****)
let contains_process_open p = NS.exists (Nodex.contains_process_open p)

(***** convert node set to string *****)
let to_string ns =
 NS.map Nodex.to_stringm ns >>= fun ss ->
 return (List.fold_right (fun s' s -> s' ^ ", " ^ s) ss "")
;;
