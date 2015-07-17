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

(** Just equations
@author Sarah Winkler
@since  2009/01/16 *)

(*** OPENS ********************************************************************)
open Util;;
open U;;
(*** SUBMODULES **********************************************************)
module F = Format;;
module Var = Variable;;
module M = U.Monad;;
module Sub = Substitution;;
module VSub = Termx.VSub;;

(*** TYPES ***************************************************************)
type t = Term.t * Term.t

(*** EXCEPTIONS **********************************************************)

exception Equation_is_trivial

exception Equation_is_empty

(*** GLOBALS *************************************************************)

(*** FUNCTIONS ***********************************************************)

let (>>=) = M.(>>=)

let return = M.return

let terms = id 

let fst = fst 

let snd = snd

(* comparison required which is independent of variables *)
let oriented_of_terms s t = 
 let s', _ = Termx.normalize s in
 let t', _ = Termx.normalize t in
 if Term.compare s' t' < 0 then 
  Termx.normalize_both s t, true 
 else 
  Termx.normalize_both t s, false
;;

let create_normalized s t = fst (oriented_of_terms s t)

(* assumes terms are already normalized! *)
let of_terms s t = if Term.compare s t < 0 then (s,t) else (t,s)

let of_pair (s, t) = of_terms s t

let of_rule = of_pair <.> Rule.to_terms

let is_trivial (s, t) = (Term.compare s t) = 0

let size e = 
 let s, t = terms e in
 (Term.size s) + (Term.size t)
;;

let max_size e =
 let s, t = terms e in
 max (Term.size s) (Term.size t)
;;


let set_size es =
 List.fold_left (fun s e -> s + (size e)) 0 es
;;

let compare = compare

let equal e e' = (compare e e' = 0)

let funs e =
 let (s, t) = terms e in
 List.union (Term.funs s) (Term.funs t)
;;

let to_string eq =
 let l, r = terms eq in
 (Term.to_string l) ^ " = " ^ (Term.to_string r)
;;

let all_to_string = List.to_string to_string "@[@]"

 let fprintfm fmt r =
  let s, t = terms r in
  F.fprintf fmt "@["; Term.fprintfm fmt s >>= fun _ ->
  F.fprintf fmt "@ =@ "; Term.fprintfm fmt t >>= fun _ ->
  M.return (F.fprintf fmt "@]")
 ;;

 let list_fprintfm fmt r =
  F.fprintf fmt "@["; M.fprintf fprintfm "@\n" fmt r >>= fun _ ->
  M.return (F.fprintf fmt "@]")
 ;;

let list_to_stringm es =
  list_fprintfm F.str_formatter es >>= (M.return <.> F.flush_str_formatter)
;;

let normalized_of_rule r = 
 let s, t = Rule.to_terms r in
 fst (oriented_of_terms s t)
;;

let to_rule (s, t) = Rule.of_terms s t

let normalize (s, t) =
 let s', t' = Termx.normalize_both s t in
 of_terms s' t'
;;

let renaming (l, r) = 
 let rec renaming s = function
   | Term.Var x ->
    if Sub.mem x s then M.return s
    else M.fresh_var >>= fun y -> M.return (Sub.add x (Term.Var y) s)
   | Term.Fun (f,ts) -> M.foldl renaming s ts
  in
 renaming Sub.empty l >>= (fun sigma ->
 renaming sigma r >>= (fun sigma' ->
 let instantiate = Sub.apply_term sigma' in
 return (Rule.of_terms (instantiate l) (instantiate r))))
;;


let skolemize (s, t) = Termx.skolemize s t

let is_ground e = 
 let s, t = terms e in
 (Term.is_ground s) && (Term.is_ground t)
;;

let fprintf fmt (s, t) =
 Format.fprintf fmt "@[%a@ =@ %a@]" Term.fprintf s Term.fprintf t
;;

let hash = Hashtbl.hash

let map f e =
 let s,t = terms e in
 of_terms (f s) (f t)
;;

let is_variant e e' =
 try
  let (s, t), (s', t') = terms e, terms e' in
  let sigma = VSub.empty in
  let sigma = Termx.rename_to sigma s s' in
  let _ = Termx.rename_to sigma t t' in true
 with
  | Elogic.Not_matchable -> false
;;
 
let copy = id

let fprintf fmt e =
  let s, t = terms e in
  F.fprintf fmt "@[%a@ =@ %a@]" Term.fprintf s Term.fprintf t
 ;;

let normalize s u r0 r1 e =
 let eqn,b = oriented_of_terms s u in
 let s,u = terms eqn in
 if b then s,u,r0,r1,e else s,u,r1,r0,e
;;
