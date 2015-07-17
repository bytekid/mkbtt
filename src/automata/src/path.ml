(* Copyright 2008 Martin Korp, Christian Sternagel, Harald Zankl
 * GNU Lesser General Public License
 *
 * This file is part of TTT2.
 * 
 * TTT2 is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * TTT2 is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with TTT2. If not, see <http://www.gnu.org/licenses/>.
 *)

(*** OPENS ********************************************************************)
open Prelude;;

(*** MODULES ******************************************************************)
module Make (R : REWRITING)
            (C : CATEGORIZATION with type 'a m = 'a R.Monad.t) = struct
 (*** MODULES *****************************************************************)
 module A = Automaton.Make (R) (C);;
 module F = Format;;
 module Fun = R.Function;;
 module Lhs = Lhs.Make (R);;
 module M = Monad.Make (R);;
 module Term = Term.Make (R);;
 module Var = R.Variable;;

 (*** OPENS *******************************************************************)
 open Util;;

 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type automaton = A.t;;
 type lhs = Lhs.t;;

 type term = Term.t =
  | Var of Var.t
  | State of State.t
  | Fun of Fun.t * term list
 ;;

 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;
 
 (* Establish Fresh Paths *)
 let lift l f t p a = match t with
  | Var _ -> failwith "variable occured"
  | State q -> A.update (Lhs.State q) p a
  | Fun (g,ts) ->
   let add ti (ps,a) = f ti a >>= fun (p,a) -> M.return (p::ps,a) in
   M.foldr add ([],a) ts >>= fun (ps,a) ->
   if l then
    M.fresh_state >>= fun q ->
    A.update (Lhs.Fun (g,ps)) q a >>= A.update (Lhs.State q) p
   else A.update (Lhs.Fun (g,ps)) p a
 ;;

 let construct l f t p a =
  let rec fresh t a = match t with
   | Var _ -> failwith "variable occured"
   | State p ->
    if l then
     M.fresh_state >>= fun q -> M.lift (pair q) (A.update (Lhs.State p) q a)
    else M.return (p,a)
   | Fun (g,ts) ->
    let add ti (ps,a) = fresh ti a >>= fun (p,a) -> M.return (p::ps,a) in
    M.foldr add ([],a) ts >>= fun (ps,a) -> f (Lhs.Fun (g,ps)) a >>= fun p ->
    M.lift (pair p) (A.update (Lhs.Fun (g,ps)) p a)
  in
  lift l fresh t p a
 ;;

 let fresh_min ?(l = false) t p a =
  let fresh q _ _ = M.return q in
  M.fresh_state >>= fun q -> construct l (fresh q) t p a
 ;;

 let fresh_funs ?(l = false) p t q a =
  let root = Option.the <.> Lhs.root in
  let fresh q l _ = if p (root l) then M.fresh_state else M.return q in
  M.fresh_state >>= fun p -> construct l (fresh p) t q a
 ;;

 let fresh l a =
  A.find l a >>= Option.fold (M.return <.> Rhs.max) M.fresh_state
 ;;

 let fresh_reuse ?(f = fresh) ?(l = false) = construct l f;;
 let fresh_max ?(l = false) = construct l (const (const M.fresh_state));;
 
 (* Path Construction Strategies *)
 let fresh = id;;

 let torpa ?(c = false) f t p a =
  M.lift Term.choose (A.rewrite ~c:c t a) >>= fun u ->
  if Term.is_state u then f u p a else f t p a
 ;;

 let rewrite c t a = match t with
  | Var _ | State _ -> M.return t
  | Fun (f,ts) ->
   M.map (M.lift Term.choose <.> flip (A.rewrite ~c:c) a) ts >>= fun ts ->
   M.return (Fun (f,ts))
 ;;

 let suffix ?(p = false) ?(c = false) f t q a =
  (if p then rewrite c t a
  else M.lift Term.choose (A.rewrite ~c:c t a)) >>= fun u -> f u q a
 ;;

 let tttbox ?(p = false) ?(n = 1) ?(c = false) f t q a =
  (if p then rewrite c t a
  else M.lift Term.choose (A.rewrite ~c:c t a)) >>= fun u ->
  A.context ~c:c ~n:n u q a >>= M.foldl (fun a (v,p) -> f v p a) a
 ;;

 let ttt2 ?(p = false) ?(n = 1) ?(c = false) f t q a =
  let choose = M.lift Term.choose in
  let rewrite u = if p then rewrite c u else choose <.> A.rewrite ~c:c u in
  A.context ~c:c ~n:n t q a >>=
  M.foldl (fun a (u,p) -> rewrite u a >>= fun v -> f v p a) a
 ;;
end
