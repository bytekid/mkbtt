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
open Util;;
open Rewritingx;;

(*** MODULES ******************************************************************)
module M = Monad;;
module Monad = Automatax.Monad;;
module Term = Automatax.Term;;

(*** TYPES ********************************************************************)
type t = Plain | DP | RT;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

let rec match_height = function
 | Term.Fun (f,ts) ->
  M.get_height f >>= fun n ->
  M.foldl (fun n ti ->
   if Term.is_fun ti then M.lift (min n) (match_height ti)
   else M.return n) n ts
 | _ -> failwith "term is not a function"
;;

let roof_height t =
 let rec roof_height n = function
  | Term.Fun (f,ts) ->
   M.get_height f >>= fun m ->
   M.foldl (fun m ti ->
    if not (Term.is_fun ti) || List.length (Term.vars ti) < n then M.return m
    else M.lift (min m) (roof_height n ti)) m ts
  | _ -> failwith "term is not a function"
 in
 roof_height (List.length (Term.vars t)) t
;;

let top_height = function
 | Term.Var _ | Term.State _ -> failwith "term is not a function"
 | Term.Fun (f,_) -> M.get_height f
;;

let rec label_height n = function
 | Term.Var _ | Term.State _ as t -> M.return t
 | Term.Fun (f,ts) ->
  M.set_height f n >>=
  (flip M.lift (M.map (label_height n) ts) <.> Term.make_fun)
;;

let enrichment t weak order get_height m l r = match l with
 | Term.Var _ | Term.State _ -> failwith "term is not a function"
 | Term.Fun (f,_) -> match t with
  | Plain -> Monad.liftm (get_height l >>= fun n -> label_height (n+1) r)
  | DP ->
   Monad.liftm (M.get_height f >>= fun n -> get_height l >>= fun n' ->
   let n = if weak then min n (n'+1) else n'+1 in label_height n r)
  | RT ->
   let rec equal n = function
    | Term.Fun (f,ts) ->
     M.get_height f >>= fun n' ->
     if n' = n then M.for_all (equal n) ts else M.return false
    | _ -> M.return true
   in
   Monad.liftm (M.get_height f >>= fun n -> get_height l >>= fun n' ->
   (if weak then
    if Term.fun_size l < Term.fun_size r || order then M.return (min (n'+1) m)
    else M.lift (fun c -> if c then n else min (n'+1) m) (equal n l)
   else M.return (n'+1)) >>= fun n -> label_height n r)
;;

let all ?(t = Plain) ?(w = false) = enrichment t w false match_height;;
let roof ?(t = Plain) ?(w = false) = enrichment t w true roof_height;;
let top ?(t = Plain) ?(w = false) = enrichment t w true top_height;;
let zero _ = Monad.liftm <.> label_height 0;;
