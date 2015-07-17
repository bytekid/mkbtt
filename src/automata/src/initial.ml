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
 module Elogic = R.Elogic;;
 module F = Format;;
 module Fun = R.Function;;
 module Ass = Util.Map.Make (Fun) (State);;
 module Lhs = Lhs.Make (R);;
 module M = Monad.Make (R);;
 module Monad = R.Monad;;
 module Path = Path.Make (R) (C);;
 module Pos = R.Position;;
 module Rule = R.Rule;;
 module T = R.Term;;
 module Term = Term.Make (R);;
 module Trs = R.Trs;;
 module Var = R.Variable;;
 module R = R.Monad;;

 (*** OPENS *******************************************************************)
 open Util;;

 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type automaton = A.t;;
 type trs = Trs.t;;

 type term = Term.t =
  | Var of Var.t
  | State of State.t
  | Fun of Fun.t * term list
 ;;

 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;

 let generate m a =
  let max f _ m = m >>= fun i -> M.lift (max i) (M.find_ari f) in
  Ass.fold max m (M.return 0) >>= fun i ->
  let ps = Ass.range m in
  let a = A.set_finals ps a and xs = State.combinations i ps in
  Ass.fold (fun f p m ->
   M.find_ari f >>= fun i -> m >>= fun a ->
   let update a ps = A.update (Lhs.Fun (f,ps)) p a in
   M.foldl update a (List.assoc i xs)) m (M.return a)
 ;;

 let individual fs gs =
  let add m ass f = m >>= (M.return <.> flip (Ass.add f) ass) in
  (if gs = [] then M.return Ass.empty
  else M.fresh_state >>= fun p -> M.foldl (add (M.return p)) Ass.empty gs) >>=
  flip (M.foldl (add M.fresh_state)) fs >>= flip generate (A.create 1000)
 ;;
 
 let min = individual [];;
 let split = flip individual [];;
 
 let specific f g ts =
  let update a t = M.lift Term.choose (A.rewrite t a) >>= function
   | Var _ -> M.fail "variable occured"
   | State p -> M.return (A.add_final p a)
   | Fun _ as t ->
    let modify a p = M.lift (A.add_final p) (g t p a) in
    f t >>= M.foldl modify a
  in
  M.foldl update (A.create 1000) ts
 ;;

 let instances f g fs gs ts =
  let rec transform q = function
   | Var _ -> State q
   | State _ as t -> t
   | Fun (f,ts) -> Fun (f,List.map (transform q) ts)
  in
  let update q a = function
   | State p -> M.return (A.add_final p a)
   | t ->
    let modify a p = M.lift (A.add_final p) (g (transform q t) p a) in
    f t >>= M.foldl modify a
  in
  if ts = [] then M.return (A.create 1000) else
   (if List.for_all Term.is_ground ts then M.return (A.create 1000)
   else individual fs gs) >>= fun a ->
   let ps = A.finals a in
   let add (q,a) p = M.lift (Pair.make q) (A.update (Lhs.State p) q a) in
   (if List.length ps = 1 then M.return (List.hd ps,a)
   else M.fresh_state >>= fun q -> M.foldl add (q,a) ps) >>= fun (q,a) ->
   M.foldl (update q) (A.set_finals [] a) ts
 ;;

 let custom fs gs hs =
  M.foldl (fun i f -> M.lift (max i) (M.find_ari f)) 0 fs >>= fun i ->
  (if i = 0 then M.return (A.create 1000) else individual gs hs) >>= fun a ->
  let ps = A.finals a in
  let xs = State.combinations i ps in
  M.foldl (fun a f ->
   M.fresh_state >>= fun p -> M.find_ari f >>= fun i ->
   let a = A.add_final p a in
   let update a ps = A.update (Lhs.Fun (f,ps)) p a in
   M.foldl update a (List.assoc i xs)) (A.set_finals [] a) fs
 ;;

 let normal fs trs =
  let trs = Trs.filter Rule.is_left_linear trs in
  let update xs ys a f =
   let update a ps =
    let t = T.make_fun f (List.map (fun p -> List.assoc p xs) ps) in
    if Trs.is_normal_form t trs then
     let xs = List.filter (Elogic.matches t <.> snd) xs in
     let x = List.hd xs and map (p,u) = ((p,u),T.size u) in
     let max (x,n) (y,m) = if n >= m then (x,n) else (y,m) in
     let x = List.foldl (fun x y -> max x (map y)) (map x) (List.tl xs) in
     A.update (Lhs.Fun (f,ps)) (fst (fst x)) a
    else M.return a
   in
   M.find_ari f >>= fun i -> M.foldl update a (List.assoc i ys)
  in
  let rec equal s t = match (s,t) with
   | T.Var _, T.Var _ -> true
   | T.Fun (f,ss), T.Fun (g,ts) -> f = g && List.for_all2 equal ss ts
   | _, _ -> false
  in
  let us = List.flat_map (T.proper_subterms ~p:T.is_fun) (Trs.lhs trs) in
  let ts = List.unique ~c:(fun s t -> 1 - (Bool.to_int (equal s t))) us in
  M.liftm (R.fresh_var) >>= fun x -> M.fresh_state >>= fun p ->
  M.foldl (fun (xs,ps) t ->
   let update p = M.return ((p,t)::xs,p::ps) in
   if Trs.is_normal_form t trs then M.fresh_state >>= update
   else M.return (xs,ps)) ([(p,T.make_var x)],[p]) ts >>= fun (xs,ps) ->
  M.foldl (fun i f -> M.lift (max i) (M.find_ari f)) 0 fs >>= fun i ->
  let ys = State.combinations i ps in
  M.foldl (update xs ys) (A.create 1000) fs >>= fun a ->
  let extend q a = M.foldl (fun a p -> A.update (Lhs.State p) q a) a ps in
  if List.length ps = 1 then let q = List.hd ps in M.return (A.add_final q a)
  else M.fresh_state >>= fun q -> extend q (A.add_final q a)
 ;;
end
