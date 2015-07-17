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

(** Functions to compute the shape/class of a node such that renaming
    isomorphisms are easier top check
@author Sarah Winkler
@since  2011/03/30 *)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Term = U.Term;;
module C = Completion;;
module W = World;;
module Monad = W.Monad;;
module I = World.IntTermIndex;;
module Sig = W.M.Sig;;

(*** OPENS (2) ***********************************************************)
open Monad;;
open World;;

(*** FUNCTIONS ***********************************************************)
let dummy_var =
 W.get_iso_context >>= fun c ->
 let set v = W.set_iso_context {c with dummy_var = Some v} in
 match c.dummy_var with
  | None ->   Sig.fresh_var >>= fun v -> set v >> return v
  | Some s -> return s
;;

let dummy_fun =
 W.get_iso_context >>= fun c ->
 let set v = W.set_iso_context {c with dummy_fun = Some v} in
 match c.dummy_fun with
  | None ->   Sig.create_fun 0 "#" >>= fun f -> set f >> return f
  | Some s -> return s
;;

let pattern_of_term t =
 let rec class_of_term f v = function
  | Term.Var _ -> Term.Var v
  | Term.Fun(_, ts) -> Term.Fun(f,List.map (class_of_term f v) ts)
 in 
 dummy_fun >>= fun f -> dummy_var >>= fun v -> return (class_of_term f v t)
;;

let by_pattern p =
 W.get_iso_context >>= fun c ->
 return (try Some (I.find_key p c.classes) with Not_found -> None)
;; 

let new_class p =
 W.get_iso_context >>= fun cxt ->
 let c, classes' = I.fresh cxt.classes in
 let classes' = I.add c p classes' in
 W.set_iso_context {cxt with classes = classes'} >>
 return c
;;

let class_of_pattern p =
 by_pattern p >>= fun c -> match c with
  | None -> new_class p 
  | Some c -> return c
;;

let class_of_eqn eqn =
 let cs t = pattern_of_term t >>= class_of_pattern in
 project cs (Equation.terms eqn)
;;
 

