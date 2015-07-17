(* Copyright 2011 Sarah Winkler
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

(** Auxiliary functions mainly related to terms 
 @author Sarah Winkler
 @since  2011/01/07 *)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module T = U.Term;;

(*** TYPES ***************************************************************)
type t = Rewriting.Position.t * int list

(*** FUNCTIONS ***********************************************************)

let to_string (p, s) =
 let rec to_s = function
    [] -> "}"
  | [i] -> string_of_int i^"}"
  | i :: is -> (string_of_int i)^","^(to_s is)
 in (Pos.to_string p)^".{"^(to_s s)
;;

let all_to_string = function
 | [] -> "[]"
 | p :: [] -> "["^(to_string p)^"]"
 | ps -> (List.foldl (fun s p -> s^", "^(to_string p)) "[" ps)^"]"
;;


let root = function
 | T.Var _  -> Pos.root,[]
 | T.Fun(_,ts) -> Pos.root,List.mapi (fun i _ -> i) ts
;;

let non_ac (p,is) = is = []

let pos_diff p q =
 let p, q = Pair.map Pos.to_list (p,q) in
 if List.length p >= (List.length q) then
  Pos.of_list (List.drop (List.length q) p)
 else
  Pos.root (* should not matter *)
;;

let leq p q =
 let leq = function
  | (p,[]),(q,_) -> Pos.is_prefix p q
  | (p,ps),(q,qs) when not (Pos.is_prefix p q) -> false
  | (p,ps),(q,qs) when p = q -> List.is_subset qs ps
  | (p,ps),(q,qs) -> (* p is prefix of q *)
   let i = fst (Pos.split_first (pos_diff q p)) in List.mem i ps
 in leq (p,q)
;;

let geq = flip leq

let equal (p,ps) (q,qs) = 
 (Pos.compare p q = 0) && (List.equal ps qs)
;;

let (>) p q = (geq p q) && (not (equal p q))

let are_parallel (p,ps) (q,qs) = 
 if Pos.are_parallel p q then true
 else if Pos.compare p q = 0 then List.is_empty (List.intersect ps qs)
 else false
;;
 
