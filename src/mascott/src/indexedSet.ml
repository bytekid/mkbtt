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

(** Module providing node sets
 @author Sarah Winkler
 @since  2008/11/16 *)

(** Implementation of set for nodes. 
    Just taking Setx does not work because nodes are mutable. This
    module compares with respect to a node's data set, which never
    changes. *)
(*** OPENS ***************************************************************)
open Util;;
(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module Rewriting = Processors.Rewritingx;;
module T = Rewriting.Term;;
module Fun = Rewriting.Function;;
module Var = Rewriting.Variable;;
module Sub = Rewriting.Substitution;;
module CP = CompletionProcessx;;
module Monad = World.Monad;;
module N = IndexedNode;;

(*** OPENS ***************************************************************)
open Monad;;

(*** TYPES ***************************************************************)
type t = int list

(*** FUNCTIONS ***********************************************************)
let empty = [];;
let union = List.union;;
let add x xs = N.id x >>= fun i -> return (union [i] xs);;
let diff = List.diff;;
let mem x xs= N.id x >>= fun i -> return (List.mem i xs);;
let remove x xs = N.id x >>= fun i -> return (List.remove i xs);;



let by_id x =
 let e = "No node for id found in IndexedSet.id" in
 N.by_id x >>= function Some n -> return n | None -> failwith e
;;

let by_eqn x =
 let e = "No node for eqn found in IndexedSet.id" in
 N.by_eqn x >>= function Some n -> return n | None -> failwith e
;;

let map f = 
 let ff i = by_id i >>= f in
 map ff
;;

let iter m f l =
 let fold m i = m >>= fun c -> N.by_id i >>= f in
 List.foldl (fun m x -> return (fold m x)) m l
;;

let list_map_union f = List.fold_left union [] <.> (List.map f);;

(* for function f mapping a node to 'a list, this maps a node set
   to the union of the resukts of f applied to its elements *)
let map_union f is =
 let fold res i = 
  by_id i >>= fun n ->
  return ((f n) @ res)
 in
 foldl fold [] is 
;;

let to_string =
 let add s x = id x >>= fun n -> return (s^"\n"^(Nodex.to_string n)) in
 foldl add "" 
;; 

let to_stringm l =  
 foldl (fun s i -> N.to_stringm i >>= fun n -> return (s ^ "\n " ^ n)) "" l
 >>= fun ls -> return ("["^ls^"]")
;;

let singleton n = N.id n >>= fun i -> return [i]

let choose s = let i = (List.hd s) in N.by_id i

let is_empty l = l = []

let cardinal = List.length

let fold f set a = 
 List.fold_right 
  (fun i m -> m >>= fun a -> by_id i >>= fun n -> return (f n a))
  set
  (return a)
;;

let exists p s = fold (fun x b -> (p x) || b) s false 

let filter p set =
 List.filter (fun i -> let n = N.by_id i in p n) set
;;

let of_list = List.map N.id

(* --- Choice from a node set ---------------------------------------- *)
(* find minimum according to Node.smaller function *)
(*
let minimum cmp ns =
 List.fold_left
 (fun m i -> let n = N.by_id i in if cmp n m then n else m)
 (choose ns)
 ns
;;*)


(* for f mapping x to (a,b), get (\cap (\pi_1 (fx)), \cap (\pi_1 (fx)))*)
(*
let map_union2 f s =
 List.fold_left
  (fun (xs, ys) i -> 
    let n = N.by_id i in
    let (x, y) = f n in (List.union x xs, List.union y ys))
  ([], []) s
;;

let from_eqs f set =
 List.fold_left
  (fun res x -> let n = f x in add n res)
  [] set
;;

let two_set x y = union [x] [y]

*)
