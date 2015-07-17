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

(** Implementation of bipartite graph matching automata.
 @author Sarah Winkler
 @since  2011/01/13 *)

(*** OPENS ***************************************************************)
open Util;;

(*** TYPES ***************************************************************)
type bitstring = int list

(* subset size, subsets *)
type state = {name: bitstring list; edges : (bitstring,state) Hashtbl.t}

(*** FUNCTIONS ***********************************************************)
let ints d = 
 let rec ints i = if i > d then [] else i :: (ints (i+1)) in 
ints 1
;;

let rec dary_subsets d xs =
 if (d=0) ||  (List.length xs < d) then [[]]
 else if List.length xs = d then [xs]
 else let x, xs = List.hd xs, List.tl xs in
 (List.map (fun ys -> x::ys) (dary_subsets (d-1) xs)) @ (dary_subsets d xs)
;;

let zeroes k = List.replicate k 0

let ones k = List.replicate k 1

let rec dary_bitstrings d k =
 if (d=0) then [zeroes k]
 else if k = d then [ones k] else
 let with_one = List.map (fun ys -> 1::ys) (dary_bitstrings (d-1) (k-1)) in
 let with_zero = List.map (fun ys -> 0::ys) (dary_bitstrings d (k-1)) in
 with_zero @ with_one
;;

let rec bitstrings k = 
 if k = 0 then [[]] else
 List.flat_map (fun s -> [0::s;1::s]) (bitstrings (k-1))
;;

let rec to_string = function
  [] -> ""
 | i::is -> (string_of_int i)^(to_string is)
;;

let all_to_string bs =
 let rec all_to_string = function
  | [] -> "}"
  | [x] -> (to_string x)^"}" 
  | x::xs -> (to_string x)^","^(all_to_string xs) 
 in "{"^(all_to_string bs)
;;

let dstates d k = 
 List.sort compare (List.diff (List.powerset (dary_bitstrings d k)) [[]])
;;

let plus = List.zip_with max

let minus = List.zip_with (fun a b -> if a=1 && b=1 then 0 else a)

let size = List.foldl (+) 0 

let one_at k i = 
 let rec fill j = 
  if j=k then [] else (if j=i then 1 else 0)::(fill (j+1))
 in fill 0
;;

let successor k ss input =
 let succ s =
  let input' = minus input s in
  let get_ones i b = if b=1 then [one_at k i] else [] in
  let split_input = List.flat_mapi get_ones input' in
  List.map (plus s) split_input
 in
 let ss' = List.unique (List.flat_map succ ss) in
 if List.is_empty ss' then ss else (List.sort compare ss')
;;

let get_state name states = 
 try Hashtbl.find states name
 with Not_found -> failwith ("Not found: "^(all_to_string name))
;;

let final n = 
 (List.length n = 1) && (List.for_all (fun x -> x=1) (List.hd n))
;;

let make_node states k n =
 let bs = bitstrings k in
 let s = {name = n; edges = Hashtbl.create (List.length bs * 2)} in
(* Format.printf "Add state %s\n" (all_to_string n);*)
 Hashtbl.add states n s;
 if not (final n) then
  let get_state s = get_state s states in
  let add_edge b = 
   (*Format.printf "edge %s -> %s with label %s\n%!" (all_to_string n) (all_to_string (successor k n b)) (to_string b); *)
   Hashtbl.add s.edges b (get_state (successor k n b)) 
  in
  List.iter add_edge bs
 else
  List.iter (fun b ->  Hashtbl.add s.edges b s) bs (* final state *)
;;
 
let kautomaton k =
 let states = Hashtbl.create 30 in
 let rec dautomaton d = 
  if d >= 0 then
  let s = dstates d k in
  List.iter (make_node states k) s;
  dautomaton (d-1)
 in dautomaton k;
 get_state [zeroes k] states (* root *)
;;

let s2 = kautomaton 2

let s3 = kautomaton 3

let s4 = kautomaton 4

let check initial input =
 let next s b = Hashtbl.find s.edges b in
 let s = List.fold_left next initial input
 in final s.name
;;
