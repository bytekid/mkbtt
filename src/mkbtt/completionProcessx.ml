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

(** Functions related to processes as they occur in MKBtt nodes.
@author Sarah Winkler
@since  2007/12/18 *)

(** Functions related to processes (i.e., bit strings) in MKBtt nodes.
    Prepare for a lot of non-functional stuff ... *)

(*** OPENS ***************************************************************)
open Util;;

(*** MODULES *************************************************************)
module H = Hashtbl;;
module St = Statistics;;
module W = World;;
module Monad = W.Monad;;

(*** OPENS ***************************************************************)
open Monad;;
open World;;

(*** FUNCTIONS ***********************************************************)
include Types.CompletionProcess

type set = t list

let epsilon = initial;;

let initial_set = [initial];;

let empty = []

let length = List.length

let compare = compare

let fprintf = List.fprintf Format.pp_print_int "@[@]"

(* administrative functions *)
let cache_m tbl f k =
 if H.mem tbl k then return (H.find tbl k)
 else (f k >>= fun v -> (H.add tbl k v; return v))
;;

let get_value t p = W.get_processes >>= fun c -> return (H.find (t c) p);;

let set_value t p n = 
 W.get_processes >>= fun c -> return (H.add (t c) p n)
;;

let get_choice_count = get_value (fun c -> c.choices);;
let get_esize = get_value (fun c -> c.esizes);;
let get_csize = get_value (fun c -> c.csizes);;
let get_rosize = get_value (fun c -> c.rosizes);;
let set_choice_count = set_value (fun c -> c.choices);;
let set_esize = set_value (fun c -> c.esizes);;
let set_csize = set_value (fun c -> c.csizes);;
let set_rosize = set_value (fun c -> c.rosizes);;


let inc_choice_count p = 
 W.get_processes >>= fun c -> 
 get_choice_count p >>= fun n ->
 return (H.add c.choices p (n+1))
;;


let to_string = function
 | [] -> "e"
 | l -> List.to_string string_of_int "" l
;;

let set_to_string l =
 let rec s = function
  | [] -> "]"
  | [x] -> (to_string x) ^ "]"
  | x :: xs -> (to_string x) ^ ", " ^ (s xs)
 in "["^(s l)
;;

(***** plain set functions *****)

let is_empty s = List.length s = 0

let add x = List.union [x]

let union = List.union ~c:compare

let union3 x y = List.union (List.union x y) 

let diff = List.diff ~c:compare

let inter = List.intersect 

let complement p = W.get_processes >>= fun c -> return (diff c.all p)

let choose = List.hd

let equal = List.equal

let is_subset = List.is_subset ~c:compare

let singleton x = [x]

let map_union f = List.concat <.> (List.map f)

let mem = List.mem ~c:compare

let fold = List.fold_right

let iter = List.iter

let cardinal = List.length

let filter = List.filter

let remove = List.remove

let map = List.map

(***** split a process *****)

let children p = [p @ [0]; p @ [1]]

let split_table t p =
 let p0, p1 = p @ [0], p @ [1] in
 W.get_processes >>= fun c -> 
 let k = H.find (t c) p in
 H.add (t c) p0 k; H.add (t c) p1 k;
 return ()
;;

(* return child processes for p and update hashtables *)
let split_state' p =
 let p0, p1 = p @ [0], p @ [1] in
 split_table (fun c -> c.choices) p >>
 split_table (fun c -> c.csizes) p >>
 split_table (fun c -> c.esizes) p >>
 split_table (fun c -> c.rosizes) p >>
 Statistics.inc_n_processes >>
 W.get_processes >>= fun c ->
 W.set_processes {c with all = p1 :: p0 :: (List.remove p c.all)}
;;

let split_state = Monad.iter split_state' 

(* split function replaces some process ids with two process ids
   (split process) *)
let split_one ht pid =
 let pid_set = [pid] in
 try
  let pid2 = H.find ht pid in
  union pid2 (remove pid pid_set)
 with
  Not_found -> pid_set
;;

let apply_split s ps = 
 let app l = List.concat (List.map (fun p -> [p@[0];p@[1]]) l) in
 union (diff ps s) (app (inter s ps))
(* let h = H.create 20 in
 List.iter (fun p -> H.add h p (children p)) s;
 map_union (split_one h) ps*)
;;

(***** functions related to choice counter *****)

let inc_choice_counts = Monad.iter inc_choice_count

let print_choice_log w steps = 
 W.get_processes >>= fun c -> 
 H.iter 
  (fun p c -> if c >= 0 then Format.printf "%s: %i;  " (to_string p) c)
  c.choices;
 get_choice_count w >>= fun n ->
 Format.printf "\n Choice ratio for %s: %f \n"
  (to_string w) ((float_of_int n) /. (float_of_int steps));
 return ()
;;

let choice_ratio_for w steps =
 get_choice_count w >>= fun n ->
 return ((float_of_int n) /. (float_of_int steps))
;;

(***** simple list functions *****)

let rec is_prefix_of p q = 
 match p, q with
  | [], _ -> true
  | a::p', b::q' when a=b -> is_prefix_of p' q'
  | _ -> false
;;

let fprintf_all = List.fprintf

let add_zero = List.map (fun p -> p @ [0])

let add_one = List.map (fun p -> p @ [1])

let all_processes = W.get_processes >>= fun c -> return c.all

let remove ps =
 Statistics.dec_n_processes (List.length ps) >>
 W.get_processes >>= fun c ->
 W.set_processes {c with all = List.diff c.all ps}
;;

let change_esize op k p = get_esize p >>= fun n -> set_esize p (op n k);;
let add_esize = change_esize (+);;
let remove_esize = change_esize (-);;

let change_csize op k p = get_csize p >>= fun n -> set_csize p (op n k);;
let add_csize = change_csize (+);;
let remove_csize = change_csize (-);;

let change_rosize op k p = get_csize p >>= fun n -> set_rosize p (op n k);;
let add_rosize = change_rosize (+);;
let remove_rosize = change_rosize (-);;
