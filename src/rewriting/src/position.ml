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

(*** TYPES ********************************************************************)
type t = int list;;

(*** FUNCTIONS ****************************************************************)
(* Miscellaneous *)
let append = (@);;
let copy = id;;
let hash = Hashtbl.hash;;
let length = List.length;;

(* Constructors and Destructors *)
let add_first = cons;;
let add_last i p = p @ [i];;
let root = [];;
let make = List.singleton;;
let of_list = id;;
let to_list = id;;
let head p = (List.hd <?> "root position") p;;
let tail p = (List.tl <?> "root position") p;;
let last p = (List.last <?> "root position") p;;
let init p = (List.init <?> "root position") p;;

let split_first p =
 (Pair.apply List.hd id <.> List.split_at 1 <?> "root position") p
;;

let split_last p =
 (Pair.apply id List.hd <.> List.split_last 1 <?> "root position") p
;;

(* Properties *)
let (||) p q = let (p,q) = List.remove_equal p q in p <> [] && q <> [];;
let (<=) p = List.is_prefix p;;
let (<) p = List.is_proper_prefix p;;
let (>=) p = flip (<=) p;;
let (>) p = flip (<) p;;
let is_root p = p = root;;
let are_parallel = (||);;
let is_prefix = (<=);;
let is_proper_prefix = (<);;
let is_suffix p = List.is_suffix p;;
let is_proper_suffix p = List.is_proper_suffix p;;

(* Compare Functions *)
let compare = compare;;
let equal p q = compare p q = 0;;

(* Printers *)
let fprintf fmt =
 List.fprintf (fun fmt -> Format.fprintf fmt "@[%i@]") "," fmt
;;

let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;
