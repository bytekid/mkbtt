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

(*** MODULES ******************************************************************)
module F = Format;;

(*** TYPES ********************************************************************)
type t = int;;

(*** FUNCTIONS ****************************************************************)
(* Miscellaneous *)
let copy = id;;
let hash = id;;
let max = max;;
let min = min;;
let next = succ;;
let of_int = id;;
let to_int = hash;;
let zero = 0;;

let combinations n ps =
 let rec combinations i xs =
  if i > n then xs else
   let ys = List.assoc (i-1) xs in
   let add zs p = List.foldl (fun zs qs -> (p::qs)::zs) zs ys in
   combinations (i+1) ((i,List.foldl add [] ps)::xs)
 in
 combinations 1 [(0,[[]])]
;;

(* Compare Functions *)
let compare = compare;;
let equal p q = compare p q = 0;;

(* Printers *)
let fprintf fmt p = F.fprintf fmt "@[%i@]" p;;
let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;
