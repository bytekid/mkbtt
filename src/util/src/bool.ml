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

(*** TYPES ********************************************************************)
type t = bool;;

(*** FUNCTIONS ****************************************************************)
(* Constructors and Destructors *)
let to_int b = if b then 1 else 0;;
let of_int n = n > 0;;

(* Miscellaneous *)
let copy = id;;
let hash = to_int;;

(* Compare Functions *)
let compare = compare;;
let equal b b' = compare b b' = 0;;

(* Printers *)
let fprintf fmt = Format.fprintf fmt "@[%B@]";;
let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;
