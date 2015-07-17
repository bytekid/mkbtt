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
type t = int;;

(*** FUNCTIONS ****************************************************************)
(* Miscellaneous *)
let copy = id;;
let hash = id;;
let next = succ;;
let zero = 0;;
let to_int = hash;;

(* Compare Functions *)
let compare = compare;;
let equal f g = compare f g = 0;;

(* Printers *)
let fprintf fmt f = Format.fprintf fmt "@[%i@]" f;;
let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;
