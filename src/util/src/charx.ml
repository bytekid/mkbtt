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

(*** INCLUDES *****************************************************************)
include Char;;

(*** MODULE TYPES *************************************************************)
module type CHAR = sig
 val chr : int -> char
 val code : char -> int
 val compare : t -> t -> int
 val escaped : char -> string
 val lowercase : char -> char
 val uppercase : char -> char
end

(*** FUNCTIONS ****************************************************************)
(* Properties *)
let is_space c = c = ' ' || c = '\n' || c = '\t' || c = '\r';;
let is_lower c = 'a' <= c && c <= 'z';;
let is_upper c = 'A' <= c && c <= 'Z';;
let is_alpha c = is_lower c || is_upper c;;
let is_digit c = '0' <= c && c <= '9';;
let is_alnum c = is_alpha c || is_digit c;;

(* Compare Functions *)
let equal c d = compare c d = 0;;

(* Printers *)
let fprintf fmt = Format.fprintf fmt "@[%c@]";;
let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;
