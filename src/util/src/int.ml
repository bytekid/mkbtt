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
type t = int;;

(*** FUNCTIONS ****************************************************************)
(* Miscellaneous *)
let copy = id;;
let hash = id;;

let binary n =
 let rec binary n bs =
  if n <= 0 then bs
  else let i = n lsr 1 in binary i (if n = i lsl 1 then 0::bs else 1::bs)
 in
 binary n []
;;

(* Constructors *)
let next = succ;;
let zero = 0;;
let one = 1;;

let rec gcd n m = if m = 0 then n else gcd m (n mod m);;
let lcm n m = if n = 0 && m = 0 then 0 else n * m / (gcd n m);;

let pow n m = 
 if m < 0 then failwith "negative exponent";
 let r = int_of_float ((float_of_int n) ** (float_of_int m)) in
 if r < 0 then failwith "overflow";
 r
;;

let bits_big_int n =
 let log2 n = (log n) /. (log 2.) in
 int_of_float (ceil (log2 (Big_int.float_of_big_int n +. 1.0)))
;;

let bits_int64 n =
 bits_big_int (Big_int.big_int_of_string (Int64.to_string n))
;;

let bits n = (bits_big_int (Big_int.big_int_of_int n));;

let bit_max n = pow 2 n - 1;;

let bit_max_big_int n = 
 Big_int.sub_big_int (Big_int.power_int_positive_int 2 n)
 (Big_int.big_int_of_int 1)
;;

let bit_max_int64 n =
 Int64.of_string (Big_int.string_of_big_int (bit_max_big_int n))
;;

let square = flip pow 2;;
let one_complement = (~-);;

(* Properties *)
let even n = n mod 2 = 0;;
let odd n = not (even n);;
let is_zero n = n = 0;;

(* Compare Functions *)
let compare = compare;;
let equal n m = compare n m = 0;;
let gt a b = a > b;;
let eq = equal;;
let ge a b = a >= b;;

(* Printers *)
let fprintf fmt = Format.fprintf fmt "@[%i@]";;
let fprintfx fmt = Format.fprintf fmt "@{<number>%i@}";;
let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;
