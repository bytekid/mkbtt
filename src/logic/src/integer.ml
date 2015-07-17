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

(* based on integers *)
(*
(*** SUBMODULES ***************************************************************)
 open Util;;
 open Big_int;;

(*** TYPES ********************************************************************)
 type t = int;;

(*** GLOBALS ******************************************************************)
let one = 1;;
let zero = 0;;

(*** FUNCTIONS ****************************************************************)
let is_zero n = (=) n zero;;
let is_one n = (=) n one;;

(*conversion*)
let of_int n = n;;
let of_big_int n = Big_int.int_of_big_int n;;
let of_number = of_big_int;; 

let to_string n = string_of_int n;;
let to_real _ = failwith "to_real";;
let to_rat _ = failwith "to_rat";;
let of_real _ = failwith "of_real";;
let of_int_string = int_of_string;;

(*arithmetic*)
let add = (+);;
let sub = (-);;
let mul = ( * );;
let div = (/);;
let scale = mul;;

(*comparisons*) (* precondition: denumerator greater zero *)
let eq = ( = );;
let gt = ( > );;
let ge = ( >=);;

(*min/max*)
let min = min;;
let max = max;;

let rec gcd n m = if m = 0 then n else gcd m (n mod m);;
let lcm n m = if is_zero n && is_zero m then zero else div (mul n m) (gcd n m);;
let minus = (-) 0;;
let (mod) = (mod);;

let bits n =
 let log2 n = (log n) /. (log 2.) in
 int_of_float (ceil (log2 (float_of_int n +. 1.)))
;;

let compare a b = if gt a b then 1 else if eq a b then 0 else ~-1;;
let of_string = int_of_string;;

let fprintf ppf n = Format.fprintf ppf "%s" (to_string n);;
let fprintfx ppf n = Format.fprintf ppf "@{<integer>%s@}" (to_string n);;
*)

(*
(*based on big_ints*)
(*** SUBMODULES ***************************************************************)
 open Util;;
 open Big_int;;

(*** TYPES ********************************************************************)
 type t = Big_int.big_int;;

(*** GLOBALS ******************************************************************)
let one = unit_big_int;;
let zero = zero_big_int;;

(*** FUNCTIONS ****************************************************************)
let is_zero n = eq_big_int n zero;;
let is_one n = eq_big_int n one;;

(*conversion*)
let of_int n = big_int_of_int n;;
let of_big_int n = n;;
let of_number = of_int;; 

let to_string n = string_of_big_int n;;
let to_real _ = failwith "to_real";;
let to_rat _ = failwith "to_rat";;
let of_real _ = failwith "of_real";;
let of_int_string = big_int_of_string;;

(*arithmetic*)
let add = add_big_int;;
let sub = sub_big_int;;
let mul = mult_big_int;;
let div = div_big_int;;
let scale = mul;;

(*comparisons*) (* precondition: denumerator greater zero *)
let eq = eq_big_int;;
let gt = gt_big_int;;
let ge = ge_big_int;;

(*min/max*)
let min = min_big_int;;
let max = max_big_int;;

let gcd = gcd_big_int;;
let lcm n m = if is_zero n && is_zero m then zero else div (mul n m) (gcd n m);;
let minus = minus_big_int;;
let (mod) = mod_big_int;;

let bits n =
 let log2 n = (log n) /. (log 2.) in
 int_of_float (ceil (log2 (float_of_big_int n +. 1.)))
;;

let compare a b = if gt a b then 1 else if eq a b then 0 else ~-1;;
let of_string = big_int_of_string;;

let fprintf ppf n = Format.fprintf ppf "%s" (to_string n);;
let fprintfx ppf n = Format.fprintf ppf "@{<integer>%s@}" (to_string n);;
*)

(*based on Int64*)
(*** SUBMODULES ***************************************************************)
 open Util;;
 open Int64;;

(*** TYPES ********************************************************************)
 type t = int64;;

(*** GLOBALS ******************************************************************)
let one = one;;
let two = of_int 2;;
let zero = zero;;

(*** FUNCTIONS ****************************************************************)
let is_zero n = (=) n zero;;
let is_one n = (=) n one;;

(*conversion*)
let of_int n = of_int n;;
let of_big_int n = of_float (Big_int.float_of_big_int n);;
let of_64_int n = n;;
let of_number = of_int;; 

let to_string n = to_string n;;
let to_real _ = failwith "to_real";;
let to_rat _ = failwith "to_rat";;
let to_big_int n = Big_int.big_int_of_string (to_string n);;
let to_int n = to_int n;;

let of_real _ = failwith "of_real";;
let of_int_string = of_string;;

(*arithmetic*)
let add = add;;
let sub = sub;;
let mul = mul;;
let div = div;;
let scale = mul;;

(*comparisons*) (* precondition: denumerator greater zero *)
let eq = (=);;
let gt = (>);;
let ge = (>=);;

(*min/max*)
let min = min;;
let max = max;;

let rec gcd n m = if is_zero m then n else gcd m (rem n m);;
let lcm n m = if is_zero n && is_zero m then zero else div (mul n m) (gcd n m);;
let minus = neg;;
let (mod) = rem;;
let succ = succ;;

let bits n =
 let log2 n = (log n) /. (log 2.) in
 of_float (ceil (log2 (to_float n +. 1.)))
;;

let compare a b = if gt a b then 1 else if eq a b then 0 else ~-1;;
let of_string = of_string;;


let fprintf ppf n = Format.fprintf ppf "%s" (to_string n);;
let fprintfx ppf n = Format.fprintf ppf "@{<integer>%s@}" (to_string n);;
