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

open Util;;

(*** TYPES ********************************************************************)
type t = (Real.t * bool);;

(*** GLOBALS ******************************************************************)
let one = (Real.one, false);;
let two = (Real.two, false);;
let zero = (Real.zero, false);;
let minf = (Real.zero, true);;
let get_minf () = minf;;

let is_zero (c,d) = Real.is_zero c && not d;;
let is_one (c,d) = Real.is_one c && not d;;
let is_minf (c,d) = d;;
(*** FUNCTIONS ****************************************************************)

(*conversion*)
let of_int n    = (Real.of_int n, false);;
let of_rat n d  = (Real.of_rat n d, false);;
let of_real ?(base=2) c d = (Real.of_real c d ~base:base, false);;
let of_int_string s = (Real.of_int_string s, false);;

let of_big_int n    = (Real.of_big_int n, false);;
let of_big_rat n d  = (Real.of_big_rat n d, false);; 
let of_big_real ?(base=2) c d = (Real.of_big_real ~base:base c d, false);; 

let of_64_int n    = (Real.of_64_int n, false);;
let of_64_rat n d  = (Real.of_64_rat n d, false);; 
let of_64_real ?(base=2) c d = (Real.of_64_real ~base:base c d, false);; 

let to_real (c,d) = if not d then Real.to_real c 
 else failwith "number not a real";;
let to_rat (c,d) = if not d then Real.to_rat c 
 else failwith "number not a real";;
let to_int (c,d) = if not d then Real.to_int c
 else failwith "number not a real";;

(*arithmetic*)
let minus (c,d) = assert (not d); (Real.minus c,false);;
let add (c0,d0) (c1,d1) = (Real.add c0 c1,d0 || d1);;
let sub (c0,d0) (c1,d1) = assert (not d0 && not d1); (Real.sub c0 c1,false);;
let mul (c0,d0) (c1,d1) = assert (not d0 && not d1); (Real.mul c0 c1,false);;
let div (c0,d0) (c1,d1) = assert (not d0 && not d1); (Real.div c0 c1,false);;
let scale a (c,d) = assert (not d); 
 let a = Integer.of_big_int a in
 (Real.scale a c,d);;

(*comparisons*) (* precondition: denumerator greater zero *)
let eq (c0,d0) (c1,d1) = (d0 = d1) && (((not d0) && Real.eq c0 c1) || d0);;
let gt (c0,d0) (c1,d1) = d1 || ((not d0) && Real.gt c0 c1);;
let ge a b = gt a b || eq a b;;
let compare a b = if eq a b then 0 else if gt a b then 1 else ~-1;;

(*min/max*)
let min a b = if gt a b then a else b;;
let max a b = if gt a b then a else b;;

let fprintf ppf (c,d) =
 if d then Format.fprintf ppf "-&" else Real.fprintf ppf c;;

let fprintfx ppf (c,d) =
 if d then Format.fprintf ppf "<minusInfinity/>" else Real.fprintfx ppf c;;
  
let to_string r = 
 fprintf Format.str_formatter r;
 Format.flush_str_formatter ();;

let fprintf_complicated ppf (s,(ms,xs,const),l,_,r) =
 Format.fprintf ppf "Logic.Number.fprintf_intp: to implement";
;;

let fprintf_intp ppf (fs, arity, ms, const) =
 let xs = List.gen (fun i -> Format.sprintf "x%d" i) arity in
 let lp, rp = if xs = [] then ("", "") else ("(", ")") in
 let xlist = List.join id ", " xs in
 let s = Format.sprintf "[%s]%s%s%s = " fs lp xlist rp in
 fprintf_complicated ppf (s,(ms,xs,const),"",None,"")
;;

let fprintfx_intp _ ppf (fs, arity, ms, const) =
 Format.fprintf ppf "Logic.Number.fprintfx_intp: to implement";
;;

(*** TESTS ********************************************************************)
(* [test ()] should perform a nice test suite for the modul under 
consideration *)
let test () = 
 Format.printf "Testing module Minf ...@\n";
 (*
 let n = 10 in
 let ns = List.init n (fun n -> of_real n (2*n)) in
 Format.printf "%s@\n" (Listx.join to_string "\n" ns);
 let a, b = of_real 4 ~-5, of_real 0 2 in
 Format.printf "a = %s, b = %s@\n" (to_string a) (to_string b);
 assert (gt b a);
 *)
;;
