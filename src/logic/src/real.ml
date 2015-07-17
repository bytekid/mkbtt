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

(*** SUBMODULES ***************************************************************)
module Rat = Rational;;

(*** TYPES ********************************************************************)
type t = Rat.t * Rat.t * int;; (*(c,d,m) = c + sqrt(m)d*)

(*** GLOBALS ******************************************************************)
let one = (Rat.one, Rat.zero,2);;
let two = (Rat.two, Rat.zero,2);;
let zero = (Rat.zero, Rat.zero,2);;

(*** FUNCTIONS ****************************************************************)
(*TODO:extend to arbitrary *)
let is_zero (c,d,b) = assert (b >= 2);
 Rat.is_zero c && Rat.is_zero d;;
let is_one (c,d,b) = assert (b >= 2);
 Rat.is_one c && Rat.is_zero d;;

(*conversion*)
let of_int n = (Rat.of_int n,Rat.zero,2);;
let of_rat n d = (Rat.of_rat n d,Rat.zero,2);;
let of_real ?(base=2) (c0,d0) (c1,d1) = assert (base >= 2);
 (Rat.of_rat c0 d0 ,Rat.of_rat c1 d1,base);;
let of_int_string s = (Rat.of_int_string s, Rat.zero,2);;

let of_big_int n = (Rat.of_big_int n, Rat.zero,2);;
let of_big_rat n d = (Rat.of_big_rat n d,Rat.zero,2);;
let of_big_real ?(base=2) (c0,d0) (c1,d1) = assert (base >= 2);
 (Rat.of_big_rat c0 d0 ,Rat.of_big_rat c1 d1,base);;

let of_64_int n = (Rat.of_64_int n, Rat.zero,2);;
let of_64_rat n d = (Rat.of_64_rat n d,Rat.zero,2);;
let of_64_real ?(base=2) (c0,d0) (c1,d1) = assert (base >= 2);
 (Rat.of_64_rat c0 d0 ,Rat.of_64_rat c1 d1,base);;


let get_minf _ = failwith "not supported";;

let to_real (c,d,b) = (Rat.to_rat c,Rat.to_rat d,b);;
let to_rat (c,d,b) = assert (b >= 2);
 if Rat.is_zero d then Rat.to_rat c
 else failwith "number not an integer";;
let to_int (c,d,b) = assert (b >= 2); 
 if Rat.is_zero d then Rat.to_int c
 else failwith "number not an integer";;

(*arithmetic*)
let minus (c,d,b) = (Rat.minus c, Rat.minus d,b);;
let add (c0,c1,b0) (d0,d1,b1) = assert (b0=b1);
 (Rat.add c0 d0,Rat.add c1 d1,b0);;

let sub (c0,c1,b0) (d0,d1,b1) = assert (b0=b1);
 (Rat.sub c0 d0,Rat.sub c1 d1,b0);;

let mul (c0,c1,b0) (d0,d1,b1) = assert (b0=b1);
 (Rat.add (Rat.mul c0 d0) (Rat.scale (Integer.of_int b0) (Rat.mul c1 d1)),
 (Rat.add (Rat.mul c0 d1) (Rat.mul c1 d0)),b0);;
let div (c0,c1,b0) (d0,d1,b1) = assert (b0=b1);
 assert (c1 = Rat.zero && d1 = Rat.zero); 
 (Rat.div c0 d0, Rat.zero,b0)
;;
let scale a (c,d,b) = (Rat.scale a c,Rat.scale a d,b);;

(*comparisons*) (* precondition: denumerator greater zero *)
let eq (c0,c1,b0) (d0,d1,b1) = assert (b0=b1);
 (Rat.eq c0 d0 && Rat.eq c1 d1);;

(* let gt (c0,d0,b0) (c1,d1,b1) = assert (b0=b1); *)
 (* Rat.gt  *)
  (* (Rat.add (Rat.scale (Integer.of_int 4) c0) (Rat.scale
  (Integer.of_int 5) d0))  *)
  (* (Rat.add (Rat.scale (Integer.of_int 4) c1) (Rat.scale
  (Integer.of_int 6) d1));; *)
let gt c d = 
  let (c1,c2,b) = sub c d in
  if Rat.is_zero c2 then Rat.gt c1 Rat.zero else (*to prevent overflows*)
  let phi = Rat.gt (Rat.mul c1 c1) (Rat.mul (Rat.mul c2 c2) (Rat.of_int b)) in
  let chi = Rat.gt (Rat.mul (Rat.mul c2 c2) (Rat.of_int b)) (Rat.mul c1 c1) in
  (Rat.ge c1 Rat.zero && Rat.gt c2 Rat.zero) ||
  (Rat.gt c1 Rat.zero && Rat.ge c2 Rat.zero) ||
  (Rat.ge c1 Rat.zero && Rat.gt Rat.zero c2 && phi) ||
  (Rat.gt Rat.zero c1 && Rat.ge c2 Rat.zero && chi)
;;

let ge a b = eq a b || gt a b;;

(*min/max*)
let min a b = if gt a b then b else a;;
let max a b = if gt a b then a else b;;

let fprintf ppf (c,d,b) =
 let fpf_real ppf d = 
  if Rat.is_one d then Format.fprintf ppf "sqrt(%d)" b 
  else if Rat.eq d (Rat.of_int ~-1) then Format.fprintf ppf "-sqrt(%d)" b 
  else Format.fprintf ppf "%asqrt(%d)" Rat.fprintf d b
 in
 if Rat.is_zero c && Rat.is_zero d then Format.fprintf ppf "0"
 else if Rat.is_zero c then fpf_real ppf d
 else if Rat.is_zero d then Format.fprintf ppf "%a" Rat.fprintf c
 else Format.fprintf ppf "%a+%a" Rat.fprintf c fpf_real d
;;

let fprintfx ppf (c,d,b) = assert (b=2);
 assert (Rat.is_zero d);
 Rat.fprintfx ppf c
;;

let to_string r = 
 fprintf Format.str_formatter r;
 Format.flush_str_formatter ();
;;

(*** TESTS ********************************************************************)
(* [test ()] should perform a nice test suite for the modul under 
consideration *)
let test () = 
 Format.printf "Testing module Real ...@\n";
 (*
 let n = 10 in
 let ns = List.init n (fun n -> of_real n (2*n)) in
 Format.printf "%s@\n" (Listx.join to_string "\n" ns);
 let a, b = of_real 4 ~-5, of_real 0 2 in
 Format.printf "a = %s, b = %s@\n" (to_string a) (to_string b);
 assert (gt b a);
 *)
;;

