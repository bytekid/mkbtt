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
 open Util;;

(*** TYPES ********************************************************************)
 type t = Integer.t * Integer.t;;

(*** GLOBALS ******************************************************************)
let one = (Integer.one, Integer.one);;
let two = (Integer.two, Integer.one);;
let zero = (Integer.zero, Integer.one);;

(*** FUNCTIONS ****************************************************************)
let fprintf ppf (n, d) =
 if Integer.is_zero n || Integer.is_one d then
  Integer.fprintf ppf n
 else
  Format.fprintf ppf "%a/%a" Integer.fprintf n Integer.fprintf d
;;

let fprintfx ppf (n, d) =
 if Integer.is_zero n || Integer.is_one d then Integer.fprintfx ppf n
 else Format.fprintf ppf
  "@{<rational>@{<numerator>%a@}@{<denominator>%a@}@}"
  Integer.fprintf n Integer.fprintf d
;;

let to_string r = 
 fprintf Format.str_formatter r;
 Format.flush_str_formatter ();
;;

let is_zero (n, d) = Integer.is_zero n;;
let is_one (n, d) = Integer.eq n d;;

let cancel (n, d) = 
 let gcd = Integer.gcd n d in
 let n, d = (Integer.div n gcd, Integer.div d gcd) in
 if Integer.gt Integer.zero d then (Integer.minus n, Integer.minus d) else (n, d)
;;

let ( * ) = Integer.mul;;
let ( + ) = Integer.add;;

let compare (n0, d0) (n1, d1) = Pervasives.compare (n0*d1) (n1*d0);;

(*conversion*)
let of_int n = (Integer.of_int n,Integer.one);;
let of_rat n d = 
 if d = 0 then failwith "denumerator zero";
 cancel (Integer.of_int n, Integer.of_int d);;

let of_big_int n = (Integer.of_big_int n,Integer.one);;
let of_big_rat n d = 
 let n, d = Integer.of_big_int n, Integer.of_big_int d in
 if Integer.eq d Integer.zero then failwith "denumerator zero";
 cancel (n, d);;

let of_64_int n = (Integer.of_64_int n,Integer.one);;
let of_64_rat n d = 
 let n, d = Integer.of_64_int n, Integer.of_64_int d in
 if Integer.eq d Integer.zero then failwith "denumerator zero";
 cancel (n, d);;

let of_int_string s = (Integer.of_string s, Integer.one);;
let get_minf _ = failwith "not supported";;

let to_int (n, m) = 
 if Integer.eq m Integer.one then Integer.to_int n else failwith 
  (* (Format.sprintf "%s is not an integer" (to_string (n,m)));; *)
  "not an integer"
let to_rat (n, d) = (Integer.to_big_int n, Integer.to_big_int d);;
let to_real (n, d) = ((n, d),zero);;

(*arithmetic*)
let minus (n,d) = (Integer.minus n,d);;
let add (n0, d0) (n1, d1) = cancel (n0*d1 + n1*d0, d0*d1);;
let sub a (n, d) = cancel (add a (Integer.minus n, d));;
let mul (n0, d0) (n1, d1) = cancel (n0*n1, d0*d1);;
let div n (n1, d1) = mul n (d1,n1);;
let scale a (n, d) = cancel (a*n, d);;

(*comparisons*) (* precondition: denumerator greater zero *)
let eq (n0, d0) (n1, d1) = Integer.eq (n0*d1) (n1*d0);;
let gt (n0, d0) (n1, d1) = Integer.gt (n0*d1) (n1*d0);;
let ge a b = eq a b || gt a b;;

(*min/max*)
let min a b = if gt a b then b else a;;
let max a b = if gt a b then a else b;;

(*** TESTS ********************************************************************)
(* [test ()] should perform a nice test suite for the module under 
consideration *)
let test () = 
 Format.printf "Testing module Rat ...@\n";
 let n = 10 in
 let ns = List.gen (fun n -> of_rat n 2) n in
 Format.printf "%s@\n" (List.join to_string ", " ns);
 let a, b = of_rat 4 ~-5, of_rat 0 2 in
 Format.printf "a = %s, b = %s@\n" (to_string a) (to_string b);
 assert (gt b a);
;;

