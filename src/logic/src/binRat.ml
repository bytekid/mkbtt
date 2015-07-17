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

(*** MODULES ******************************************************************)
module M = Monad;;
module Rat = Rational;;

(*** OPENS ********************************************************************)
open Monad;;
open Formula;;
open Util;;

(*** TYPES ********************************************************************)
type t = BinType.rat;;

(*** FUNCTIONS ****************************************************************)
let zero = (BinInt.zero, Integer.one);;
let one = (BinInt.one, Integer.one);;
let of_int n = (BinInt.of_int (Integer.of_int n), Integer.one);;

let is_zero (c,_)= BinInt.is_zero c;;
let is_one (c,d) = BinInt.is_one c && Integer.eq d Integer.one;;

(*** conversions **************************************************************)
let fprintf ppf (n, d) =
 Format.fprintf ppf "(%a,%a)" BinInt.fprintf n Integer.fprintf d;;

let to_string r = Format.fprintf Format.str_formatter "%a" fprintf r;
 Format.flush_str_formatter ();;

let of_rat (n,d) = (BinInt.of_number n, d);;
let of_number (n,d) = of_rat (n,d);;
let get_minf _ = failwith "get_minf not supported";;

(*** helpers ******************************************************************)
let restrict obits a = a >>= fun (n,d) ->
 BinInt.restrict obits n >>= fun n -> return (n, d)
;;

let ( / ) = Integer.div;;
let ( + ) = Integer.add;;
let ( * ) = Integer.mul;;

let expand (n0,d0) (n1,d1) =
 let gcd = Integer.gcd d0 d1 in
 BinInt.scale (d1 / gcd) n0 >>= fun a ->
 BinInt.scale (d0 / gcd) n1 >>= fun b ->
 return (a, b, Integer.lcm d0 d1)
;;

let rec cancel a = a >>= fun (n,d) ->
 M.get_dbits () >>= fun db ->
 if Integer.gt 
   (Integer.bits (Integer.sub d (Integer.of_int 1))) 
   (Integer.of_int db) (*cancellation applies*)
   && (BinInt.bits n <> []) (*numerator can be canceled*)
   && Integer.is_zero (Integer.(mod) d (Integer.of_int 2)) (*denominator can be canceled *) then(
  let s,bs = BinInt.sign n, BinInt.bits n in
  M.add_side_constraint (neg (List.hd bs)) >>
  cancel (return (BinInt.make (BinInt.sign n) (List.tl
  bs),d/(Integer.of_int 2)))
 ) else
  return (n,d)
;;

(*** comparisons **************************************************************)
let ge a b = expand a b >>= fun (a, b, _) -> (BinInt.ge a b);;
let gt a b = expand a b >>= fun (a, b, _) -> (BinInt.gt a b);;
let eq a b = expand a b >>= fun (a, b, _) -> (BinInt.eq a b);;

(*** arithmetical operations **************************************************)
let bit_multiply bit (n,d) = (BinInt.bit_multiply bit n, d);;

let add ?(obits=max_int) a b = 
 expand a b >>= fun (a, b, d) ->
 BinInt.add ~obits:max_int a b >>= fun s ->
 restrict obits (cancel (return (s,d)))
;;

let mul ?(obits=max_int) (n0, d0) (n1, d1) = 
 BinInt.mul ~obits:max_int n0 n1 >>= fun p ->
 restrict obits (cancel (return (p,d0*d1)))
;;

(*TODO: make more efficient be reducing fraction *)
let scale c (n,d) = BinInt.scale c n >>= fun x -> return (x, d);;

let two_complement (n,d) = (BinInt.two_complement n,d);;

let sub ?(obits=max_int) a b = add ~obits:obits a (two_complement b)

let ite x a b = expand a b >>= fun (a,b,d) ->
 BinInt.ite x a b >>= fun r ->
 return (r,d)
;;

let bits_n (n,_) = (BinInt.bits_n n);;

let sign (n,_) = BinInt.sign n;;

(*** fresh arithmetical variables *********************************************)
let fresh a = BinInt.fresh a >>= fun b -> return (b, Integer.of_int a.rat);; 

let use_fresh_bits (n,d) = BinInt.use_fresh_bits n >>= fun n -> return (n, d)

(*** evaluation ***************************************************************)
let eval (n, d) ass = Rational.of_64_rat (BinInt.eval n ass) d;;

