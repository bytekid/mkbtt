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

(* binary numbers in two-complement representation *)
(*** FUNCTIONS ****************************************************************)
module M = Monad;;
open Monad;;
open Formula;;
open Util;;

(*** TYPES ********************************************************************)
type t = BinType.bint;; 

(*** FUNCTIONS ****************************************************************)
let make sign bits = (sign, bits);;
let sign = fst;;
let bits = snd;;

let zero = make bot BinNat.zero;;
let one  = make bot BinNat.one;;
(* let is_zero c = (sign c) = bot && (BinNat.is_zero (bits c));; *)
let is_zero c = (sign c) = bot && (List.for_all (fun bit -> bit = bot) (bits c));;
let is_one c = (sign c) = bot && BinNat.is_one (bits c);;

(*** conversions **************************************************************)
let fprintf ppf a = Format.fprintf ppf "@[(%a,%a)@]"
 Formula.fprintf_p (sign a) BinNat.fprintf (bits a);;

let to_string a = Format.fprintf Format.str_formatter "%a" fprintf a;
 Format.flush_str_formatter ();;

(* first sign-extend and then compute tc then drop MSB *)
let two_complement a = 
 let tc = BinNat.two_complement (bits a@[sign a;sign a]) in
 make (List.last tc) (List.init tc)
;;

let of_int n = if Integer.ge n Integer.zero then make bot (BinNat.of_nat n) else
 two_complement (make bot (BinNat.of_nat (Integer.minus n)))
;;
let of_number n = of_int n;;

(*** helpers ******************************************************************)
let restrict obits a = 
 let (bits, o) = List.split_at obits (BinNat.bits (bits a)) in
 M.add_side_constraint (big_and (List.map ((<<->>) (sign a)) o)) >>
 return (make (sign a) bits)
;;

let sign_extend2 n a = make (sign a) (bits a @ List.replicate n (sign a));;

let sign_extend ?(offset = 0) a b = 
 let (ba,bb) = (Pair.map (List.length <.> BinNat.bits bits) (a,b)) in
 let n = Pervasives.max ba bb + offset in
 let a, b = (sign_extend2 (n-ba) a, sign_extend2 (n-bb) b) in
 assert (List.length (bits a) = List.length (bits b));
 (a,b)
;;

(*** comparisons **************************************************************)
let ge a b = 
 let a, b = sign_extend a b in
 BinNat.ge (bits a) (bits b) >>= fun x -> 
 return (((sign a <<->> sign b) <&> x) <|> ((neg (sign a)) <&> (sign b)));;

let gt a b = 
 let a, b = sign_extend a b in
 BinNat.gt (bits a) (bits b) >>= fun x -> 
 return (((sign a <<->> sign b) <&> x) <|> ((neg (sign a)) <&> (sign b)));;

let eq a b = 
 let a, b = sign_extend a b in
 (BinNat.eq (bits a) (bits b)) >>= fun x -> 
 return ((sign a <<->> sign b) <&> x);;

(*** arithmetical operations **************************************************)
let bit_multiply bit a = 
 make (sign a <&> bit) (BinNat.bit_multiply bit (bits a));;

let add ?(obits=max_int) a b = if sign a = bot && sign b = bot 
 then BinNat.add ~obits:obits (bits a) (bits b) >>= (return <.> (make bot)) else
 let a, b = sign_extend ~offset:1 a b in
 BinNat.add (bits a) (bits b) >>= fun s -> 
 let (sum,overflow) = List.split_at (List.length s - 1) (BinNat.bits s) in
 assert (List.length overflow = 1);
 let overflow = List.hd overflow in
 let r = (make (xor (xor (sign a) (sign b)) overflow) sum) in
 restrict obits r
;;

(* see: http://en.wikipedia.org/wiki/Two's_complement#Multiplication 05-27-2009 *)
let mul ?(obits=max_int) a b = if sign a = bot && sign b = bot
 then BinNat.mul ~obits:obits (bits a) (bits b) >>= (return <.> (make bot)) else
 let a, b = sign_extend ~offset:0 a b in
 let n = List.length (BinNat.bits (bits a)) in
 let a, b = sign_extend ~offset:(n+2) a b in (* no overflow using 2n+2 bits *)
 BinNat.mul (bits a) (bits b) >>= fun m ->
 let prod = List.take (2*n+2) (BinNat.bits m) in
 let r = make (List.last prod) (List.init prod) in
 restrict obits r
;;

let rec scale n a =
 if Integer.is_zero n then return zero 
 else if Integer.is_one n then return a else 
 if (sign a = bot) && Integer.gt n Integer.zero 
  then lift2 make (return (sign a)) (BinNat.scale n (bits a)) else 
 mul (of_int n) a;;

let sub a b = add a (two_complement b);;

let ite x a b = add (bit_multiply x a) (bit_multiply (~!x) b);;

let bits_n a = List.length (bits a);;

(*** fresh arithmetical variables *********************************************)
let fresh spec = BinNat.fresh spec >>= fun bits ->
 (if spec.neg then fresh_bool else return bot) >>= fun sign ->
 return (make sign bits)
;;

let use_fresh_bits a = 
 M.use_fresh_bit (sign a) >>= fun s ->
 BinNat.use_fresh_bits (bits a) >>= fun bits -> 
 return (make s bits)
;;

(*** evaluation ***************************************************************)
let rec eval_list ass = function
 | [] -> []
 | b::bs -> Assignment.val_of b ass::eval_list ass bs
;;

let b2i = function
 | false -> Integer.zero
 | true -> Integer.one
;;

let rec count acc base = function
 | [] -> acc
 | b::bs -> let b = b2i b in
 count (Integer.add acc (Integer.mul b base)) (Integer.mul base (Integer.of_int 2)) bs
;;

let count bs = count Integer.zero Integer.one bs;;

let bflip x = not x;;

let eval a ass = 
 let s, bn = Assignment.val_of (sign a) ass, eval_list ass (bits a) in
 if s then Integer.minus (Integer.succ (count (List.map bflip bn))) 
 else count bn
;;

