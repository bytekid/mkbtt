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

(* natural numbers in binary representation, bits are little endian, i.e.,
[0;0;1] is 4 *)

(*** MODULES ******************************************************************)
module M = Monad;;
open Monad;;
open Formula;;
open Util;;

(*** TYPES ********************************************************************)
type t = BinType.nat;;

(*** FUNCTIONS ****************************************************************)
let make = id;;
let bits = id;;

let zero = make [bot];;
let one = make [top];;

let is_zero = (=) zero;;
let is_one = (=) one;;

(*** conversions **************************************************************)
let fprintf ppf a = Format.fprintf ppf "@[[%a]@]" (List.fprintf Formula.fprintf_p ",") a

let rec to_string a = Format.fprintf Format.str_formatter "%a" fprintf a;
 Format.flush_str_formatter ();;

(*
let rec of_nat n res = match n with
  | 0 -> bot::res
  | 1 -> top::res
  | n -> 
   let bit = if (n mod 2) = 0 then bot else top in
   of_nat (n / 2) (bit::res)
;;

let of_nat n =
 assert (Integer.ge n 0);
 let r = List.rev (of_nat n []) in
 assert (r <> []);
 r
;;

let of_number n = of_nat n;;
*)

let rec of_nat n res =
 let zero = Integer.zero in
 let one = Integer.one in
 let two = Integer.of_int 2 in
 if Integer.eq n zero then bot::res
 else if Integer.eq n one then top::res
 else 
  let bit = if (Integer.eq zero (Integer.(mod) n two))
   then bot else top in
  of_nat (Integer.div n two) (bit::res)
;;

let of_nat n =
 assert (Integer.ge n Integer.zero);
 let r = List.rev (of_nat n []) in
 assert (r <> []);
 r
;;

let of_number n = of_nat n;;
let of_nat = of_number;;

let sub _ = failwith "not implemented";;

(* half adder *)
let ha a b = (a <&> b, xor a b);;

(* full adder *)
let fa a b c = 
 let c0,s0 = ha a b in
 let c1,s1 = ha s0 c in
 (c0 <|> c1, s1)
;;

let rec same_length ax bx = function
 | a::al, b::bl -> same_length (a::ax) (b::bx) (al, bl)
 | [], b::bl -> same_length (bot::ax) (b::bx) ([], bl)
 | a::al, [] -> same_length (a::ax) (bot::bx) (al, [])
 | [], [] -> (List.rev ax, List.rev bx)
;;

let same_length a b = same_length [] [] (a, b);;
let slz a b = (uncurry List.zip (same_length a b));;

(*** comparisons **************************************************************)
let gt a b =
 let gt_acc (a,b) (a0,a1) =
  (a0 <|> (a1 <&> (a <&> (~!b))), (a1 <&> (a <<-> b))) in
 return (fst (List.fold_right gt_acc (slz a b) (bot,top)))
;;
let eq a b = return (big_and (List.map (uncurry (<<->>)) (slz a b)));;
let ge a b = lift2 (<|>) (gt a b) (eq a b);;

(*** arithmetical operations **************************************************)
let bit_multiply bit a = List.map ((<&>) bit) a;;

let add_acc m (a, b) = m >>= fun (c, acc) ->
 let (c,s) = fa a b c in
 M.use_fresh_bit c >>= fun c ->
 M.use_fresh_bit s >>= fun s ->
 return (c,(s::acc))
;;

let add_aux obits a b = 
 List.fold_left add_acc (return (bot, [])) (slz a b) >>= fun (c, sum) ->
 let (sum, o) = List.split_at obits (List.rev (c::sum)) in
 M.add_side_constraint (big_and (List.map neg o)) >>
 return sum
;;

let add ?(obits=max_int) a b =
 if is_zero a then return b else if is_zero b then return a else
 add_aux obits a b;;

let mul_aux obits a b =
 fst (List.fold_left 
  (fun (acc,shift) bit ->
   ((acc >>= add ~obits:obits (shift@(bit_multiply bit a))), bot::shift))
  (return zero,[])
  b)
;;

let mul ?(obits=max_int) a b =
 if is_zero a || is_zero b then return zero else
 if is_one a then return b else if is_one b then return a else
 mul_aux obits a b;;

let scale n a = 
 let rec scale n a = 
  if Integer.is_one n then return a else
  (*TODO: uncommenting below line results in a bug, I do not know why *)
  (* if n > 0 && n mod 2 = 0 then scale (n/2) (bot::a) else *)
  mul (of_nat n) a
 in
 assert (Integer.ge n Integer.zero);
 if Integer.is_zero n then return zero else scale n a
;;

let one_complement b = List.map (~!) b;;

let two_complement bl = 
 let inc l =
  let add_carry = (fun (r, c) b -> ((xor b c) :: r, c <&> b)) in
  List.fold_left add_carry ([], top) l 
 in
 List.rev (fst (inc (one_complement bl)))
;;

let ite x a b = add (bit_multiply x a) (bit_multiply (~!x) b);;

(*** fresh arithmetical variables *********************************************)
 let rec get_n acc n =
 if n = 0 then return acc
 else fresh_id >>= fun x -> (get_n (prop x::acc) (n-1))
;;

let fresh spec = get_n [] (Int.bits_int64 spec.min) >>= (return <.> make)

let rec ufb m = function 
 | [] -> m >>= (return <.> List.rev)
 | b::bs -> m >>= fun acc -> 
  M.use_fresh_bit b >>= fun c -> ufb (return (c::acc)) bs
;;

let use_fresh_bits a = ufb (return []) a;;

(*** evaluation ***************************************************************)
let rec eval acc base ass = function
 | [] -> acc
 | b::bs ->
  let x = if Assignment.val_of b ass then base else 0 in
  eval (x+acc) (base*2) ass bs
;;

let eval a ass = eval 0 1 ass a;;

