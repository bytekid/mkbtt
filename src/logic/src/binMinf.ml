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
open Monad;;
open Formula;;
open Util;;

(*** TYPES ********************************************************************)
type t = BinType.minf;;

(*** EXCEPTIONS ***************************************************************)

(*** SUBMODULES ***************************************************************)

(*** FUNCTIONS ****************************************************************)
let zero = (BinReal.zero, bot);;
let one  = (BinReal.one, bot);;
let minf = (BinReal.zero, top);;

(*let is_zero = (=) zero;; *)
(*let is_one = (=) one;; *)
(*let is_minf = (=) minf;; *)
(*** conversions **************************************************************)
let fprintf ppf (r, m) =
 Format.fprintf ppf "(%a,%a)" BinReal.fprintf r Formula.fprintf_p m;;

let to_string a = 
 Format.fprintf Format.str_formatter "%a" fprintf a;
 Format.flush_str_formatter ();;

let of_number (a,m) = (BinReal.of_real a, if m then top else bot);;

(*** fresh arithmetical variables *********************************************)
let fresh a =
 BinReal.fresh a >>= fun c -> 
 (if a.minf then fresh_bool else return bot) >>= fun d ->
 (* number part zero if minf *)
 (* BinReal.eq c BinReal.zero >>= fun z -> *)
 (* M.add_side_constraint (d <->> z) >> *)
 return (c, d);; 

let use_fresh_bits (n,d) = BinReal.use_fresh_bits n >>= fun n -> 
 M.use_fresh_bit d >>= fun d ->
 return (n, d);;

(*** comparisons **************************************************************)
let ge ?(obits=max_int) (r0, m0) (r1, m1) = BinReal.ge ~obits:obits r0 r1 >>= fun g ->
 return (m1 <|> (big_and [g; ~!m0]));;

let gt ?(obits=max_int) (r0, m0) (r1, m1) = BinReal.gt ~obits:obits r0 r1 >>= fun g ->
 return (m1 <|> (big_and [g; ~!m0; ~!m1]));;

let eq (r0, m0) (r1, m1) = BinReal.eq r0 r1 >>= fun e ->
 return ((m0 <&> m1) <|> (big_and [e; ~!m0; ~!m1]));;

(*** arithmetical operations **************************************************)
let bit_multiply bit (r, m) = (BinReal.bit_multiply bit r, m <&> bit);;

let add ?(obits=max_int) (r0,m0) (r1,m1) =
 if m0 = top || m1 = top then return minf else
 BinReal.add ~obits:obits r0 r1 >>= fun s -> return (s, m0 <|> m1);;

let mul ?(obits=max_int) (r0,m0) (r1,m1) =
 assert (m0 = bot && m1 = bot);
 BinReal.mul ~obits:obits r0 r1 >>= fun m -> return (m, bot);;

let sub ?(obits = -1) (r0, m0) (r1, m1) = 
 assert (m0 = bot && m1 = bot);
 BinReal.sub r0 r1 >>= fun d -> return (d, bot);;

let bits_n (r,_) = BinReal.bits_n r;;

let ite x a b = 
let obits = uncurry Pervasives.max (Pair.map bits_n (a,b)) in
add ~obits:obits (bit_multiply x a) (bit_multiply (neg x) b);;
let max a b = gt a b >>= fun x -> ite x a b;;
let min a b = gt a b >>= fun x -> ite x b a;;

(*** evaluation ***************************************************************)
let eval (r, m) ass = (BinReal.eval r ass, Assignment.val_of m ass);;
