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
(* c + sqrt(2)*d *)
type t = BinType.real;;

(*** FUNCTIONS ****************************************************************)
let zero = (BinRat.zero, BinRat.zero);;
let one = (BinRat.one, BinRat.zero);;

(*** conversions **************************************************************)
let fprintf ppf (c, d) = Format.fprintf ppf "(%a,%a*sqrt(2))" 
 BinRat.fprintf c BinRat.fprintf d;;

(*TODO: do not throw away base*)
let of_real (c,d,b) = assert (b = 2);
 (BinRat.of_rat c, BinRat.of_rat d)
 ;;
(* let of_number (c,d) = of_real (c,d);; *)

let of_binRat r = r >>= fun x -> return (x, BinRat.zero);;

(*** comparisons **************************************************************)

(*
let under b = 
 if BinRat.sign b = bot then (*speedup for non-negative*)
  return (BinRat.of_int 5)
 else 
 BinRat.ge b BinRat.zero >>= fun x ->
 (BinRat.ite x (BinRat.of_int 5) (BinRat.of_int 6)) ;;

let over d = 
 if BinRat.sign d = bot then (*speedup for non-negative*)
  return (BinRat.of_int 6) 
 else 
 BinRat.ge d BinRat.zero >>= fun x ->
 (BinRat.ite x (BinRat.of_int 6) (BinRat.of_int 5)) ;;

 (*lpar-16 version, not sound at negative positions *)
let gt ?(obits=max_int) (c0,d0) (c1,d1) = 
 if BinRat.is_zero d0 && BinRat.is_zero d1 then BinRat.gt c0 c1 else
 under d0 >>= fun u -> 
 BinRat.scale (Integer.of_int 4) c0 >>= fun l0 ->
 (* BinRat.scale (Integer.of_int 5) d0 >>= fun l1 -> *)
 BinRat.mul ~obits:obits u d0 >>= fun l1 ->
 over d1 >>= fun o ->
 BinRat.scale (Integer.of_int 4) c1 >>= fun r0 ->
 (* BinRat.scale (Integer.of_int 6) d1 >>= fun r1 -> *)
 BinRat.mul ~obits:obits o d1 >>= fun r1 ->
 BinRat.add ~obits:obits l0 l1 >>= fun l ->
 BinRat.add ~obits:obits r0 r1 >>= fun r ->
 BinRat.gt l r;;
*)

(**)
(*scss 2010 version, exact *)
let gt ?(obits=max_int) (c1,c2) (d1,d2) = 
 if BinRat.is_zero c2 && BinRat.is_zero d2 then BinRat.gt c1 d1 
 else
 let foo f ma mb = ma >>= fun a -> mb >>= fun b -> f a b in
 let (<*>) = foo (BinRat.mul ~obits:obits) in
 let (<+>) = foo (BinRat.add ~obits:obits) in
 let (<>=>) = foo (BinRat.ge) and (<<=>) = flip (foo BinRat.ge) in
 let (<>>) = foo (BinRat.gt) and (<<>) = flip (foo BinRat.gt) in
 let ($|$) = lift2 (<|>) and ($&$) = lift2 (<&>) in
 let m = return (BinRat.of_int 2) and two = return (BinRat.of_int 2) in
 let c1 = return c1 and c2 = return c2 and d1 = return d1 and d2 = return d2 in
 let c op = op ((c1<*>c1) <+> (d1<*>d1) <+> (two<*>c2<*>d2<*>m))
               ((c2<*>c2<*>m) <+> (d2<*>d2<*>m) <+> (two<*>c1<*>d1)) in
 ((c1 <>> d1)  $&$ (d2 <<=> c2)) $|$
 ((c1 <>=> d1) $&$ (d2 <<> c2)) $|$
 ((c1 <>=> d1) $&$ (d2 <>=> c2) $&$ (c (<>>))) $|$
 ((c1 <<=> d1) $&$ (d2 <<=> c2) $&$ (c (<<>)))
;;
(**)

let eq (c0,d0) (c1,d1) = lift2 (<&>) (BinRat.eq c0 c1) (BinRat.eq d0 d1);;

let ge ?(obits=max_int) ((c0,d0) as a) ((c1,d1) as b) = 
 if BinRat.is_zero d0 && BinRat.is_zero d1 then BinRat.ge c0 c1
 else lift2 (<|>) (gt ~obits:obits a b) (eq a b)
;;

(*** arithmetical operations **************************************************)
let bit_multiply x a = Pair.map (BinRat.bit_multiply x) a;;

let add ?(obits=max_int) (c0,d0) (c1,d1) =
 (*if BinRat.is_zero d0 && BinRat.is_zero d1 then of_binRat (BinRat.add ~obits:obits c0 c1) else *)
 BinRat.add ~obits:obits c0 c1 >>= fun sc ->
 BinRat.add ~obits:obits d0 d1 >>= fun sd ->
 return (sc,sd)
;;

let mul ?(obits=max_int) (c0, d0) (c1, d1) = 
 (*if BinRat.is_zero d0 && BinRat.is_zero d1 then of_binRat (BinRat.mul
 ~obits:obits c0 c1) else *)
 BinRat.mul ~obits:obits c0 c1 >>= fun sc0 ->
 BinRat.mul ~obits:obits d0 d1 >>= fun sc1 ->
 BinRat.mul ~obits:obits c0 d1 >>= fun sd0 ->
 BinRat.mul ~obits:obits d0 c1 >>= fun sd1 ->
 BinRat.scale (Integer.of_int 2) sc1 >>= fun x ->
 BinRat.add ~obits:obits x sc0 >>= fun c ->
 BinRat.add ~obits:obits sd0 sd1 >>= fun d ->
 return (c,d)
;;

let two_complement a = Pair.map BinRat.two_complement a;;

let sub ?(obits=max_int) a b = add ~obits:obits a (two_complement b)

let ite x a b = add (bit_multiply x a) (bit_multiply (neg x) b);;

let max a b = gt a b >>= fun x -> ite x a b;;

let bits_n a = uncurry Pervasives.max (Pair.map BinRat.bits_n a);;


(*** fresh arithmetical variables *********************************************)
let fresh a = BinRat.fresh a >>= fun c -> 
 (if a.real then BinRat.fresh a else return BinRat.zero) >>= fun d -> return (c, d);; 

let use_fresh_bits (c,d) = 
 lift2 Pair.make (BinRat.use_fresh_bits c) (BinRat.use_fresh_bits d)

(*** evaluation ***************************************************************)
(*TODO: add base *)
let eval (c,d) ass = Real.of_64_real ~base:2 (BinRat.eval c ass) (BinRat.eval d ass);;

