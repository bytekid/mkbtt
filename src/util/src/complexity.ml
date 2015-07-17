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

(*** MODULES ******************************************************************)
module F = Format;;
module O = Option;;

(*** TYPES ********************************************************************)
type t = Other | Poly of int option;;

(*** EXCEPTIONS ***************************************************************)
exception Undefined;;

(*** FUNCTIONS ****************************************************************)
(* Constructors *)
let constant = Poly (Some 0);;
let linear = Poly (Some 1);;
let other = Other;;
let poly n = Poly n;;

(* Predicates *)
let is_constant = function Poly (Some n) -> n = 0 | _ -> false;;
let is_linear = function Poly (Some n) -> n = 1 | _ -> false;;
let is_other = function Other -> true | _ -> false;;
let is_poly = function Poly (Some n) -> n > 1 | _ -> false;;

(* Miscellaneous *)
let decrease = function
 | Other -> raise Undefined
 | Poly n -> match n with
  | None -> raise Undefined
  | Some n -> Poly (Some (max 0 (n-1)))
;;

let increase = function
 | Other -> raise Undefined
 | Poly n -> match n with
  | None -> raise Undefined
  | Some n -> Poly (Some (n+1))
;;

(* Combining Complexities *)
let max c c' = match (c,c') with
 | _, Other | Other, _ -> Other
 | Poly n, Poly m -> Poly (O.fold (flip O.map m <.> max) None n)
;;

let add = max;;

let mul c c' = match (c,c') with
 | _, Other | Other, _ -> Other
 | Poly n, Poly m -> match (n,m) with
  | None, _ | _, None -> Poly None
  | Some n, Some m ->
   let max = Pervasives.max in
   Poly (Some (if n = 0 || m = 0 then max n m else n * m))
;;

let sub c c' = match (c,c') with
 | Other, _ -> Other
 | _, Other -> raise Undefined
 | Poly n, Poly m ->
  let sub n m = if n < m then raise Undefined else n in
  Poly (O.fold (flip O.map m <.> sub) None n)
;;

(* Compare Functions *)
let compare c c' = match (c,c') with
 | Other, Other -> 0
 | Other, _ -> 1
 | _, Other -> ~-1
 | Poly n, Poly m -> match (n,m) with
  | None, None -> 0
  | None, _ -> 1
  | _, None -> ~-1
  | Some n, Some m -> compare n m
;;

let equal c c' = compare c c' = 0;;
let (<) c c' = compare c c' < 0;;
let (<=) c c' = compare c c' <= 0;;
let (>) c c' = compare c c' > 0;;
let (>=) c c' = compare c c' >= 0;;

(* Printers *)
let fprintf ?(short = false) fmt = function
 | Other -> F.fprintf fmt "?"
 | Poly n ->
  let shortform n = if n = 0 then "0" else F.sprintf "%i" n in
  let standard n = if n = 0 then "O(1)" else F.sprintf "O(n^%i)" n in
  let to_string = if short then shortform else standard in
  F.fprintf fmt "%s" (O.fold to_string "POLY" n)
;;

let to_string ?(short = false) =
 F.flush_str_formatter <.> fprintf ~short:short F.str_formatter
;;

(* Parser *)
(* very nice Harald ;-) *)
let bound = function
 | "?" -> Some (Other)
 | "O(1)" -> Some (linear)
 | "O(n^1)" -> Some (Poly (Some 1))
 | "O(n^2)" -> Some (Poly (Some 2))
 | "O(n^3)" -> Some (Poly (Some 3))
 | "O(n^4)" -> Some (Poly (Some 4))
 | "O(n^5)" -> Some (Poly (Some 5))
 | "O(n^6)" -> Some (Poly (Some 6))
 | "O(n^7)" -> Some (Poly (Some 7))
 | "O(n^8)" -> Some (Poly (Some 8))
 | "POLY" -> Some (Poly None)
 | "" -> None
 | _ -> Some (Other)
;;

let of_answer a = 
 let a = Stringx.trim a in
 let lp s = String.index s '(' + 1 in
 let rp s = String.index s ')' in
 let co s = String.index s ',' in
 let left = try String.sub a (lp a) (co a - lp a) with | Not_found -> "" in
 let upper = try String.sub a (co a + 1) (String.length a - co a - 2) with | Not_found -> "" in
 (bound left,bound upper)
;;
