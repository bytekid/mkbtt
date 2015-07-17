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

(*** FUNCTIONS ****************************************************************)
(* Constructors and Destructors *)
let some x = Some x;;
let the = function None -> failwith "no content" | Some x -> x;;

(* Iterators *)
let fold f d = function None -> d | Some x -> f x;;
let option = fold;;
let map f = function None -> None | Some x -> Some (f x);;

(* Properties *)
let is_none = function None -> true | _ -> false;;
let is_some = function Some _ -> true | _ -> false;;

(* Compare Functions *)
let compare f o o' = match (o,o') with
 | None, None -> 0
 | None, Some _ -> ~-1
 | Some _, None -> 1
 | Some l, Some l' -> f l l'
;;

let equal f o o' = compare f o o' = 0;;

(* Printers *)
let fprintf f fmt o = 
 let fprintf fmt = function
  | None -> Format.fprintf fmt "None"
  | Some d -> f fmt d
 in
 Format.fprintf fmt "@[%a@]" fprintf o
;;

let to_string f =
 let f fmt = Format.fprintf fmt "%s" <.> f in
 Format.flush_str_formatter <.> fprintf f Format.str_formatter
;;
