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

(*** TYPES ********************************************************************)
type ('a, 'b) t = ('a, 'b) either = Left of 'a | Right of 'b;;

(*** FUNCTIONS ****************************************************************)
(* Iterators *)
let map f g = function Left x -> Left (f x) | Right x -> Right (g x);;

(* Constructors and Destructors *)
let make_left x = Left x;;
let make_right x = Right x;;
let left = function Left x -> x | _ -> failwith "left no content";;
let right = function Right x -> x | _ -> failwith "right no content";;
let strip = function Left x | Right x -> x;;
let either f g = strip <.> map f g;;

(* Properties *)
let is_left = function Left _ -> true | _ -> false;;
let is_right = function Right _ -> true | _ -> false;;

(* Printers *)
let fprintf f g fmt e =
 let fprintf fmt = function Left x -> f fmt x | Right x -> g fmt x in
 Format.fprintf fmt "@[%a@]" fprintf e
;;

let to_string f g =
 let f fmt x = Format.fprintf fmt "%s" (f x) in
 let g fmt x = Format.fprintf fmt "%s" (g x) in
 Format.flush_str_formatter <.> fprintf f g Format.str_formatter
;;
