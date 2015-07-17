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

(*** MODULE TYPES *************************************************************)
module type PAIR = sig
 val fst : 'a * 'b -> 'a
 val snd : 'a * 'b -> 'b
end

(*** FUNCTIONS ****************************************************************)
let fst = fst;;
let snd = snd;;

(* Constructors and Destructors *)
let create x = (x,x);;
let flip (x,y) = (y,x);;
let make x y = (x,y);;
let pair (f,g) x = (f x,g x);;
let uncurry f (x,y) = f x y;;
let assocr ((x,y),z) = (x,(y,z));;
let assocl (x,(y,z)) = ((x,y),z);;

(* Iterators *)
let cross (f,g) (x,y) = (f x,g y);;
let apply f = cross <.> make f;;
let map f = cross (create f);;
let fold f g d (x,y) = g y (f x d);;

(* Scan Functions *)
let for_all f g (x,y) = f x && g y;;
let exists f g (x,y) = f x || g y;;

(* Printers *)
let fprintf f g fmt (x,y) = Format.fprintf fmt "@[(%a,%a)@]" f x g y;;

let to_string f g =
 let f fmt x = Format.fprintf fmt "%s" (f x) in
 let g fmt y = Format.fprintf fmt "%s" (g y) in
 Format.flush_str_formatter <.> fprintf f g Format.str_formatter
;;
