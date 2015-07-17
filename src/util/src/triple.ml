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
let fst (x,_,_) = x;;
let snd (_,x,_) = x;;
let thd (_,_,x) = x;;
let create x = (x,x,x);;
let flip (x,y,z) = (z,y,x);;
let make x y z = (x,y,z);;
let triple (f,g,h) x = (f x,g x,h x);;
let uncurry f (x,y,z) = f x y z;;
let insert_fst x (y,z) = (x,y,z);;
let insert_snd y (x,z) = (x,y,z);;
let insert_thd z (x,y) = (x,y,z);;
let drop_fst (x,y,z) = (y,z);;
let drop_snd (x,y,z) = (x,z);;
let drop_thd (x,y,z) = (x,y);;
let replace_fst x (_,y,z) = (x,y,z);;
let replace_snd y (x,_,z) = (x,y,z);;
let replace_thd z (x,y,_) = (x,y,z);;

(* Iterators *)
let cross (f,g,h) (x,y,z) = (f x,g y,h z);;
let apply f g = cross <.> make f g;;
let map f = cross (create f);;
let fold f g h d (x,y,z) = h z (g y (f x d));;

(* Scan Functions *)
let for_all f g h (x,y,z) = f x && g y && h z;;
let exists f g h (x,y,z) = f x || g y || h z;;

(* Printers *)
let fprintf f g h fmt (x,y,z) =
 Format.fprintf fmt "@[(%a,%a,%a)@]" f x g y h z
;;

let to_string f g h =
 let f fmt x = Format.fprintf fmt "%s" (f x) in
 let g fmt y = Format.fprintf fmt "%s" (g y) in
 let h fmt z = Format.fprintf fmt "%s" (h z) in
 Format.flush_str_formatter <.> fprintf f g h Format.str_formatter
;;
