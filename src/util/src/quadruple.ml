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

(*** FUNCTIONS ****************************************************************)
(* Constructors and Destructors *)
let fst (x,_,_,_) = x;;
let snd (_,x,_,_) = x;;
let thd (_,_,x,_) = x;;
let fth (_,_,_,x) = x;;
let create x = (x,x,x,x);;
let flip (w,x,y,z) = (z,y,x,w);;
let make w x y z = (w,x,y,z);;
let quadruple (f,g,h,i) x = (f x,g x,h x,i x);;
let uncurry f (w,x,y,z) = f w x y z;;
let insert_fst w (x,y,z) = (w,x,y,z);;
let insert_snd x (w,y,z) = (w,x,y,z);;
let insert_thd y (w,x,z) = (w,x,y,z);;
let insert_fth z (w,x,y) = (w,x,y,z);;
let drop_fst (w,x,y,z) = (x,y,z);;
let drop_snd (w,x,y,z) = (w,y,z);;
let drop_thd (w,x,y,z) = (w,x,z);;
let drop_fth (w,x,y,z) = (w,x,y);;
let replace_fst w (_,x,y,z) = (w,x,y,z);;
let replace_snd x (w,_,y,z) = (w,x,y,z);;
let replace_thd y (w,x,_,z) = (w,x,y,z);;
let replace_fth z (w,x,y,_) = (w,x,y,z);;

(* Iterators *)
let cross (f,g,h,i) (w,x,y,z) = (f w,g x,h y,i z);;
let apply f g h = cross <.> make f g h;;
let map f = cross (create f);;
let fold f g h i d (w,x,y,z) = i z (h y (g x (f w d)));;

(* Scan Functions *)
let for_all f g h i (w,x,y,z) = f w && g x && h y && i z;;
let exists f g h i (w,x,y,z) = f w || g x || h y || i z;;

(* Printers *)
let fprintf f g h i fmt (w,x,y,z) =
 F.fprintf fmt "@[(%a,%a,%a,%a)@]" f w g x h y i z
;;

let to_string f g h i =
 let f fmt w = F.fprintf fmt "%s" (f w) in
 let g fmt x = F.fprintf fmt "%s" (g x) in
 let h fmt y = F.fprintf fmt "%s" (h y) in
 let i fmt z = F.fprintf fmt "%s" (i z) in
 F.flush_str_formatter <.> fprintf f g h i F.str_formatter
;;
