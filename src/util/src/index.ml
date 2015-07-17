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
module type KEY = sig
 include Isomorphism.DOMAIN

 val next : t -> t
 val zero : t
end

module type ELEMENT = Isomorphism.RANGE;;

module type SIGNATURE = sig
 type key
 type element
 type t
 
 val add : key -> element -> t -> t
 val empty : t
 val index : element -> t -> t
 val of_list : (key * element) list -> t
 val replace : key -> element -> t -> t
 val to_list : t -> (key * element) list
 val fold : (key -> element -> 'a -> 'a) ->'a -> t -> 'a
 val iter : (key -> element -> unit) -> t -> unit
 val mem : key -> t -> bool
 val find : key -> t -> element
 val clear : t -> t
 val copy : t -> t
 val elements : t -> element list
 val fresh : t -> key * t
 val keys : t -> key list
 val size : t -> int
 val fprintf : Format.formatter -> t -> unit
 val to_string : t -> string
end

module type ISOMORPHIC = sig
 type key
 type element
 type t
 
 val add : key -> element -> t -> t
 val empty : int -> t
 val index : element -> t -> t
 val of_list : (key * element) list -> t
 val replace : key -> element -> t -> t
 val to_list : t -> (key * element) list
 val fold : (key -> element -> 'a -> 'a) -> 'a -> t -> 'a
 val iter : (key -> element -> unit) -> t -> unit
 val mem_elt : element -> t -> bool
 val mem_key : key -> t -> bool
 val find_elt : key -> t -> element
 val find_key : element -> t -> key
 val clear : t -> t
 val copy : t -> t
 val elements : t -> element list
 val fresh : t -> key * t
 val keys : t -> key list
 val size : t -> int
 val fprintf : Format.formatter -> t -> unit
 val to_string : t -> string
end

(*** MODULES ******************************************************************)
module Make (K : KEY) (E : ELEMENT) = struct
 (*** MODULES *****************************************************************)
 module M = Mapx.Make (K) (E);;

 (*** TYPES *******************************************************************)
 type key = K.t;;
 type element = E.t;;
 type t = {key : key; index : M.t};;

 (*** FUNCTIONS ***************************************************************)
 (* Miscellaneous *)
 let greater x y = K.compare x y > 0;;
 let incr_key i = {i with key = K.next i.key};;
 let set_key k i = {i with key = K.next k};;
 let clear _ = {key = K.zero; index = M.empty};;
 let copy i = {key = i.key; index = M.copy i.index};;
 let elements i = M.range i.index;;
 let fresh i = let k = i.key in (k,incr_key i);;
 let keys i = M.domain i.index;;
 let size i = M.size i.index;;

 (* Iterators *)
 let fold f d i = M.fold f i.index d;;
 let iter f i = M.iter f i.index;;

 (* Constructors *)
 let update f k e i =
  let i = if greater k i.key then set_key k i else i in
  {i with index = f k e i.index}
 ;;

 let add = update M.add;;
 let replace = update M.replace;;
 let empty = {key = K.zero; index = M.empty};;
 let to_list = fold (curry Listx.cons) [];;
 let of_list xs = Listx.foldl (flip (Pair.uncurry add)) empty xs;;

 let index e i =
  let k = i.key and i = incr_key i in {i with index = M.add k e i.index}
 ;;

 (* Scan Functions *)
 let mem k i = M.mem k i.index;;

 (* Search Functions *)
 let find k i = M.find k i.index;;

 (* Printers *)
 let fprintf fmt i = M.fprintf fmt i.index;;
 let to_string i = M.to_string i.index;;
end

module Isomorphic (K : KEY) (E : ELEMENT) = struct
 (*** MODULES *****************************************************************)
 module I = Isomorphism.Make (K) (E);;

 (*** TYPES *******************************************************************)
 type key = K.t;;
 type element = E.t;;
 type t = {mutable key : key; index : I.t};;

 (*** FUNCTIONS ***************************************************************)
 (* Miscellaneous *)
 let greater x y = K.compare x y > 0;;
 let incr_key i = i.key <- K.next i.key; i;;
 let set_key k i = i.key <- K.next k; i;;
 let clear i = i.key <- K.zero; {i with index = I.clear i.index};;
 let copy i = {key = i.key; index = I.copy i.index};;
 let elements i = I.range i.index;;
 let fresh i = let k = i.key in (k,incr_key i);;
 let keys i = I.domain i.index;;
 let size i = I.size i.index;;

 (* Iterators *)
 let fold f d i = I.fold f d i.index;;
 let iter f i = I.iter f i.index;;

 (* Constructors *)
 let add k e i =
  if not (I.mem_dom k i.index || I.mem_ran e i.index) then (
   let i = if greater k i.key then set_key k i else i in
   {i with index = I.add k e i.index})
  else i
 ;;

 let replace k e i =
  let i = if greater k i.key then set_key k i else i in
  {i with index = I.replace k e i.index}
 ;;

 let empty n = {key = K.zero; index = I.empty n};;
 let to_list = fold (curry Listx.cons) [];;

 let of_list xs =
  Listx.foldl (flip (Pair.uncurry add)) (empty (List.length xs)) xs
 ;;

 let index e i =
  if I.mem_ran e i.index then i
  else let k = i.key and i = incr_key i in {i with index = I.add k e i.index}
 ;;

 (* Scan Functions *)
 let mem_elt e i = I.mem_ran e i.index;;
 let mem_key k i = I.mem_dom k i.index;;

 (* Search Functions *)
 let find_elt k i = I.find_ran k i.index;;
 let find_key e i = I.find_dom e i.index;;

 (* Printers *)
 let fprintf fmt i = I.fprintf fmt i.index;;
 let to_string i = I.to_string i.index;;
end

module Default (E : ELEMENT) = Make (Int) (E);;
module Standard (E : ELEMENT) = Isomorphic (Int) (E);;
