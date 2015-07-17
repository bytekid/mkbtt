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
module type DOMAIN = sig
 type t

 val compare : t -> t -> int
 val fprintf : Format.formatter -> t -> unit
end

module type RANGE = DOMAIN;;

module type SIGNATURE = sig
 type domain
 type range
 type t

 exception Inconsistent

 val add : domain -> range -> t -> t
 val copy : t -> t
 val empty : t
 val of_list : (domain * range) list -> t
 val remove : domain -> t -> t
 val replace : domain -> range -> t -> t
 val singleton : domain -> range -> t
 val to_list : t -> (domain * range) list
 val apply : domain -> range -> t -> range
 val find : domain -> t -> range
 val search: (domain -> range -> bool) -> t -> domain * range
 val fold : (domain -> range -> 'a -> 'a) -> t -> 'a -> 'a
 val foldi : (int -> domain -> range -> 'a -> 'a) -> t -> 'a -> 'a
 val iter : (domain -> range -> unit) -> t -> unit
 val iteri : (int -> domain -> range -> unit) -> t -> unit
 val map : (range -> range) -> t -> t
 val mapi : (domain -> range -> range) -> t -> t
 val exists : (domain -> range -> bool) -> t -> bool
 val for_all : (domain -> range -> bool) -> t -> bool
 val mem : domain -> t -> bool
 val is_bijective : t -> bool
 val is_empty : t -> bool
 val is_injective : t -> bool
 val compose : (t -> range -> range) -> t -> t -> t
 val domain : t -> domain list
 val power : int -> (t -> range -> range) -> t -> t
 val range : t -> range list
 val size : t -> int
 val union : t -> t -> t
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val fprintf : Format.formatter -> t -> unit
 val to_string : t -> string
end

(*** EXCEPTIONS ***************************************************************)
exception Inconsistent;;

(*** MODULES ******************************************************************)
module Make (D : DOMAIN) (R : RANGE) = struct
 (*** INCLUDES ****************************************************************)
 include Mapx.Make (D) (R);;

 (*** EXCEPTIONS **************************************************************)
 exception Inconsistent = Inconsistent;;
 exception Not_injective;;
 
 (*** FUNCTIONS ***************************************************************)
 (* Search Functions *)
 let apply k e r = catch Not_found (find k) e r;;

 (* Constructors *)
 let add k e r =
  if mem k r && not (R.equal (find k r) e) then raise Inconsistent
  else replace k e r
 ;;

 let of_list = Listx.foldl (flip (Pair.uncurry add)) empty;;

 (* Properties *)
 let is_injective r =
  let f k e l =
   if Listx.mem ~c:R.compare e l then raise Not_injective else e :: l
  in
  try const true (fold f r []) with Not_injective -> false
 ;;

 let is_bijective = is_injective;;

 (* Miscellaneous *)
 let compose f r r' =
  let r = fold (fun k e r -> add k (f r' e) r) r empty in
  fold (fun k e r' -> if mem k r then r' else add k e r') r' r
 ;;

 let power n f r =
  let rec power n r' = if n > 0 then power (n - 1) (compose f r' r) else r' in
  power n empty
 ;;

 let union = flip (fold add);;
end
