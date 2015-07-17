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

module type PARTIAL = sig
  type domain
  type 'a t

  val add: domain -> 'a -> 'a t -> 'a t
  val empty: 'a t
  val remove: domain -> 'a t -> 'a t
  val find: domain -> 'a t -> 'a
  val fold: (domain -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter: (domain -> 'a -> unit) -> 'a t -> unit
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (domain -> 'a -> 'b) -> 'a t -> 'b t
  val mem:  domain -> 'a t -> bool
  val is_empty: 'a t -> bool
  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module type SIGNATURE = sig
 type domain
 type range
 type t

 val add : domain -> range -> t -> t
 val copy : t -> t
 val empty : t
 val of_list : (domain * range) list -> t
 val remove : domain -> t -> t
 val replace : domain -> range -> t -> t
 val singleton : domain -> range -> t
 val to_list : t -> (domain * range) list
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
 val is_empty : t -> bool
 val domain : t -> domain list
 val range : t -> range list
 val size : t -> int
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val fprintf : Format.formatter -> t -> unit
 val to_string : t -> string
end

(*** MODULES ******************************************************************)
module Make (D : DOMAIN) (R : RANGE) = struct
 (*** MODULES *****************************************************************)
 module R = struct
  include R;;

  let equal x y = R.compare x y = 0;;
 end

 module M = Map.Make (D);;

 (*** TYPES *******************************************************************)
 type domain = D.t;;
 type range = R.t;;
 type t = range M.t;;

 (*** EXCEPTIONS **************************************************************)
 exception Fail;;
 exception Found of domain * range;;

 (*** FUNCTIONS ***************************************************************)
 (* Iterators *)
 let fold = M.fold;;
 let foldi f m d = snd (M.fold (fun k e (i,x) -> (i + 1,f i k e x)) m (0,d));;
 let iter = M.iter;;
 let iteri f = const () <.> flip (M.fold (fun k e i -> f i k e; i + 1)) 0;;
 let map = M.map;;
 let mapi = M.mapi;;

 (* Search Functions *)
 let find = M.find;;

 let search p m =
  try
   iter (fun k e -> if p k e then raise (Found (k,e)) else ()) m;
   raise Not_found
  with Found (k,e) -> (k,e)
 ;;

 (* Scan Functions *)
 let for_all p m =
  let p k e = if p k e then () else raise Fail in
  try iter p m; true with Fail -> false
 ;;

 let exists p = not <.> for_all (fun k e -> not (p k e));;
 let mem = M.mem;;

 (* Compare Functions *)
 let compare = M.compare R.compare;;
 let equal = M.equal R.equal;;

 (* Constructors *)
 let add k e m = if mem k m then m else M.add k e m;;
 let copy = id;;
 let empty = M.empty;;
 let of_list xs = Listx.foldl (flip (Pair.uncurry add)) empty xs;;
 let remove = M.remove;;
 let replace = M.add;;
 let singleton k e = add k e empty;;
 let to_list m = fold (curry Listx.cons) m [];;

 (* Properties *)
 let is_empty = M.is_empty;;

 (* Miscallenous *)
 let domain m = Listx.unique ~c:D.compare (fold (drop Listx.cons) m []);;
 let range m = Listx.unique ~c:R.compare (fold (const Listx.cons) m []);;
 let size m = fold (const (const succ)) m 0;;

 (* Printers *)
 let fprintf fmt =
  let fprintf fmt m =
   iteri (fun i k e ->
    let fs =
     if i > 0 then format_of_string "@\n@[%a@ ->@ %a@]"
     else format_of_string "@[%a@ ->@ %a@]"
    in
    Format.fprintf fmt fs D.fprintf k R.fprintf e) m
  in
  Format.fprintf fmt "@[%a@]" fprintf
 ;;

 let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;
end

module Partial (D : DOMAIN) = struct
 (*** INCLUDES ****************************************************************)
 include Map.Make (D);;

 (*** TYPES *******************************************************************)
 type domain = key;;
end
