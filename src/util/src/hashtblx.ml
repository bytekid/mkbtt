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
 val hash : t -> int
 val fprintf : Format.formatter -> t -> unit
end

module type RANGE = sig
 type t

 val compare : t -> t -> int
 val fprintf : Format.formatter -> t -> unit
end

module type HASHTBL = sig
 type ('a,'b) t

 val add : ('a,'b) t -> 'a -> 'b -> unit
 val clear : ('a,'b) t -> unit
 val copy : ('a,'b) t -> ('a,'b) t
 val create : ?random:bool -> int -> ('a,'b) t
 val remove : ('a,'b) t -> 'a -> unit
 val replace : ('a,'b) t -> 'a -> 'b -> unit
 val find : ('a,'b) t -> 'a -> 'b
 val find_all : ('a,'b) t -> 'a -> 'b list
 val fold : ('a -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c
 val iter : ('a -> 'b -> unit) -> ('a,'b) t -> unit
 val mem : ('a,'b) t -> 'a -> bool
 val length : ('a,'b) t -> int
 val hash : 'a -> int
 val hash_param : int -> int -> 'a -> int
end

module type PARTIAL = sig
 type domain
 type 'a t

 val add : 'a t -> domain -> 'a -> unit
 val clear : 'a t -> unit
 val copy : 'a t -> 'a t
 val create : int -> 'a t
 val remove : 'a t -> domain -> unit
 val replace : 'a t -> domain -> 'a -> unit
 val find : 'a t -> domain -> 'a
 val find_all : 'a t -> domain -> 'a list
 val fold : (domain -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
 val iter : (domain -> 'a -> unit) -> 'a t -> unit
 val mem : 'a t -> domain -> bool
 val length : 'a t -> int
end

module type SIGNATURE = sig
 type domain
 type range
 type t

 val add : domain -> range -> t -> t
 val clear : t -> t
 val copy : t -> t
 val create : int -> t
 val empty : t
 val of_list : (domain * range) list -> t
 val remove : domain -> t -> t
 val replace : domain -> range -> t -> t
 val singleton : domain -> range -> t
 val to_list : t -> (domain * range) list
 val find : domain -> t -> range
 val find_all : domain -> t -> range list
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
 val length : t -> int
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
 module D = struct
  include D;;

  let equal x y = D.compare x y = 0;;
 end

 module H = Hashtbl.Make (D);;

 (*** TYPES *******************************************************************)
 type domain = D.t;;
 type range = R.t;;
 type t = range H.t;;

 (*** EXCEPTIONS **************************************************************)
 exception Fail;;
 exception Found of domain * range;;

 (*** FUNCTIONS ***************************************************************)
 (* Iterators *)
 let fold = H.fold;;
 let foldi f h d = snd (H.fold (fun k e (i,x) -> (i + 1,f i k e x)) h (0,d));;
 let iter = H.iter;;
 let iteri f = const () <.> flip (H.fold (fun k e i -> f i k e; i + 1)) 0;;
 let map f h = iter (fun k -> H.replace h k <.> f) h; h;;
 let mapi f h = iter (fun k -> H.replace h k <.> f k) h; h;;

 (* Search Functions *)
 let find k h = H.find h k;;
 let find_all k h = H.find_all h k;;

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

 let exists p = not <.> for_all (fun k -> not <.> p k);;
 let mem k h = H.mem h k;;

 (* Constructors *)
 let add k e h = if mem k h then h else (H.add h k e; h);;
 let clear h = H.clear h; h;;
 let copy = H.copy;;
 let create = H.create;;
 let empty = (create 0 : t);;

 let of_list xs =
  Listx.foldl (flip (Pair.uncurry add)) (create (List.length xs)) xs
 ;;

 let remove k h = H.remove h k; h;;
 let replace k e h = H.replace h k e; h;;
 let singleton k e = add k e (create 2);;
 let to_list h = fold (curry Listx.cons) h [];;

 (* Compare Functions *)
 let compare h h' =
  let compare xs ys = match (xs,ys) with
   | [], [] -> 0
   | [], _ -> ~-1
   | _, [] -> 1
   | (k,e) :: xs, (k',e') :: ys ->
    let c = D.compare k k' in
    if c = 0 then let c = R.compare e e' in if c = 0 then compare xs ys else c
    else c
  in
  compare (to_list h) (to_list h')
 ;;

 let equal h h' = compare h h' = 0;;

 (* Miscallenous *)
 let domain h = Listx.unique ~c:D.compare (fold (drop Listx.cons) h []);;
 let range h = Listx.unique ~c:R.compare (fold (const Listx.cons) h []);;
 let length = H.length;;
 let size = length;;

 (* Properties *)
 let is_empty h = size h = 0;;

 (* Printers *)
 let fprintf fmt =
  let fprintf fmt h =
   iteri (fun i k e ->
    let fs =
     if i > 0 then format_of_string "@\n@[%a@ ->@ %a@]"
     else format_of_string "@[%a@ ->@ %a@]"
    in
    Format.fprintf fmt fs D.fprintf k R.fprintf e) h
  in
  Format.fprintf fmt "@[%a@]" fprintf
 ;;

 let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;
end

module Partial (D : DOMAIN) = struct
 (*** MODULES *****************************************************************)
 module D = struct
  include D;;

  let equal x y = D.compare x y = 0;;
 end

 (*** INCLUDES ****************************************************************)
 include Hashtbl.Make (D);;

 (*** TYPES *******************************************************************)
 type domain = key;;
end

(*** TYPES ********************************************************************)
type ('a,'b) t = ('a,'b) Hashtbl.t;;

(*** FUNCTIONS ****************************************************************)
let add = Hashtbl.add;;
let clear = Hashtbl.clear;;
let copy = Hashtbl.copy;;
let create = Hashtbl.create;;
let remove = Hashtbl.remove;;
let replace = Hashtbl.replace;;
let find = Hashtbl.find;;
let find_all = Hashtbl.find_all;;
let fold = Hashtbl.fold;;
let iter = Hashtbl.iter;;
let mem = Hashtbl.mem;;
let length = Hashtbl.length;;
let hash = Hashtbl.hash;;
let hash_param = Hashtbl.hash_param;;
