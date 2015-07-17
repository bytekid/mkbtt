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
 val copy : t -> t
 val fprintf : Format.formatter -> t -> unit
 val hash : t -> int
end

module type RANGE = DOMAIN;;

module type SIGNATURE = sig
 type domain
 type range
 type t
 
 val add : domain -> range -> t -> t
 val empty : int -> t
 val replace : domain -> range -> t -> t
 val fold : (domain -> range -> 'a -> 'a) -> 'a -> t -> 'a
 val iter : (domain -> range -> unit) -> t -> unit
 val iteri : (int -> domain -> range -> unit) -> t -> unit
 val mem_dom : domain -> t -> bool
 val mem_ran : range -> t -> bool
 val find_dom : range -> t -> domain
 val find_ran : domain -> t -> range
 val clear : t -> t
 val copy : t -> t
 val domain : t -> domain list
 val range : t -> range list
 val size : t -> int
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

 module R = struct
  include R;;

  let equal x y = R.compare x y = 0;;
 end

 module DH = Hashtblx.Make (D) (R);;
 module RH = Hashtblx.Make (R) (D);;

 (*** TYPES *******************************************************************)
 type domain = D.t;;
 type range  = R.t;;
 type t = {domain : DH.t; range : RH.t};;
 
 (*** FUNCTIONS ***************************************************************)
 let clear t = {domain = DH.clear t.domain; range = RH.clear t.range};;
 
 let copy t =
  let add d r = DH.add (D.copy d) (R.copy r) in
  let domain = DH.fold add t.domain (DH.create (DH.length t.domain)) in
  let add r d = RH.add (R.copy r) (D.copy d) in
  let range = RH.fold add t.range (RH.create (RH.length t.range)) in
  {domain = domain; range = range}
 ;;

 let domain t = DH.domain t.domain;;
 let range t = RH.domain t.range;;
 let size t = DH.length t.domain;;

 (* Constructors *)
 let add d r t =
  if DH.mem d t.domain || RH.mem r t.range then t
  else {domain = DH.replace d r t.domain; range = RH.replace r d t.range}
 ;;

 let empty n = {domain = DH.create n; range = RH.create n};;

 let replace d r t =
  let remove p r f h h' = if p h then r (f h) h' else h' in
  let range = remove (DH.mem d) RH.remove (DH.find d) t.domain t.range in
  let domain = remove (RH.mem r) DH.remove (RH.find r) t.range t.domain in
  {domain = DH.replace d r domain; range = RH.replace r d range}
 ;;
 
 (* Iterators *)
 let fold f d t = DH.fold f t.domain d;;
 let iter f t = DH.iter f t.domain;;

 let iteri f t =
  const () (DH.fold (fun d r i -> f i d r; i + 1) t.domain 0)
 ;;

 (* Scan Functions *)
 let mem_dom d t = DH.mem d t.domain;;
 let mem_ran r t = RH.mem r t.range;;

 (* Search Functions *)
 let find_dom r t = RH.find r t.range;;
 let find_ran d t = DH.find d t.domain;;
 
 (* Printers *)
 let fprintf fmt =
  let fprintf fmt t =
   iteri (fun i d r ->
    let fs =
     if i > 0 then format_of_string "@\n@[%a@ <->@ %a@]"
     else format_of_string "@[%a@ <->@ %a@]"
    in
    Format.fprintf fmt fs D.fprintf d R.fprintf r) t
  in
  Format.fprintf fmt "@[%a@]" fprintf;
 ;;

 let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;
end
