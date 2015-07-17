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
open Util;;

(*** MODULES ******************************************************************)
module type RING = sig
 type t
 val add : t -> t -> t
 val mul : t -> t -> t
 val fprintf : Format.formatter -> t -> unit
 val to_string : t -> string
end

module type STRINGIFIABLE = sig
 type t
 val to_string : t -> string
end

module type MONOMIAL = sig
 type t
 type elt
 type var
 val add : t -> t -> t
 val coefficient : t -> elt
 val compare : t -> t -> int
 val is_constant : t -> bool
 val make : elt -> var list -> t
 val mul : t -> t -> t
 val normal_form : t -> t
 val scale : elt -> t -> t
 val split : t -> (elt * var list)
 val to_string : t -> string
 val fprintf : Format.formatter -> t -> unit
 val variables : t -> var list
end

module Make (Elt : RING) (Var : STRINGIFIABLE) :
 (MONOMIAL with type elt = Elt.t and type var = Var.t) = struct

 (*** TYPES *******************************************************************)
 type elt = Elt.t;;
 type var = Var.t;;
 type t = elt * var list;;

 (*** FUNCTIONS ***************************************************************)
 let normal_form (c, xs) = (c, List.sort Pervasives.compare xs);;
 
 let compare (c0, xs0) (c1, xs1) = Pervasives.compare xs0 xs1;;

 let make c xs = normal_form (c, xs);;
 
 let split = id;;
 let coefficient = Pervasives.fst;;
 let variables = Pervasives.snd;;

 let is_constant m = variables m = [];;

 let add (c0, xs0) (c1, xs1) =
  if xs0 = xs1 then 
   (Elt.add c0 c1, xs0)
  else 
   failwith "add: different variables" 
 ;;

 let mul (c0,xs0) (c1,xs1) =
  make (Elt.mul c0 c1) (List.merge Pervasives.compare xs0 xs1)
 ;;

 let scale c' (c, xs) = (Elt.mul c' c, xs);;

 let fprintf ppf = function
  | (c, []) -> Format.fprintf ppf "%a" Elt.fprintf c
  | (c, xs) -> Format.fprintf ppf "%a%a" Elt.fprintf c 
   (List.fprintf (fun ppf x -> Format.fprintf ppf "%s" (Var.to_string x)) " * ") xs
 ;;

 let to_string m = Format.fprintf Format.str_formatter"@[%a@]" fprintf m; 
  Format.flush_str_formatter ()
 ;;
 end
