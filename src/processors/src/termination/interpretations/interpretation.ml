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
open Rewritingx;;

(*** MODULES ******************************************************************)
module F = Format;;
module Fun = Function;;
module M = Monad;;

(*** MODULE TYPES *************************************************************)
module type ELT = sig
 type t

 val fprintf_intp : F.formatter -> (string * int * t list * t) -> unit
 val fprintfx_intp : int -> F.formatter -> (string * int * t list * t) -> unit
end

module type ALGEBRA = sig
 type elt 
 type value = elt list * elt
 type t

 val add : t -> Fun.t -> elt list -> elt -> t
 val empty : ?size:int -> unit -> t
 val get : t -> Fun.t -> elt list * elt
 val fold : (Fun.t -> value -> 'a -> 'a) -> t -> 'a -> 'a
 val fprintf : F.formatter -> t -> unit M.t 
 val fprintfx : int -> F.formatter -> t -> unit M.t 
end

(*** MODULES ******************************************************************)
module Make (E : ELT) : ALGEBRA with type elt = E.t = struct
 (*** TYPES *******************************************************************)
 type elt = E.t;;
 type value = (elt list * elt);;
 type t = (Fun.t, value) Hashtbl.t;;

 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;
 let empty ?(size = 128) () = Hashtbl.create (2 * size);;
 let add i f cs a = Hashtbl.replace i f (cs,a); i;;

 let get i f =
  try Hashtbl.find i f with Not_found -> failwith "missing interpretation"
 ;;

 let fold f intp e = Hashtbl.fold f intp e;;

 let fprintf fmt i =
  let xs = Hashtbl.fold (fun f v xs -> (f,v) :: xs) i [] in
  M.iteri (fun i (f,(cs,const)) ->
   M.find_ari f >>= fun a ->
   if i > 0 then F.fprintf fmt ",@\n@\n"; M.to_string_fun f >>= fun f ->
   M.return (E.fprintf_intp fmt (f,a,cs,const))) xs
 ;;

 let fprintfx dim fmt i =
  let xs = Hashtbl.fold (fun f v xs -> (f,v) :: xs) i [] in
  M.iteri (fun i (f,(cs,const)) ->
   M.find_ari f >>= fun a ->
   M.to_stringx_fun f >>= fun f ->
   M.return (E.fprintfx_intp dim fmt (f,a,cs,const))) xs
 ;;
end
