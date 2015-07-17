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

(*** OPEN *********************************************************************)
open Util;;

(*** MODULE TYPES *************************************************************)
module type LABEL = sig
 module F : Index.ISOMORPHIC with
  type key = Function.t and type element = string

 module V : Index.ISOMORPHIC with
  type key = Variable.t and type element = string

 type signature = F.t * V.t
 type t

 val compare : t -> t -> int
 val copy : t -> t
 val fprintf : Format.formatter -> t -> unit
 val fprintfs : (Format.formatter -> Function.t -> signature -> signature)
  -> (Format.formatter -> Variable.t -> signature -> signature)
  -> Format.formatter -> t -> signature -> signature
 val hash : t -> int
end
