(* Copyright 2010 Sarah Winkler
 * GNU Lesser General Public License
 *
 * This file is part of MKBtt.
 * 
 * MKBtt is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * MKBtt is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with MKBtt. If not, see <http://www.gnu.org/licenses/>.
 *)

(*** SUBMODULES **********************************************************)
module Label : sig
 type t = Symbol of Rewriting.Function.t | Star

 val compare : t -> t -> int

 val to_string : t -> string

 val fprintf : Format.formatter -> t -> unit
end

(*** TYPES ***************************************************************)
type label = Label.t

type t = Nil | Element of label * t * t

(*** VALUES **************************************************************)
val of_term : U.Term.t -> t

val time : float ref
