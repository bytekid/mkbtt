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

(*** VALUES *************************************************************)
type t = int list

val initial : t
val length : t -> int
val to_string : t -> string

val fprintf : Format.formatter -> t -> unit
val compare : t -> t -> int

val empty : t list
val initial_set : t list
val is_empty : t list -> bool
val diff : t list -> t list -> t list
val union : t list -> t list -> t list
val union3 : t list -> t list -> t list -> t list
val is_subset : t list -> t list -> bool
val mem : t -> t list -> bool
val set_to_string : t list -> string

val is_prefix_of : t -> t -> bool
val add_zero : t list -> t list
val add_one : t list -> t list
val apply_split : t list -> t list -> t list

val all_processes : t list World.Monad.t
val complement : t list -> t list World.Monad.t
val split_state : Types.CompletionProcess.t list -> unit World.Monad.t
val remove : t list -> unit World.Monad.t

val get_esize : t -> int World.Monad.t
val get_csize : t -> int World.Monad.t
val get_rosize : t -> int World.Monad.t
val add_esize : int -> t -> unit World.Monad.t
val remove_esize : int -> t -> unit World.Monad.t
val add_csize : int -> t -> unit World.Monad.t
val add_rosize : int -> t -> unit World.Monad.t
val remove_rosize : int -> t -> unit World.Monad.t

