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

(*** VALUES ******************************************************************)
type t

val terms : t -> U.Term.t * U.Term.t

val of_terms : U.Term.t -> U.Term.t -> t

val of_pair : U.Term.t * U.Term.t -> t

val oriented_of_terms : U.Term.t -> U.Term.t -> (t * bool) U.Monad.t

val normalized_of_rule : U.Rule.t -> t U.Monad.t

val of_rule : U.Rule.t -> t

val to_rule : t -> U.Rule.t

val hash : t -> int

val compare : t -> t -> int

val to_string : t -> string

val all_to_string : t list -> string

val to_stringm : t -> string U.Monad.t

val list_to_stringm : t list -> string U.Monad.t

val map : (U.Term.t -> U.Term.t) -> t -> t

val mapm : (U.Term.t -> U.Term.t U.Monad.t) -> t -> t U.Monad.t

val is_ground : t -> bool

val is_variant : t -> t -> bool

val skolemize : t -> t U.Monad.t

val size : t -> int

val max_size : t -> int

val set_size : t list -> int

val invert : t -> t
