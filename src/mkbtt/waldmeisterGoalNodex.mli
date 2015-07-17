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

(*** SUBMODULES *********************************************************)
module CP : sig
 type t = int list
end

(*** TYPES **************************************************************)
type origin = Types.WaldmeisterGoalNode.origin = 
 Initial | Reduct of int * int * CP.t list
 
type t = Types.WaldmeisterGoalNode.t = {
 term : U.Term.t;
 processes : CP.t list * CP.t list;
 origins : origin list * origin list;
}

(*** VALUES *************************************************************)
val map : (CP.t list -> CP.t list) -> t -> t
val process_inter : t -> CP.t list
val borigin : bool -> t -> origin list
val restrict_to_process : CP.t -> t -> t
val add_processes1 : CP.t list * origin -> t -> t
val add_processes2 : CP.t list * origin -> t -> t
val content : t -> U.Term.t * (CP.t list * CP.t list)
val create : U.Term.t -> CP.t list -> CP.t list -> origin list -> origin list -> t
val merge : CP.t list -> CP.t list -> origin list -> origin list -> t -> t
val term : t -> U.Term.t
val remove_processes : CP.t list -> t -> t
val bcontains_prefix : bool -> CP.t -> t -> bool
val inter_contains_process : CP.t -> t -> bool
val to_stringm : t -> string World.Monad.t
val has_non_empty_processes : t -> bool
