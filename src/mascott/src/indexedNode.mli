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

(*** VALUES *************************************************************)
val by_id : int -> Nodex.t option World.Monad.t
val id : Nodex.t -> int World.Monad.t
val by_eqn : Equation.t -> int option World.Monad.t
val node_by_eqn : Equation.t -> (int * Nodex.t) option World.Monad.t
val store : Nodex.t -> unit World.Monad.t
val restore : (Nodex.t -> Nodex.t) -> int -> unit World.Monad.t
val create_axiom : Equation.t -> Nodex.t World.Monad.t
val create_s_axiom : U.Rule.t -> Nodex.t World.Monad.t
val of_axiom : Equation.t -> int World.Monad.t
val create :
  U.Term.t -> U.Term.t ->
  Nodex.CP.t list * Nodex.CP.t list -> Nodex.CP.t list * Nodex.CP.t list ->
  Nodex.CP.t list -> Nodex.occurrence -> int World.Monad.t
val create_deduce :
  U.Term.t -> U.Term.t ->
  Nodex.CP.t list -> Nodex.occurrence -> int World.Monad.t
val split : CP.t list -> int -> unit World.Monad.t
val check_node :
  (Nodex.t -> 'a -> 'a -> 'b -> bool) ->
  U.Term.t -> U.Term.t -> 'a -> 'a -> 'b -> bool World.Monad.t
val is_not_necessary :
  U.Term.t ->
  U.Term.t -> Nodex.ppair -> Nodex.ppair -> Nodex.CP.t list -> bool World.Monad.t
val id_lift : (Nodex.t -> 'a) -> int -> 'a World.Monad.t
val id_liftm : (Nodex.t -> 'a World.Monad.t) -> int -> 'a World.Monad.t
val eo : int -> Nodex.CP.t list World.Monad.t
val ec : int -> Nodex.CP.t list World.Monad.t
val e_all : int -> Nodex.CP.t list World.Monad.t
val reo : int -> Nodex.CP.t list World.Monad.t
val rueo : int -> Nodex.CP.t list World.Monad.t
val data : int -> (U.Term.t * U.Term.t) World.Monad.t
val parents : int -> Nodex.occurrence list World.Monad.t
val constraints : int -> (Nodex.CP.t list * Nodex.CP.t list) World.Monad.t
val brule : bool -> int -> U.Rule.t World.Monad.t
val brc : bool -> int -> Nodex.CP.t list World.Monad.t
val brcec : bool -> int -> Nodex.CP.t list World.Monad.t
val brewpos : bool -> int -> ACPosition.t list World.Monad.t
val bdedpos : bool -> int -> ACPosition.t list World.Monad.t
val bcontent :
  bool ->
  int ->
  (U.Term.t * U.Term.t * (Nodex.ppair * Nodex.ppair) *
   (Nodex.ppair * Nodex.ppair) * Nodex.ppair)
  World.Monad.t
val bmap_union : (bool -> int -> 'a list World.Monad.t) -> int -> 'a list World.Monad.t 
val to_string : int -> string World.Monad.t
val to_stringm : int -> string World.Monad.t
val close : int -> unit World.Monad.t
val move_to_r_and_c :
  Nodex.CP.t list * Nodex.CP.t list * Nodex.CP.t list -> int -> unit World.Monad.t
(*
val bupdate :
  bool ->
  int ->
  Nodex.CP.t list * Nodex.CP.t list ->
  Nodex.CP.t list * Nodex.CP.t list ->
  Nodex.CP.t list * Nodex.CP.t list -> unit World.Monad.t*)
val has_non_empty_labels : int -> bool World.Monad.t
val has_non_empty_unprotected_open_labels : int -> bool World.Monad.t
val contains_rule_labels : int -> bool World.Monad.t
val er_contains_closed : Nodex.CP.t -> int -> bool World.Monad.t
val sum_size : int -> int World.Monad.t
val max_size : int -> int World.Monad.t
val restrict_to_process : Nodex.CP.t -> int -> unit World.Monad.t
val remove_processes : Nodex.CP.t list -> int -> unit World.Monad.t
val remove_er : Nodex.CP.t list -> int -> unit World.Monad.t
(** [remove_er ps n] removes processes in [ps] from all rewrite and
 equation labels in the node with id [n]. Used in rewrite to update the 
 old node. Note that this is only sound if [ps] does not contain all 
 rule labels in the rule node, but only its intersection with the
 labels to be used in the node to be reduced (label condition!). *)

val extend : int -> int list World.Monad.t
(** [extend n] returns the (possibly empty) set of nodes corresponding
 to extended rules for [n] with respect to AC. *)

(* returns all labels in node with these terms if it exists *)
val considered_for : Equation.t -> Nodex.CP.t list World.Monad.t

val set_deduce_positions : int -> unit World.Monad.t
(** [set_deduce_positions n] computes the subset of function symbol
 positions that are relevant for overlaps (i.e., symmetric positions are
 filtered out). Performed upon orienting a node. *)

