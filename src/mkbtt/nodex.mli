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
type t = Types.Node.t = {
  data : Equation.t;
  r0 : CP.t list * CP.t list;
  r1 : CP.t list * CP.t list;
  e : CP.t list * CP.t list;
  c0 : CP.t list;
  c1 : CP.t list;
  parents : occurrence list;
}
and t_rule = int * bool
and occurrence =
  Types.Node.occurrence =
    Axiom
  | Deduce of t_rule * Rewriting.Position.t * t_rule *
      CP.t list
  | Rewrite of t_rule * Rewriting.Position.t * t_rule *
      CP.t list * CP.t list
  | Instance of t_rule * CP.t list

(*** VALUES *************************************************************)
val first : t -> U.Term.t
val second : t -> U.Term.t
val data : t -> U.Term.t * U.Term.t
val r0 : t -> CP.t list * CP.t list
val r0o : t -> CP.t list
val r0c : t -> CP.t list
val r0_all : t -> CP.t list
val r1 : t -> CP.t list * CP.t list
val r1o : t -> CP.t list
val r1c : t -> CP.t list
val r1_all : t -> CP.t list
val e : t -> CP.t list * CP.t list
val eo : t -> CP.t list
val ec : t -> CP.t list
val e_all : t -> CP.t list
val reo : t -> CP.t list
val c0 : t -> CP.t list
val c1 : t -> CP.t list
val constraints : t -> CP.t list * CP.t list
val parents : t -> occurrence list

val brule : bool -> t -> U.Rule.t
val brc : bool -> t -> CP.t list
val brcec : bool -> t -> CP.t list
val bcontent :
  bool ->
  t ->
  U.Term.t * U.Term.t *
  (CP.t list * CP.t list) *
  (CP.t list * CP.t list) *
  (CP.t list * CP.t list)

val equation : t -> Equation.t

val create :
  U.Term.t ->
  U.Term.t ->
  CP.t list ->
  CP.t list ->
  CP.t list -> occurrence list -> t World.Monad.t

val merge :
  CP.t list ->
  CP.t list ->
  CP.t list ->
  CP.t list ->
  CP.t list -> occurrence list -> t -> t World.Monad.t

val of_axiom : Equation.t -> t World.Monad.t
val of_rule : U.Rule.t -> t World.Monad.t
val map :
  (CP.t list -> CP.t list) ->
  t -> t
val project_rule :
  (t -> CP.t list) -> (t -> CP.t list) -> CP.t -> t -> U.Rule.t list
val project_eqn : (t -> CP.t list) -> CP.t -> t -> Equation.t list
val project_r : CP.t -> t -> U.Rule.t list
val project_e : CP.t -> t -> Equation.t list
val project_c : CP.t -> t -> U.Rule.t list
val project_r_closed : CP.t -> t -> U.Rule.t list
val project_r_open : CP.t -> t -> U.Rule.t list
val project_e_closed : CP.t -> t -> Equation.t list
val project_rx : CP.t -> t -> (U.Rule.t * bool) list
val project_cx : CP.t -> t -> (U.Rule.t * bool) list
val contains_process_open : CP.t -> t -> bool
val er_contains_closed : CP.t -> t -> bool
val update :
  t ->
  CP.t list * CP.t list ->
  CP.t list * CP.t list ->
  CP.t list * CP.t list -> t World.Monad.t
val bupdate :
  bool ->
  t ->
  CP.t list * CP.t list ->
  CP.t list * CP.t list ->
  CP.t list * CP.t list -> t World.Monad.t
val close : t -> t World.Monad.t
val move_to_r_and_c : 
 CP.t list * CP.t list * CP.t list -> t -> t World.Monad.t
val subsumes : t -> CP.t list -> CP.t list -> CP.t list -> bool
val considered_for : t -> CP.t list
val is_done_with : t -> CP.t list -> CP.t list -> CP.t list -> bool
val has_non_empty_labels : t -> bool
val has_er_labels : bool -> t -> bool
val contains_rule_labels : t -> bool
val sum_size : t -> int
val max_size : t -> int
val restrict_to_process : CP.t -> t -> t
val remove_processes : CP.t list -> t -> t

val to_string : t -> string
val to_stringm : t -> string World.Monad.t
val is_axiom : t -> bool
