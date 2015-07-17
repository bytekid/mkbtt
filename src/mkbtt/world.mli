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

(** Huge state context describing almost the entire world.
 @author Sarah Winkler
 @since  2010/11/17 *)

(*** OPENS ***************************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module IntNodeIndex : Index.ISOMORPHIC
 with type key = Int.t
 and type element = Types.Node.t

module IntTermIndex : Index.ISOMORPHIC
 with type key = Int.t
 and type element = U.Term.t

module IntWNodeIndex : Index.ISOMORPHIC
 with type key = Int.t
 and type element = Types.WaldmeisterGoalNode.t


(*** TYPES ***************************************************************)
type statistics_context = {
 (* termination-related figures *)
 t_termination          : float;
 n_termination_calls    : int;   (* number of termination checks *)
 n_termination_yes      : int;   (* number of positive checks *)
 n_termination_timeouts : int;   (* number of timeouts *)
 (* related to term indexing *)
 t_variants             : float; (* time to retrieve variants *)
 t_encs                 : float; (* time to retrieve encompassments *)
 t_overlaps1            : float; (* time to retrieve overlaps1 *)
 t_overlaps2            : float; (* time to retrieve overlaps2 *)
 t_insert               : float; (* time for index insertion *)
 t_delete               : float; (* time for index deletion *)
 (* general mkbTT stuff *)
 n_iterations           : int;   (* number of iterations *)
 n_processes            : int;   (* number of simulated processes *)
 n_nodes                : int;   (* number of nodes *)
 t_mkb_orient           : float; (* time for orient steps *)
 t_mkb_rewrite          : float; (* time for rewrite steps *)
 t_mkb_deduce           : float; (* time for deduce steps *)
 (* figures related to ordered rewriting for  ordered completion *)
 n_ordrewrite_attempts  : int; (* number of ordered rewrite attempts *)
 n_ordrewrite_tcalls    : int; (* number of required termination calls *)
 n_ordrewrite_ok       : int; (* number of successful ordered rewrites *)
 (* for critical pair criteria *)
 redundant_cps          : (int list,  int) Hashtbl.t; (* for processes *)
 n_redundant_cps        : int; (* number of CPs redundant for some process*)
 t_cpc                  : float; (* time to compute CPC *)
 (* for process killing *)
 t_process_kill         : float; (* time required for process killing *)
 n_victims              : int; (* number of victims *)
 (* related to selection *)
 t_selection            : float; (* time required for node selection *)
 (* related to isomorphism checks *)
 t_isomorphisms         : float;
 (* for various debugging purposes *)
 t_one                  : float;
 t_two                  : float;
}

type processes_context = {
 all            : Types.CompletionProcess.t list;
 count          : int;
 choices        : (Types.CompletionProcess.t, int) Hashtbl.t;
 esizes         : (Types.CompletionProcess.t, int) Hashtbl.t;
 csizes         : (Types.CompletionProcess.t, int) Hashtbl.t;
 rosizes        : (Types.CompletionProcess.t, int) Hashtbl.t;
}

type iso_context = {
 dummy_fun    : Rewriting.Function.t option;
 dummy_var    : Rewriting.Variable.t option;
 classes      : IntTermIndex.t;
 node_classes : (int, int * int) Hashtbl.t
}

type node_pool_context = {
 ids: IntNodeIndex.t;
 equations: (Equation.t, int) Hashtbl.t;
}

type goal_state_context = {
 true_symbol  : Rewriting.Function.t option;
 false_symbol : Rewriting.Function.t option;
 equal_symbol : Rewriting.Function.t option;
 goal         : Types.Goal.t;
 goal_eqs     : Equation.t list;
 windex       : IntWNodeIndex.t * (U.Term.t,int) 
                Hashtbl.t
}

type node_state_context = {
 open_nodes: int list;
 closed_nodes: int list;
 term_index: Types.NodeTermIndex.t * Types.NodeTermIndex.t;
}

type cpc_state_context = {
 cpc_cache: (U.Term.t, Types.CompletionProcess.t list) Hashtbl.t
}

type choice_state_context = {
 ccount: int
}

type context = {
 options      : Completion.options;
 statistics   : statistics_context;
 processes    : processes_context;
 iso_context  : iso_context;
 node_pool    : node_pool_context;
 goal_state   : goal_state_context;
 node_state   : node_state_context;
 cpc_state    : cpc_state_context;
 choice_state : choice_state_context;
 logs         : string list
}

module Statex : sig type t = context end

module Monad : Util.Monad.Transformer.STATE_MONAD 
 with type state = Statex.t 
 and type 'a m = 'a U.Monad.t

(*** VALUES *******************************************************************)

(** {2 The Initial Context} *)

val initial_context : context

val initial_context_with : Completion.options -> context

(** {3 Functions Related to Completion Options} *)

val get_options: Completion.options Monad.t

val set_options: Completion.options -> unit Monad.t

(** {4 Statistics-Related Functions} *)

val get_statistics: statistics_context Monad.t

val set_statistics: statistics_context -> unit Monad.t

(** {5 Process-Related Functions} *)

val get_processes: processes_context Monad.t

val set_processes: processes_context -> unit Monad.t

(** {5 Isomorphism-Related Functions} *)

val get_iso_context: iso_context Monad.t

val set_iso_context: iso_context -> unit Monad.t

(** {6 Functions Related to Node Storage} *)

val get_node_pool: node_pool_context Monad.t

val set_node_pool: node_pool_context -> unit Monad.t

(** {6 Functions Related to Goal State} *)

val get_goal_state: goal_state_context Monad.t

val set_goal_state: goal_state_context -> unit Monad.t

(** {6 Functions Related to Node State} *)

val get_node_state: node_state_context Monad.t

val set_node_state: node_state_context -> unit Monad.t

(** {7 Functions Related to CPC State} *)

val get_cpc_state: cpc_state_context Monad.t

val set_cpc_state: cpc_state_context -> unit Monad.t

(** {7 Functions Related to CPC State} *)

val get_choice_state: choice_state_context Monad.t

val set_choice_state: choice_state_context -> unit Monad.t

(** {8 Functions Related to Logging} *)

val get_logs: (string list) Monad.t

val log: int -> string -> unit Monad.t

module M : sig
 module Sig : sig
  val create_fun : int -> string -> Rewriting.Function.t Monad.t
  val fresh_var  : Rewriting.Variable.t Monad.t
 end

 module Term : sig
  val rename : U.Term.t -> U.Term.t Monad.t
  val to_stringm : U.Term.t -> string Monad.t
  val fprintfx : Format.formatter -> U.Term.t -> unit Monad.t
 end

 module Termx : sig
  val fresh_vars : U.Term.t -> U.Term.t Monad.t
 end

 module Rule : sig
  val to_stringm : U.Rule.t -> string Monad.t
  val rename : U.Rule.t -> U.Rule.t Monad.t
  val fprintfx : Format.formatter -> U.Rule.t -> unit Monad.t
 end

 module Rulex : sig
  val rewrite : 
   U.Term.t -> U.Rule.t -> Rewriting.Position.t ->
   (U.Term.t * U.Substitution.t * U.Rule.t) option Monad.t

  val narrow :
   U.Term.t -> U.Rule.t -> Rewriting.Position.t ->
   ((U.Term.t * U.Substitution.t * U.Rule.t) option) Monad.t

  val rewrite_subrule :
   U.Term.t -> U.Rule.t -> Rewriting.Position.t -> 
   (U.Term.t * U.Rule.t) option Monad.t
 end

 module Trsx : sig
  val critical_pairs : U.Trs.t -> Equation.t list Monad.t
  val to_stringm : U.Trs.t -> string Monad.t
  val to_wst_stringm : U.Trs.t -> string Monad.t
  val to_stringx : U.Trs.t -> string Monad.t
  val fprintfx : Format.formatter -> U.Trs.t -> unit Monad.t
 end

 module Equation : sig
  val list_to_stringm : Equation.t list -> string Monad.t
 end

 val liftm : 'a U.Monad.t -> 'a Monad.t
end
