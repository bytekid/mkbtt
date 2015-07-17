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
val add_t_termination : float -> unit World.Monad.t
val inc_n_termination_calls : unit World.Monad.t
val inc_n_termination_yes : unit World.Monad.t
val inc_n_termination_timeouts : unit World.Monad.t
val add_t_variants : float -> unit World.Monad.t
val add_t_encs : float -> unit World.Monad.t
val add_t_overlaps1 : float -> unit World.Monad.t
val add_t_overlaps2 : float -> unit World.Monad.t
val add_t_insert : float -> unit World.Monad.t
val add_t_delete : float -> unit World.Monad.t
val inc_n_iterations : unit World.Monad.t
val inc_n_nodes : unit World.Monad.t
val n_processes : int World.Monad.t
val inc_n_processes : unit World.Monad.t
val dec_n_processes : int -> unit World.Monad.t
val add_t_orient : float -> unit World.Monad.t
val add_t_rewrite : float -> unit World.Monad.t
val add_t_deduce : float -> unit World.Monad.t
val add_n_ordrewrite_attempts : int -> unit World.Monad.t
val add_n_ordrewrite_tcalls : int -> unit World.Monad.t
val add_n_victims : int -> unit World.Monad.t
val add_t_process_kill : float -> unit World.Monad.t
val add_n_ordrewrite_ok : int -> unit World.Monad.t
val add_t_selection : float -> unit World.Monad.t
val inc_n_redundant_cps : unit World.Monad.t
val add_cp_for_process : int list -> unit World.Monad.t
val add_cp_for_processes : int list list -> unit World.Monad.t
val add_t_cpc : float -> unit World.Monad.t
val get_redundant_cps_for : int list -> int World.Monad.t
val add_t_isomorphisms : float -> unit World.Monad.t
val add_t_one : float -> unit World.Monad.t
val add_t_two : float -> unit World.Monad.t
val print_ordered : unit World.Monad.t
val general : unit World.Monad.t
val index : unit World.Monad.t
val cpc : int list -> unit World.Monad.t
val isomorphism : unit World.Monad.t
val selection : 'a -> unit World.Monad.t
val termination : unit World.Monad.t
val mkb_general : unit World.Monad.t
val mkb : 'a -> int list -> unit World.Monad.t
val slothrop : float -> unit World.Monad.t
val print : int list -> float -> unit World.Monad.t
val print_basic : 
 Completion.szs_status -> string -> float -> unit
val print_failure : float -> unit World.Monad.t
val print_timeout : float -> unit World.Monad.t
val print_system : U.Trs.t -> Equation.t list -> unit World.Monad.t
val print_success : 
 Equation.t list -> Types.CompletionProcess.t -> float -> unit World.Monad.t
val print_proof : 
 Completion.szs_status -> Equation.t list -> int list -> float -> 
 unit World.Monad.t
val log : string -> int -> unit World.Monad.t
val blog : bool -> string -> int -> unit World.Monad.t
val print_logs : unit World.Monad.t

