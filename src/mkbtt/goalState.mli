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
val equals_x_x_is_true : unit -> Equation.t World.Monad.t
val equals_s_t_is_false : U.Term.t -> U.Term.t -> Equation.t World.Monad.t
val true_node : unit -> int World.Monad.t
val true_is_false : unit -> Equation.t World.Monad.t
val false_term : unit -> U.Term.t World.Monad.t
val get_goal : Types.Goal.t World.Monad.t
val get_ngoal : int list World.Monad.t
val get_egoal : (int * Types.Goal.poss_pair) list World.Monad.t
val get_wgoal : int list World.Monad.t
val set_goal : Types.Goal.t -> unit World.Monad.t
val set_ngoal : int list -> unit World.Monad.t
val set_wgoal : int list -> unit World.Monad.t
val set_egoal : (int * Types.Goal.poss_pair) list -> unit World.Monad.t
val split : CompletionProcessx.t list -> unit World.Monad.t
val restrict_to_process : CompletionProcessx.t -> unit World.Monad.t
val remove_processes : CompletionProcessx.t list -> unit World.Monad.t
val store_goal_equation : Equation.t -> unit World.Monad.t
val get_goal_eqs : Equation.t list World.Monad.t
val constants : Rewriting.Function.t list World.Monad.t
