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
module CompletionProcess : sig
 type t = int list

 val initial : t
end

module Node :sig
 type t = {
   data    : Equation.t;
   r0      : CompletionProcess.t list * CompletionProcess.t list;
   r1      : CompletionProcess.t list * CompletionProcess.t list;
   e       : CompletionProcess.t list * CompletionProcess.t list;
   c0      : CompletionProcess.t list;
   c1      : CompletionProcess.t list;
   parents : occurrence list
  }
 and t_rule = int * bool
 and occurrence =
  | Axiom
  | Deduce of 
   t_rule * 
   Rewriting.Position.t * 
   t_rule * 
   CompletionProcess.t list
  | Rewrite of 
   t_rule * 
   Rewriting.Position.t * 
   t_rule * 
   CompletionProcess.t list * 
   CompletionProcess.t list
  | Instance of t_rule * CompletionProcess.t list
 
  val hash : t -> int
  val fprintf : Format.formatter -> t -> unit
  val make :
      U.Term.t ->
      U.Term.t ->
      CompletionProcess.t list * CompletionProcess.t list ->
      CompletionProcess.t list * CompletionProcess.t list ->
      CompletionProcess.t list * CompletionProcess.t list -> 
      CompletionProcess.t list -> CompletionProcess.t list -> 
      occurrence list -> t
  val copy : t -> t
  val compare : t -> t -> int
  end

module NodeEntry :
  sig
    type t = int * bool
    val to_string : t -> string
    val compare : t -> t -> int
  end

module NodeTermIndex :
  sig
    type entry = IndexWrapper.Make(NodeEntry).entry
    type t = IndexWrapper.Make(NodeEntry).t
    val make : Completion.options -> t * t
    val insert : U.Term.t * entry -> t -> t U.Monad.t
    val delete : U.Term.t * entry -> t -> t U.Monad.t
    val variant_candidates : U.Term.t -> t -> entry list U.Monad.t
    val generalization_candidates : U.Term.t -> t -> entry list U.Monad.t
    val unification_candidates : U.Term.t -> t -> entry list U.Monad.t
    val encompassment_candidates :
      U.Term.t -> t -> (entry * Rewriting.Position.t) list U.Monad.t
    val encompassment_candidates_below_root :
      U.Term.t -> t -> (entry * Rewriting.Position.t) list U.Monad.t
    val overlap2_candidates :
      U.Term.t -> t -> (entry * Rewriting.Position.t) list U.Monad.t
    val overlap1_candidates :
      U.Term.t -> t -> (entry * Rewriting.Position.t) list U.Monad.t
    val overlap1_candidates_below_root :
      U.Term.t -> t -> (entry * Rewriting.Position.t) list U.Monad.t
    val size : t -> int U.Monad.t
    val is_empty : t -> bool U.Monad.t
  end

module WaldmeisterGoalNode :
  sig
    type origin = Initial | Reduct of int * int * CompletionProcess.t list
    type t = {
      term : U.Term.t;
      processes : CompletionProcess.t list * CompletionProcess.t list;
      origins : origin list * origin list;
    }

    val hash : t -> int
    val make : 
     U.Term.t -> 
     CompletionProcess.t list * CompletionProcess.t list -> 
     origin list * origin list -> 
     t
    val copy : t -> t
    val compare : t -> t -> int
    val fprintf : Format.formatter -> t -> unit
  end

module Goal :
  sig
    type poss_pair = Rewriting.Position.t list * Rewriting.Position.t list
    type t =
        NodeGoal of int list
      | WaldmeisterGoal of int list
      | ExistentialGoal of (int * poss_pair) list
      | NoGoal
  end
