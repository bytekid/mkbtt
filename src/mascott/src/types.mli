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

module Overlap : sig
type t = {
 source: U.Term.t;
 outer_rule: (U.Rule.t * CompletionProcess.t list * (int * bool));
 position: ACPosition.t;
 inner_rule: (U.Rule.t * CompletionProcess.t list * (int * bool));
 sub: U.Substitution.t;
 cp: U.Rule.t
 }

val source: t -> U.Term.t
val outer_rule: t -> (U.Rule.t * CompletionProcess.t list * (int * bool))
val inner_rule: t -> (U.Rule.t * CompletionProcess.t list * (int * bool))
val position: t -> ACPosition.t
val sub : t -> U.Substitution.t
val mom : t -> int * bool
val dad : t -> int * bool
val cp : t -> U.Rule.t
val processes : t -> CompletionProcess.t list

val make :
 U.Term.t ->
 (U.Rule.t * CompletionProcess.t list * (int * bool)) ->
 ACPosition.t ->
 (U.Rule.t * CompletionProcess.t list * (int * bool)) ->
 U.Substitution.t ->
 U.Rule.t ->
 t

val is_nontrivial : t -> bool

end

module Node :sig
 type ppair = CompletionProcess.t list * CompletionProcess.t list
 type poss = {
   all : ACPosition.t list;
   nonsymmetric : ACPosition.t list;
 }
 type t = {
   data    : Equation.t;
   r0      : ppair * ppair;
   r1      : ppair * ppair;
   e       : ppair;
   c0      : CompletionProcess.t list;
   c1      : CompletionProcess.t list;
   pos     : poss * poss;
   parents : occurrence list
  }
 and t_rule = int * bool
 and occurrence =
  | Axiom
  | SAxiom
  | Deduce of Overlap.t
  | Rewrite of Overlap.t
  | Extend of t_rule * CompletionProcess.t list
 
  val hash : t -> int
  val fprintf : Format.formatter -> t -> unit
  val make :
      U.Term.t ->
      U.Term.t ->
      ppair * ppair ->
      ppair * ppair ->
      ppair -> 
      CompletionProcess.t list -> CompletionProcess.t list -> 
      poss * poss ->
      occurrence list -> t
  val copy : t -> t
  val compare : t -> t -> int
  val all_pos : poss * poss -> ACPosition.t list * ACPosition.t list
  val nonsymm_pos : poss * poss -> ACPosition.t list * ACPosition.t list
  val make_pos :
      (ACPosition.t list * ACPosition.t list) ->
      (ACPosition.t list * ACPosition.t list) ->
      poss * poss
  val create_pos :
      ACPosition.t list ->  ACPosition.t list ->
      poss * poss
  end

module NodeEntry :
  sig
    type t = int * bool
    val to_string : t -> string
    val compare : t -> t -> int
  end

(*
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
*)

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

module Label :
  sig
    type t = Star | Fun of Rewriting.Function.t | AC of  Rewriting.Function.t
    val compare : 'a -> 'a -> int
    val to_string : t -> string
    val fprintf : Format.formatter -> t -> unit
  end

module ACDiscTree : sig
 type 'a values = (U.Term.t * 'a list) list

 type 'a t_node =
   Leaf of 'a values
 | Node of 'a t_edges 
 | ACLeaf of ('a values * U.Term.t list * 'a t_node * Rewriting.Position.t)
 | ACNode of ('a t_edges * U.Term.t list * 'a t_node * Rewriting.Position.t)
 and 'a t_edges = ('a t_node) Map.Make(Label).t

 type 'a t = 'a t_node

 val empty : 'a t
end
