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

(*** TYPES ***************************************************************)
type modetype =
 | Completion
 | Proof
 | CompletenessCheck of string

type goal_type =
 | Universal of Equation.t list
 | Existential of Equation.t list

type universal_goal_type =
 | NodeGoal
 | WaldmeisterGoal
 | Rewrite2NF

type equality_type =
 | Equality
 | Disequality
;;

type conjecture_type =
 | Conjecture of equality_type
 | NegatedConjecture of equality_type
;;

type szs_status =
 | Suc
 | Sat
 | Uns
 | Noc
 | Unk
 | Gup of string
 | Tmo
;;

type indextype = Naive
 | PathIndexing
 | DiscriminationTrees
 | CodeTrees

type cpctype =   NoCPC
 | Prime
 | Connected
 | Blocked
 | SReduce
 | All

type isomorphism_checks = NoChecks
 | Renamings
 | Permutations

type t_node_size = Sum | Max

type result = MKBttResult of int list | SlothropResult of U.Trs.t

type proof_type = 
  NoProof 
 | SZS 
 | CpfConversion     
 | CpfEquationalProofTree
 | CpfEquationalProofTreeWithLemmas

type options = {
 axioms                       : Equation.t list;
 axiom_ids                    : int list;
 certification_output         : bool;
 certification_strategy       : string;
 check_externally             : bool;
 check_isomorphism_with       : isomorphism_checks;
 check_isomorphisms_repeatedly: bool;
 conjecture                   : conjecture_type option;
 cpc                          : cpctype; (* critical pair criterion used *)
 detect_iso_auto              : bool;
 do_cpc_caching               : bool;
 filename                     : string;
 goal                         : goal_type;
 kill_processes               : float;
 mode                         : modetype;
 node_size                    : t_node_size;
 no                           : bool;
 ordered_completion           : bool;
 output_proof                 : proof_type;
 output_statistics            : bool;
 output_trs                   : bool;
 preoriented                  : bool;
 propagate_small_lemmata      : int;
 rewrite_before_termination_check: bool * int;
 rewrite_index                : indextype;
 s_theory_file                : string;
 selection_strategy           : SelectionStrategy.t;
 size_age_ratio               : float;
 slothrop                     : bool;
 strategy_macro               : SelectionStrategy.macro option;
 termination_certifier        : string;
 termination_strategy         : string;
 timeout                      : float;
 tool                         : string;
 ttimeout                     : float; (* termination timeout *)
 unification_index            : indextype;
 universal_goal               : universal_goal_type;
 use_slothrop                 : bool; (* mkb or slothrop *)
 verbosity                    : int; (* 0, 1 or 2 *)
}

(*** EXCEPTIONS **********************************************************)
exception GiveUp of string

(*** VALUES **************************************************************)

val default_options : options

val axioms : options -> Equation.t list
val axiom_ids : options -> int list
val certification_output : options -> bool
val certification_strategy : options -> string
val kill_processes : options -> float
val set_kill : options -> float -> options
val filename : options -> string
val termination_strategy : options -> string
val verbosity : options -> int
val timeout : options -> float
val ttimeout : options -> float
val command : options -> string
val check_externally : options -> bool
val rewrite_index : options -> indextype
val perfect_filtering : options -> bool
val unification_index : options -> indextype
val use_slothrop : options -> bool
val cpc : options -> cpctype
val do_cpc_caching : options -> bool
val check_isomorphism : options -> isomorphism_checks
val check_isomorphisms_repeatedly : options -> bool
val is_ordered : options -> bool
val goal : options -> goal_type
val goal_implementation : options -> universal_goal_type
val has_goal : options -> bool
val preoriented : options -> bool
val s_theory_file : options -> string
val selection_strategy : options -> SelectionStrategy.t
val selection_macro : options -> SelectionStrategy.macro option
val size_age_ratio : options -> float
val mode : options -> modetype
val node_size : options -> t_node_size
val propagate_small_lemmata : options -> int
val rewrite_before_termination_check : options -> bool * int
val conjecture : options -> conjecture_type option
val is_successful : szs_status -> bool
val detect_isomorphism : options -> bool
val output_proof : options -> proof_type
