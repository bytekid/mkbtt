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
 | CompletenessCheck of string

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

type cpctype =   NoCPC | SReducibility | Prime | Connected | Mixed

type isomorphism_checks = NoChecks
 | Renamings
 | Permutations

type t_node_size = Sum | Max

type t_theory = Theory.t

type proof_type =
   NoProof
 | SZS
 | CpfConversion
 | CpfEquationalProofTree
 | CpfEquationalProofTreeWithLemmas

type options = {
 axioms                       : Equation.t list;
 filename                     : string;
 mode                         : modetype;
 timeout                      : float;
 ttimeout                     : float;
 verbosity                    : int;
 rewrite_index                : indextype;
 unification_index            : indextype;
 tool                         : string;
 certification_output         : bool;
 check_externally             : bool;
 termination_strategy         : string;
 cpc                          : cpctype;
 do_cpc_caching               : bool;
 check_isomorphism_with       : isomorphism_checks;
 check_isomorphisms_repeatedly: bool;
 selection_strategy           : SelectionStrategy.t;
 strategy_macro               : SelectionStrategy.macro option;
 ordered_completion           : bool;
 goal                         : Equation.t option;
 use_orphan_check             : bool;
 size_age_ratio               : float;
 node_size                    : t_node_size;
 no                           : bool;
 rewrite_before_termination_check: bool * int;
 conjecture                   : conjecture_type option;
 output_proof                 : proof_type;
 output_statistics            : bool;
 output_trs                   : bool;
 kill_processes               : float;
 s_theory_file                : string;
 s_theory                     : U.Trs.t;
 theory                       : t_theory
}

(*** EXCEPTIONS **********************************************************)
exception GiveUp of string

(*** VALUES **************************************************************)

val default_options : options

val axioms : options -> Equation.t list
val filename : options -> string
val termination_strategy : options -> string
val verbosity : options -> int
val timeout : options -> float
val ttimeout : options -> float
val command : options -> string
val check_externally : options -> bool
val rewrite_index : options -> indextype
val unification_index : options -> indextype
val cpc : options -> cpctype
val do_cpc_caching : options -> bool
val check_isomorphism : options -> isomorphism_checks
val check_isomorphisms_repeatedly : options -> bool
val is_ordered : options -> bool
val set_kill : options -> float -> options
val goal : options -> Equation.t option
val has_goal : options -> bool
val selection_strategy : options -> SelectionStrategy.t
val selection_macro : options -> SelectionStrategy.macro option
val use_orphan_check : options -> bool
val size_age_ratio : options -> float
val mode : options -> modetype
val node_size : options -> t_node_size
val rewrite_before_termination_check : options -> bool * int
val conjecture : options -> conjecture_type option
val is_successful : szs_status -> bool
val set_axioms : options -> Equation.t list -> options
val set_goal : options -> Equation.t option -> options
val kill_processes : options -> float
val s_theory_file : options -> string
val s_theory : options -> U.Trs.t
val theory : options -> t_theory
val certification_output : options -> bool
val output_proof : options -> proof_type
