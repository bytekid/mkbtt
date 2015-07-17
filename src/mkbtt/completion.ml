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

(** Collects options and settings for mkbTT run
@author Sarah Winkler
@since  Thu Feb 19 13:28:01 CET 2008
*)

(** Defines type of mkbTT settings (options) and various auxiliary types *)

(*** SUBMODULES **********************************************************)
module Trs = U.Trs;;

(*** TYPES ***************************************************************)
(* mode to run mkbTT in, either saturate equations or prove conjecture *)
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

(* different kind of term indexing techniques *)
type indextype = Naive 
 | PathIndexing 
 | DiscriminationTrees
 | CodeTrees

(* different kind of critical pair criteria *)
type cpctype =   NoCPC
 | Prime (* primality criterion *)
 | Connected (* connectedness criterion *)
 | Blocked
 | SReduce
 | All  (* PCP + CCP *)

type isomorphism_checks = NoChecks
 | Renamings
 | Permutations

type t_node_size = Sum | Max

type result = MKBttResult of int list | SlothropResult of Trs.t

type proof_type = 
   NoProof 
 | SZS 
 | CpfConversion 
 | CpfEquationalProofTree
 | CpfEquationalProofTreeWithLemmas

(* options record collects settings for mkbTT run *)
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

(*** FUNCTIONS ***********************************************************)
let mycstrategy =
 "dp;(edg?; sccs?; (sc | \
   {ur?;matrix -dp -dim 1 -ib 3 -ob 4 -ur[2]}restore | \
   {ur?;lpo -dp -af[2]}restore ))* || matrix -dim 2 -ib 2"
;;


let default_options = {
 axioms = [];
 axiom_ids = [];
 certification_output = false;
 certification_strategy = mycstrategy;
 check_externally = false;
 check_isomorphism_with = Renamings;
 check_isomorphisms_repeatedly = false;
 conjecture = None;
 cpc = All;
 detect_iso_auto = true;
 do_cpc_caching = false;
 filename = "";
 goal = Universal [];
 kill_processes = 5.0;
 mode = Completion;
 node_size = Sum;
 no = false;
 ordered_completion = false;
 output_proof = NoProof;
 output_statistics = false;
 output_trs = false;
 preoriented = false;
 propagate_small_lemmata = 5;
 rewrite_before_termination_check = false, 0;
 rewrite_index = CodeTrees;
 s_theory_file = "";
 selection_strategy = SelectionStrategy.japan_max;
 size_age_ratio = 0.65;
 slothrop = false;
 strategy_macro = Some SelectionStrategy.JapanMax;
 termination_certifier = "./bin/ttt2 -x ";
 termination_strategy = "dp;(odg?; sccs?; (sc | csf | lpo | kbo -ib 2))*";
 timeout = 60.0;
 tool = ""; (* does not matter if no external checker used *)
 ttimeout = 2.0;
 unification_index = DiscriminationTrees;
 universal_goal = NodeGoal;
 use_slothrop = false;
 verbosity = 0;
}

let axioms o = o.axioms;;
let axiom_ids o = o.axiom_ids;;
let certification_output o = o.certification_output;;
let certification_strategy o = o.certification_strategy;;
let check_externally o = o.check_externally;;
let check_isomorphism o = o.check_isomorphism_with;; 
let check_isomorphisms_repeatedly o = o.check_isomorphisms_repeatedly;;
let command o = o.tool;;
let conjecture o = o.conjecture;;
let cpc o = o.cpc;;
let detect_isomorphism o = o.detect_iso_auto;;
let do_cpc_caching o = o.do_cpc_caching;;
let filename o = o.filename;;
let goal o = o.goal;;
let goal_implementation o = o.universal_goal;;
let has_goal o = 
 match o.goal with
  | Universal e -> not (e = [])
  | _ -> true
;;
let is_ordered o = o.ordered_completion;;
let kill_processes o = o.kill_processes;;
let set_kill o k = {o with kill_processes = k};;
let mode o = o.mode;;
let node_size o = o.node_size;;
let preoriented o = o.preoriented;;
let propagate_small_lemmata o = o.propagate_small_lemmata;;
let rewrite_index o = o.rewrite_index;;
let s_theory_file o = o.s_theory_file;;
let selection_macro o = o.strategy_macro;;
let selection_strategy o = o.selection_strategy;;
let size_age_ratio o = o.size_age_ratio;;
let rewrite_before_termination_check o = o.rewrite_before_termination_check;;
let termination_strategy o = o.termination_strategy;;
let timeout o = o.timeout;;
let ttimeout o = o.ttimeout;;
let unification_index o = o.unification_index;;
let use_slothrop o = o.use_slothrop;;
let verbosity o = o.verbosity;;

let perfect_filtering o =
 match o.rewrite_index with
  | Naive 
  | CodeTrees -> true
  | _ -> false
;;

let is_successful = function
 | Suc 
 | Sat
 | Noc
 | Uns -> true
 | Unk
 | Gup _ 
 | Tmo -> false
;;

let output_proof o = o.output_proof
