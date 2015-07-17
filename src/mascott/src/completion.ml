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

(* different kind of term indexing techniques *)
type indextype = Naive 
 | PathIndexing 
 | DiscriminationTrees
 | CodeTrees

(* different kind of critical pair criteria *)
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

(* options record collects settings for mkbTT run *)
type options = {
 axioms                       : Equation.t list;
 filename                     : string;
 mode                         : modetype;
 timeout                      : float;
 ttimeout                     : float; (* termination timeout *)
 verbosity                    : int; (* 0, 1 or 2 *)
 rewrite_index                : indextype;
 unification_index            : indextype;
 tool                         : string;
 certification_output         : bool;
 check_externally             : bool;
 termination_strategy         : string;
 cpc                          : cpctype; (* critical pair criterion used *)
 do_cpc_caching               : bool;
 check_isomorphism_with       : isomorphism_checks;
 check_isomorphisms_repeatedly: bool;
 (*process_cost: process_cost_type;*)
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

(*** FUNCTIONS ***********************************************************)
let default_options = {
 axioms = [];
 filename = "";
 mode = Completion;
 timeout = 60.0;
 ttimeout = 3.0;
 verbosity = 0;
 rewrite_index = CodeTrees;
 unification_index = DiscriminationTrees;
 check_externally = true;
 certification_output = false;
 tool = "callmuterm "; (* does not matter if no external checker used *)
 termination_strategy = "dp; matrix -dp -ib 2 -dim 1 -direct";
 cpc = Prime;
 do_cpc_caching = true;
 check_isomorphism_with = Renamings;
 check_isomorphisms_repeatedly = false;
 ordered_completion = false;
 goal = None;
 selection_strategy = SelectionStrategy.japan_max;
 strategy_macro = Some SelectionStrategy.JapanMax;
 use_orphan_check = false;
 size_age_ratio = 0.65;
 node_size = Sum;
 no = false;
 rewrite_before_termination_check = false, 0;
 conjecture = None;
 output_proof = NoProof;
 output_statistics = false;
 output_trs = false;
 kill_processes = 0.0;
 s_theory_file = "";
 s_theory = Trs.empty;
 theory = []
}

let axioms o = o.axioms

let filename o = o.filename

let termination_strategy o = o.termination_strategy 

let verbosity o = o.verbosity

let timeout o = o.timeout

let ttimeout o = o.ttimeout

let certification_output o = o.certification_output

let command o = o.tool

let check_externally o = o.check_externally

let rewrite_index o = o.rewrite_index

let unification_index o = o.unification_index

let cpc o = o.cpc

let do_cpc_caching o = o.do_cpc_caching

let check_isomorphism o = o.check_isomorphism_with
 
let check_isomorphisms_repeatedly o =
 o.check_isomorphisms_repeatedly
;;

let is_ordered o = o.ordered_completion

let set_kill o k = {o with kill_processes = k};;

let goal o = o.goal

let has_goal o = 
 match o.goal with
  | None -> false
  | _ -> true
;;

let selection_strategy o = o.selection_strategy

let selection_macro o = o.strategy_macro

let use_orphan_check o = o.use_orphan_check

let size_age_ratio o = o.size_age_ratio

let mode o = o.mode

let node_size o = o.node_size

let rewrite_before_termination_check o = o.rewrite_before_termination_check

let conjecture o = o.conjecture

let is_successful = function
 | Suc 
 | Sat
 | Noc
 | Uns -> true
 | Unk
 | Gup _ 
 | Tmo -> false
;;

let set_axioms o eqs = {o with axioms = eqs}

let set_goal o e = {o with goal = e}

let kill_processes o = o.kill_processes

let s_theory_file o = o.s_theory_file

let s_theory o = o.s_theory

let theory o = o.theory

let output_proof o = o.output_proof
