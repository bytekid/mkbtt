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
module Fun = Rewriting.Function;;
module Var = Rewriting.Variable;;
module Term = U.Term;;
module Trs = U.Trs;;
module C = Completion;;
module H = Hashtbl;;
module T = Types;;
module CP = T.CompletionProcess;;
module IntNodeIndex = Index.Isomorphic (Int) (T.Node);;
module IntWNodeIndex = Index.Isomorphic (Int) (T.WaldmeisterGoalNode);;
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
 t_mkb_srewrite          : float; (* time for S-rewrite steps *)
 t_mkb_deduce           : float; (* time for deduce steps *)
 (* figures related to ordered rewriting for  ordered completion *)
 n_ordrewrite_attempts  : int; (* number of ordered rewrite attempts *)
 n_ordrewrite_tcalls    : int; (* number of required termination calls *)
 n_ordrewrite_ok       : int; (* number of successful ordered rewrites *)
 (* for critical pair criteria *)
 redundant_cps          : (int list,  int) Hashtbl.t; (* for processes *)
 n_redundant_cps        : int; (* number of CPs redundant for some process*)
 t_cpc                  : float; (* time to compute CPC *)
 t_process_kill         : float; (* time required for process killing *)
 n_victims              : int; (* number of victims *)
 (* related to selection *)
 t_selection            : float; (* time required for node selection *)
 (* for various debugging purposes *)
 t_one                  : float;
 t_two                  : float;
}

type processes_context = {
 all            : CP.t list;
 count          : int;
 choices        : (CP.t, int) H.t
}

type node_pool_context = {
 ids: IntNodeIndex.t;
 equations: (Equation.t, int) H.t;
}


type node_state_context = {
 open_nodes: int list;
 closed_nodes: int list;
 ext_nodes: int list;
 s_nodes: int list;
 term_index: (int * bool) Types.ACDiscTree.t;
 sterm_index: (int * bool) Types.ACDiscTree.t;
 current: int list;
 fixed_rules: Trs.t
}

type cpc_state_context = {
 cpc_cache: (Term.t, CP.t list) H.t
}

type choice_state_context = {
 ccount: int
}

type context = {
 options      : C.options;
 statistics   : statistics_context;
 processes    : processes_context;
 node_pool    : node_pool_context;
 node_state   : node_state_context;
 cpc_state    : cpc_state_context;
 choice_state : choice_state_context;
 logs         : string list
}

(*** SUBMODULES (2) ******************************************************)
module Statex = struct type t = context end;;
module Monad = Util.Monad.Transformer.State (Statex) (U.Monad);;

(*** OPENS ***************************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

let initial_statistics = {
 t_termination          = 0.0;
 n_termination_calls    = 0;
 n_termination_yes      = 0;
 n_termination_timeouts = 0;
 t_variants             = 0.0;
 t_encs                 = 0.0;
 t_overlaps1            = 0.0;
 t_overlaps2            = 0.0;
 t_insert               = 0.0;
 t_delete               = 0.0;
 n_iterations           = 0;
 n_processes            = 0;
 n_nodes                = 0;
 t_mkb_orient           = 0.0;
 t_mkb_rewrite          = 0.0;
 t_mkb_srewrite         = 0.0;
 t_mkb_deduce           = 0.0;
 n_ordrewrite_attempts  = 0;
 n_ordrewrite_tcalls    = 0;
 n_ordrewrite_ok        = 0;
 n_redundant_cps        = 0;
 redundant_cps          = Hashtbl.create 100;
 t_cpc                  = 0.0;
 t_process_kill         = 0.0;
 n_victims              = 0;
 t_selection            = 0.0;
 t_one                  = 0.0;
 t_two                  = 0.0;
}

let initial_processes =
 let counter = H.create 20 in
 H.add counter CP.initial 0;
 {
 all = [CP.initial];
 count = 1;
 choices = counter
 }
;;

let initial_node_pool = {
 ids       = IntNodeIndex.empty 200;
 equations = H.create 200;
}

let initial_node_state = {
 open_nodes   = []; 
 closed_nodes = [];
 ext_nodes    = [];
 s_nodes      = []; 
 term_index   = Types.ACDiscTree.empty;
 sterm_index  = Types.ACDiscTree.empty; 
 current      = [-1];
 fixed_rules  = Trs.empty
}

let initial_cpc_state = {
 cpc_cache = H.create 20
}

let initial_choice_state = {
 ccount = 0
}

let initial_context = {
  options                = C.default_options;
  statistics             = initial_statistics;
  processes              = initial_processes;
  node_pool              = initial_node_pool;
  node_state             = initial_node_state;
  cpc_state              = initial_cpc_state;
  choice_state           = initial_choice_state;
  logs                   = []
}

let initial_context_with o = 
 (*let ix = Types.NodeTermIndex.make o in
 let ns = {initial_node_state with term_index = ix} in*)
 {initial_context with options = o(*; node_state = ns*)}
;;

let get_options = get >>= fun c -> return c.options

let set_options o = get >>= fun c -> set {c with options = o}

let set_axioms eqs = 
 get >>= fun c -> set {c with options = (C.set_axioms c.options eqs)}
;;

let set_goal g =
 get >>= fun c -> set {c with options = (C.set_goal c.options g)}
;;

let get_statistics = get >>= fun c -> return c.statistics

let set_statistics s = get >>= fun c -> set {c with statistics = s}

let get_processes = get >>= fun c -> return c.processes

let set_processes s = get >>= fun c -> set {c with processes = s}

let get_node_pool = get >>= fun c -> return c.node_pool

let set_node_pool s = get >>= fun c -> set {c with node_pool = s}

let get_node_state = get >>= fun c -> return c.node_state

let set_node_state g = get >>= fun c -> set {c with node_state = g}

let get_cpc_state = get >>= fun c -> return c.cpc_state

let set_cpc_state s = get >>= fun c -> set {c with cpc_state = s}

let get_choice_state = get >>= fun c -> return c.choice_state

let set_choice_state s = get >>= fun c -> set {c with choice_state = s}

let get_logs = get >>= fun c -> return (List.rev c.logs)

let log v s =
 get >>= fun c -> 
 if C.verbosity c.options >= v then (
  Format.printf "%s\n%!" s;
  set {c with logs = s :: c.logs})
 else
  return ()
;;
(*** SUBMODULES (2) ******************************************************)

module M = struct
 module Sig = struct
  let create_fun x y = liftm (U.Monad.create_fun x y)
  let fresh_var = liftm U.Monad.fresh_var
 end

 module Term = struct
  let rename t = liftm (Term.rename t)
  let to_stringm t = liftm (Term.to_stringm t)
  let fprintfx fmt t = liftm (Term.fprintfx fmt t)
 end

 module Rule = struct
  let lift1 f x = liftm (f x)
  let lift2 f x y = liftm (f x y)
  let overlaps = lift2 U.Rule.overlaps
  let rename r = liftm (U.Rule.rename r)
  let to_stringm r = liftm (U.Rule.to_stringm r)
  let fprintfx = lift2 U.Rule.fprintfx;;
 end

 module Trs = struct
  let theory trs = liftm (U.Trs.theory trs);;
  let to_stringm t = liftm (Trs.to_stringm t);;
  let fprintfx f t = liftm (Trs.fprintfx f t);;
 end

 module Termx = struct
 type pos = ACPosition.t
  let normalize_flatten_both x y = liftm (Termx.normalize_flatten_both x y)
  let flatten t = liftm (Termx.flatten t)
  let funs_pos t = liftm (Termx.funs_pos t)
  let funs_pos_below_root t = liftm (Termx.funs_pos_below_root t)
  let funs_pos_extended rl t = liftm (Termx.funs_pos_extended rl t)
  let ac_equivalent s t = liftm(Termx.ac_equivalent s t)
  let unflatten t = liftm (Termx.unflatten t)
  let readable t = liftm (Termx.readable t)
 end

 module Rulex = struct
  let extend x = liftm (Rulex.extend x)
  let narrow x y z = liftm (Rulex.narrow x y z)
  let unflatten t = liftm (Rulex.unflatten t)
  let readable t = liftm (Rulex.readable t)
 end

 module Equation = struct
  let list_to_stringm t = liftm (Equation.list_to_stringm t);;
  let oriented_of_terms s t = liftm (Equation.oriented_of_terms s t);;
  let to_stringm e = liftm (Equation.to_stringm e)
 end

 module Trsx = struct
  let lift1 f x = liftm (f x);;
  let lift2 f x y = liftm (f x y)
  let critical_pairs = lift1 Trsx.critical_pairs;;
  let to_wst_stringm = lift1 Trsx.to_wst_stringm;;
  let readable = lift1 Trsx.readable;;
  let normal_form = lift2 Trsx.normal_form;;
  let is_reducible = lift2 Trsx.is_reducible
  let unflatten = lift1 Trsx.unflatten
 end

 module ACLogic = struct
  let ac_root f = liftm (ACLogic.ac_root f)
 end

 module ACRewrite = struct
  let matches s = liftm (ACRewrite.matches s)
  let rewrite1 t rl = liftm (ACRewrite.rewrite1 t rl)
  let rewrite2 t p rl = liftm (ACRewrite.rewrite2 t p rl)
  let rewrite_with_at t rl p = liftm (ACRewrite.rewrite_with_at t rl p)
  let reducible_below_root t rl p = liftm (ACRewrite.reducible_below_root t rl p)
  let reducible_at t p r = liftm (ACRewrite.reducible_at t p r)
  let nf_with ss t rl p = liftm (ACRewrite.nf_with ss t rl p)
  let reducts t p rl = liftm (ACRewrite.reducts t p rl)
  let normalize trs t = liftm (ACRewrite.normalize trs t)
  let narrow t rl ps = liftm (ACRewrite.narrow t rl ps)
  let narrow_below_root t rl ps = liftm (ACRewrite.narrow_below_root t rl ps)
  let joinable e trs = liftm (ACRewrite.joinable e trs)
 end


 module ACDiscTree = struct
  type 'a t = 'a Types.ACDiscTree.t
  let empty = ACDiscTree.empty
  let insert x y  = liftm (ACDiscTree.insert x y)
  let variants x y = liftm (ACDiscTree.variants x y)
  let encompassments x y = liftm (ACDiscTree.encompassments x y)
  let encompassments_below_root x y = liftm (ACDiscTree.encompassments_below_root x y)
 end

end
