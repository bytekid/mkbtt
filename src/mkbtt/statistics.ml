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

(**
 @author Sarah Winkler
 @since  2008/09/17 *)

(** Plenty of statistics for completion; both Slothrop-like and
    wih multicompletion *)

(*** OPENS ********************************************************************)
open Util;;
(*** SUBMODULES **********************************************************)
module C = Completion;;
module Rewriting = Processors.Rewritingx;;
module Term = Rewriting.Term;;
module Rule = Rewriting.Rule;;
module Trs = Rewriting.Trs;;
module M = World.Monad;;

(*** OPENS ***************************************************************)
open World;;

(*** FUNCTIONS ***********************************************************)
let (>>=) = Monad.(>>=)
let (>>) = Monad.(>>)
let return = Monad.return

let update u = 
 get_statistics >>= fun s ->
 set_statistics (u s)
;;

let add_t_termination t =
 update (fun s -> {s with t_termination = s.t_termination  +. t})
;;

let inc_n_termination_calls =
 let inc c = {c with n_termination_calls = c.n_termination_calls + 1} in
 update inc
;;

let inc_n_termination_yes =
 let inc c = {c with n_termination_yes = c.n_termination_yes + 1} in
 update inc
;;

let inc_n_termination_timeouts =
 let i c = {c with n_termination_timeouts = c.n_termination_timeouts + 1} in
 update i
;;

let add_t_variants t =   
 update (fun c -> {c with t_variants = c.t_variants +. t})
;;

let add_t_encs t =
 update (fun c -> {c with t_encs = c.t_encs +. t})
;;

let add_t_overlaps1 t =
 update (fun c -> {c with t_overlaps1 = c.t_overlaps1 +. t})
;;

let add_t_overlaps2 t =
 update (fun c -> {c with t_overlaps2 = c.t_overlaps2 +. t})
;;

let add_t_insert t =
 update (fun c -> {c with t_insert = c.t_insert +. t})
;;

let add_t_delete t =
 update (fun c -> {c with t_delete = c.t_delete +. t})
;;

let inc_n_iterations =
 let inc c = {c with n_iterations = c.n_iterations + 1} in
 update inc
;;

let inc_n_nodes =
 let inc c = {c with n_nodes = c.n_nodes + 1} in
 update inc
;;

let n_processes =
 get_statistics >>= fun s ->
 return s.n_processes
;;

let inc_n_processes =
 let inc c = {c with n_processes = c.n_processes + 1} in
 update inc
;;

let dec_n_processes k =
 let dec c = {c with n_processes = c.n_processes - k} in
 update dec
;;

let add_t_orient t =
 update (fun c -> {c with t_mkb_orient = c.t_mkb_orient +. t})
;;

let add_t_rewrite t =
 update (fun c -> {c with t_mkb_rewrite = c.t_mkb_rewrite +. t})
;;

let add_t_deduce t =
 update (fun c -> {c with t_mkb_deduce = c.t_mkb_deduce +. t})
;;

let add_n_ordrewrite_attempts n =
 let i c = {c with n_ordrewrite_attempts=c.n_ordrewrite_attempts + n} in
 update i
;;

let add_n_ordrewrite_tcalls n =
 let i c = {c with n_ordrewrite_tcalls = c.n_ordrewrite_tcalls + n} in
 update i
;;

let add_n_ordrewrite_ok n =
 let i c = {c with n_ordrewrite_ok = c.n_ordrewrite_ok + n} in
 update i
;;

let add_t_selection t =
 update (fun c -> {c with t_selection = c.t_selection +. t})
;;

let add_t_process_kill t =
 update (fun c -> {c with t_process_kill = c.t_process_kill +. t})
;;

let add_n_victims k =
 let i c = {c with n_victims = c.n_victims + k} in
 update i
;;

let inc_n_redundant_cps =
 let i c = {c with n_redundant_cps = c.n_redundant_cps + 1} in
 update i
;;

(* count redundant critical pairs per process *)
let add_cp_for_process p =
 let u c =
 try let i = Hashtbl.find c.redundant_cps p in
  Hashtbl.replace c.redundant_cps p (i + 1); c
 with Not_found -> Hashtbl.add c.redundant_cps p 1; c
 in update u
;;

let add_cp_for_processes pset = 
 inc_n_redundant_cps >>
 Monad.iter add_cp_for_process pset
;;

let add_t_cpc t =
 update (fun c -> {c with t_cpc = c.t_cpc +. t})
;;

let rec is_prefix_of p q =
 match p, q with
  | [], _ -> true
  | a::p', b::q' when a=b -> is_prefix_of p' q'
  | _ -> false
;;

(* needed by sum_cps_for_process in statistics.ml *)
let sum_cps_for p p' n s =
 if is_prefix_of p' p then n + s else s
;;

let get_redundant_cps_for p =
 get_statistics >>= fun c -> 
 return (Hashtbl.fold (sum_cps_for p) c.redundant_cps 0)
;;

let add_t_isomorphisms t =
 update (fun c -> {c with t_isomorphisms = c.t_isomorphisms +. t})
;;

let add_t_one t =
 update (fun c -> {c with t_one = c.t_one +. t})
;;

let add_t_two t =
 update (fun c -> {c with t_two = c.t_two +. t})
;;


let print_ordered =
 get_statistics >>= fun c -> return (
 Format.printf "ordered rewrite attempts: %i, tcalls: %i, ok: %i\n" 
  c.n_ordrewrite_attempts c.n_ordrewrite_tcalls c.n_ordrewrite_ok)
;;

let general =
 get_options >>= fun o -> 
 return (Format.printf "\nSTATISTICS: \n%!")
;;

let index =
 get_options >>= fun o ->
 get_statistics >>= fun c -> return (
  let get_string f = 
   match f o with
    | C.Naive -> "naive"
    | C.PathIndexing -> "path indexing"
    | C.DiscriminationTrees -> "discrimination tree"
    | C.CodeTrees -> "code tree"
  in
  Format.printf "\nIndexing\n";
  Format.printf " techniques: %s (rewriting) %s (overlaps)\n" 
   (get_string C.rewrite_index) 
   (get_string C.unification_index);
  Format.printf " variants:                %.2f\n" c.t_variants;
  Format.printf " encompassments:          %.2f\n" c.t_encs;
  Format.printf " overlaps:                %.2f\n" 
   (c.t_overlaps1 +. c.t_overlaps2);
  Format.printf " maintenance:             %.2f\n" 
   ((c.t_insert) +. (c.t_delete));
  Format.printf "    One: %f\n" c.t_one;
  Format.printf "    Two: %f\n" c.t_two )
;;

let cpc p =
 get_options >>= fun o ->
 get_statistics >>= fun c -> 
 let cpc = C.cpc o in
 let sl = C.propagate_small_lemmata o in
 get_redundant_cps_for p >>= fun n ->
 if (cpc <> C.NoCPC) || (sl > 0) then
  (
  Format.printf "\nDeduction \n";
  if (cpc <> C.NoCPC) then (
   Format.printf " critical pair criterion:   %s \n"
    (match cpc with C.Prime -> "primality"
      | C.Connected -> "connectedness"
      | C.Blocked -> "blocked"
      | _ -> "all");
   Format.printf " redundant CPs in total:    %i\n" c.n_redundant_cps; 
   Format.printf " for successful process:    %i\n" n;
   Format.printf " required time:             %.2f\n" c.t_cpc);
  if (sl > 0) then
   Format.printf " small lemmata propagation: %i\n" sl;
  return ()
  ) else return ()
;;

let isomorphism =
 get_statistics >>= fun c ->
 get_options >>= fun o ->
 let check =
  let rep = C.check_isomorphisms_repeatedly o in
  match C.check_isomorphism o with
   | C.NoChecks -> "none"
   | C.Renamings when rep -> "renamings+"
   | C.Renamings -> "renamings"
   | C.Permutations when rep -> "term permutations+"
   | C.Permutations -> "term permutations"
 in
 Format.printf "\nIsomorphism Check: %s %s\n" check
  (if C.detect_isomorphism o then "(detected automatically)" else "");
 if C.check_isomorphism o <> C.NoChecks then
  Format.printf " time for checking: %f\n%!" c.t_isomorphisms;
 return ()
;;

let selection p =
 get_statistics >>= fun c ->
 get_options >>= fun o ->
 let str = C.selection_strategy o in
 Format.printf "\nSelection\n"; 
 let s = SelectionStrategy.to_string str in
 if String.length s <= 70 then
  Format.printf " strategy: %s\n" s
 else (
  let i = try String.index_from s 60 ' ' with not_found -> 60 in
  Format.printf " strategy: %s\n" (String.sub s 0 i);
  Format.printf " %s\n" (String.sub s i (String.length s - i))
 );
 return (Format.printf " time for selection:      %.2f\n" c.t_selection)
(* Format.printf " winning selection rate:  %2.f\n" 
  (CProcess.choice_ratio_for p !iterations)*)
;;

let process_kill =
 get_statistics >>= fun c ->
 get_options >>= fun o ->
 let kp = C.kill_processes o in
 if kp > 0.0 then (
  Format.printf "\nProcess Killing\n";
  let p = int_of_float ((kp -. 0.99) *. 100.0) in
  let t = c.t_process_kill in
  let n = c.n_victims in
  Format.printf " killed %i exceeding best by: %i%%\n" n p;
  Format.printf " required time:               %.2f\n" t
 );
 return ()
;;

let termination =
 get_statistics >>= fun c ->
 get_options >>= fun o ->
 Format.printf "\nTermination Checks ";
 if C.check_externally o then
  (Format.printf "(external with %s)\n"
  (C.command o)
  )
 else (
  Format.printf " (internal)\n";
  Format.printf " strategy: %s\n"
   (C.termination_strategy o)
  );
 Format.printf (" termination checks:      %i (yes: %i, timeouts: %i)\n")
   c.n_termination_calls c.n_termination_yes c.n_termination_timeouts;
 Format.printf " time limit for check:    %.2f\n"
   (C.ttimeout o);
 Format.printf " termination time:        %.2f\n" (c.t_termination);
 return ()
;;


let mkb_general =
 get_statistics >>= fun c ->
 Format.printf "General\n"; 
 Format.printf " number of iterations:    %i \n" c.n_iterations;
 Format.printf " number of nodes:         %i \n" c.n_nodes;
 Format.printf " number of processes:     %i \n" c.n_processes;
 Format.printf " time for orient:         %.2f\n" c.t_mkb_orient;
 Format.printf " time for rewrite:        %.2f\n" c.t_mkb_rewrite;
 Format.printf " time for deduce:         %.2f\n" c.t_mkb_deduce;
 return ()
;;

let mkb total_time p =
 get_statistics >>= fun c ->
 get_options >>= fun o ->
 mkb_general >>
 isomorphism >>
 selection p >>
 process_kill >>
 termination >>
 cpc p >>
 index >>
 if C.is_ordered o then print_ordered else return ()
;;

let slothrop total_time =
 get_statistics >>= fun c ->
 Format.printf "total time:   %.2f\n" total_time;
 Format.printf "termination:  %.2f\n\n" c.t_termination;
 return ()
;;

let print p total_time =
 get_statistics >>= fun c ->
  get_options >>= fun o ->
 general >>
 if not (C.use_slothrop o) then 
  mkb total_time p 
 else 
  slothrop total_time
;;

let print_failure = print []

let print_timeout = print []

let rule_to_string rule del =
 let l, r = Rule.lhs rule, Rule.rhs rule in
 (Term.to_string l) ^ " " ^ del ^ " " ^ (Term.to_string r)
;;

let system_to_string del trs =
 Trs.fold (fun rule s -> (rule_to_string rule del) ^ "\n" ^ s) "" trs
;;

let trs_to_string = system_to_string "->"

let eqs_to_string eqs = 
 List.fold_right (fun eq s -> (Equation.to_string eq) ^ "\n" ^ s) eqs ""
;;

let print_system r_omega e_omega =
 get_statistics >>= fun c ->
 get_options >>= fun o ->
 if C.is_ordered o then
  begin
   log 0 "\nGROUND COMPLETE SYSTEM:" >>
   M.Trsx.to_stringm r_omega >>= log 0 >>
   M.Equation.list_to_stringm e_omega >>= log 0
  end
 else
  log 0 "\nCOMPLETE SYSTEM:" >>
  M.Trsx.to_stringm r_omega >>= log 0
;;

let goal_to_string = function
 | C.Universal eqs 
 | C.Existential eqs -> 
  M.Equation.list_to_stringm eqs
;;

let print_goal status e0 =
 get_statistics >>= fun c ->
 get_options >>= fun o ->
 goal_to_string (C.goal o) >>= fun gs ->
 Format.printf "From the initial equations \n%s\n" (eqs_to_string e0);
 if status = C.Noc then
  Format.printf "the goal \n %s\ncould not be proven.\n" gs
 else
  Format.printf "the goal \n%s\n was proven.\n" gs;
 return ()
;;

let print_success e0 p t = print p t

let print_proof status e0 p t =
 print_goal status e0 >>
 print p t
;;

let print_basic status problem t =
 let s = 
 let prefix = "% SZS status " in
 prefix ^ (
 match status with
  | C.Suc -> "Success"
  | C.Sat -> (*"Satisfiable"*) "Success"
  | C.Uns -> "Unsatisfiable"
  | C.Noc -> "NoConsequence"
  | C.Unk -> "Unknown"
  | C.Gup s -> "GaveUp (" ^ s ^ ")"
  | C.Tmo -> "Timeout"
 ) ^ " for " ^ problem
 in Format.printf "%s\n" s;
 Format.printf "%.2f (total time)\n" t
;;

let log s i =
 get_options >>= fun o ->
 if C.verbosity o >= i then Format.printf "%s\n" s;
 return ()
;;

let blog b s i = if b then log s i else return ()

let print_logs =
 get_logs >>= fun ls ->
 List.iter (fun s -> Format.printf "%s\n" s) ls;
 return ()
;;
