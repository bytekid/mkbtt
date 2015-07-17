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

(** Main file
@author Sarah Winkler
@since  2007/05/10 *)

open Util;;

(*** SUBMODULES **********************************************************)
module F = Format;;
module C = Completion;;
module W = World;;
module St = Statistics;;
module Monad = W.Monad;;
module NS = NodeState;;
module PI = ProcessIsomorphism;;
module RMonad = U.Monad;;
module TrsParser = U.MyTrsParser;;
module TrsLexer = U.MyTrsLexer;;
module TrsSyntax = U.MyTrsSyntax;;
module Problem = U.MyProblem;;
module Xml = U.MyXml;; 
module Trs = U.Trs;;

(*** OPENS ***************************************************************)
open Monad;;
open Completion;;

(*** EXCEPTIONS **********************************************************)
exception Help;;
 
(*** GLOBALS *************************************************************)

let log0 = W.log 0

let log1 = W.log 1 

let log2 = W.log 2

let check_options =
 W.get_options >>= fun options ->
 let strategy_string = Completion.termination_strategy options in
 log2 ("Main check: strategy "^strategy_string)
;;

(* termination strategies *)
let s_linpoly = "(matrix -dim 1 -ib 1 -ob 4)+";;
let s_poly1 = "(poly -ib 1 -ob 4)+";;
let s_poly2 = "(poly -ib 2 -ob 4)+";;
let s_kbo = "(kbo -smt || ref;kbo)+";;
let s_lpo = "(lpo || ref;lpo)+";;
let s_total1 = "(kbo | lpo | matrix -dim 1 -ib 3 -ob 4)+";;
let s_total2 = "(kbo -smt || lpo || matrix -dim 1 -ib 3 -ob 3 || ref;kbo)+";;

let s_microlpo = "dp;(odg?; sccs?; (sc | csf | lpo))*";;
let s_microkbo = "dp;(odg?; sccs?; (sc | csf | kbo -ib 2))*";;
let s_microlpokbo = "dp;(odg?; sccs?; (sc | csf | lpo | kbo -ib 2))*";;

(* default settings *)
let flags = ref C.default_options
let mascott = ref false
let selection_strategy_string = ref ""
(***** functions to treat flags *****)

let yesno = ref false
let output_proof = ref ""

let set_cpc s =
 if (String.compare s "prime") == 0 then 
  flags := {!flags with cpc = C.Prime}
 else if (String.compare s "blocked") == 0 then 
  flags := {!flags with cpc =  C.Blocked}
 else if (String.compare s "connected") == 0 then 
  flags := {!flags with cpc =  C.Connected}
 else if (String.compare s "all") == 0 then 
  flags := {!flags with cpc =  C.All}
 else
  flags := {!flags with cpc = C.NoCPC}
;;

let set_selection s =
 selection_strategy_string := s;
 if (String.compare s "sum") == 0 then
  flags := {!flags with 
    selection_strategy = SelectionStrategy.japan_sum;
    strategy_macro = Some(SelectionStrategy.JapanSum)}
 else if (String.compare s "max") == 0 then
  flags := {!flags with  
    selection_strategy = SelectionStrategy.japan_max;
    node_size = C.Max;
    strategy_macro = Some(SelectionStrategy.JapanMax)}
 else 
  (flags := {!flags with strategy_macro = None};
   if (String.compare s "slothrop") == 0 then
    flags := {!flags with selection_strategy = SelectionStrategy.slothrop}
  else
   let strategy = SelectionStrategyAux.of_string s in
   flags := {!flags with selection_strategy = strategy}
  )
;;

let set_iso s =
 if (String.compare s "auto") <> 0 then (
  flags := {!flags with detect_iso_auto = false};
  if (String.compare s "none") == 0 then
   flags := {!flags with check_isomorphism_with = C.NoChecks}
  else if (String.compare s "rename") == 0 then 
   flags := {!flags with check_isomorphism_with = C.Renamings} 
  else if (String.compare s "rename+") == 0 then
   flags := {!flags with 
    check_isomorphism_with = C.Renamings;
    check_isomorphisms_repeatedly = true}
  else if (String.compare s "perm") == 0 then 
   flags := {!flags with check_isomorphism_with = C.Permutations}
  else if (String.compare s "perm+") == 0 then
   flags := {!flags with
    check_isomorphism_with = C.Permutations;
    check_isomorphisms_repeatedly = true}
  )
;;

let set_rindex i = flags := {!flags with rewrite_index = i}

let set_uindex i = flags := {!flags with unification_index = i}

let set_index set s =
 if (String.compare s "dt") == 0 then set C.DiscriminationTrees
 else if (String.compare s "pi") == 0 then set C.PathIndexing
 else if (String.compare s "ct") == 0 then set C.CodeTrees
 else set C.Naive
;;

let set_strategy s =
 if (String.compare s "slinpoly") == 0 then
  flags := {!flags with termination_strategy = s_linpoly}
 else if (String.compare s "spoly1") == 0 then
  flags := {!flags with termination_strategy = s_poly1}
 else if (String.compare s "spoly2") == 0 then
  flags := {!flags with termination_strategy = s_poly2}
 else if (String.compare s "slpo") == 0 then
  flags := {!flags with termination_strategy = s_lpo}
 else if (String.compare s "skbo") == 0 then
  flags := {!flags with termination_strategy = s_kbo}
 else if (String.compare s "smicrolpo") == 0 then
  flags := {!flags with termination_strategy = s_microlpo}
 else if (String.compare s "smicrokbo") == 0 then
  flags := {!flags with termination_strategy = s_microkbo}
 else if (String.compare s "smicrolpokbo") == 0 then
  flags := {!flags with termination_strategy = s_microlpokbo}
 else if (String.compare s "stotal1") == 0 then
  flags := {!flags with termination_strategy = s_total1}
 else if (String.compare s "stotal2") == 0 then
  flags := {!flags with termination_strategy = s_total2}
 else flags := {!flags with termination_strategy = s};
  flags := {!flags with check_externally = false}
;;

let set_cmd c =
 flags := {!flags with 
  tool = "./" ^ c; 
  termination_strategy = "";
  check_externally = true}
;;


let set_completion_check c =
 flags := {!flags with mode = CompletenessCheck(c)}
;;

let set_goal g =
 if (String.compare g "wm") = 0 then
  flags:= {!flags with universal_goal = C.WaldmeisterGoal}
 else if (String.compare g "rewrite") = 0 then
  flags:= {!flags with universal_goal =  C.Rewrite2NF}
 else
  flags:= {!flags with universal_goal =  C.NodeGoal}
;;

let set_kill_processes r = flags := {!flags with kill_processes = r}

let set_output_trs () = flags := {!flags with output_trs = true}

let set_output_proof p = 
 output_proof := p;
 if String.compare p "szs" = 0 then 
  flags := {!flags with output_proof = SZS}
 else if String.compare p "cpfconv" = 0 then 
  flags := {!flags with output_proof = CpfConversion}
 else if String.compare p "cpftree" = 0 then
  flags := {!flags with output_proof = CpfEquationalProofTree}
 else if String.compare p "cpfsub" = 0 then
  flags := {!flags with output_proof = CpfEquationalProofTreeWithLemmas}
 else flags := {!flags with output_proof = NoProof}
;;

let set_output_stats () = flags := {!flags with output_statistics = true}

let set_cert_output () = flags := {!flags with certification_output = true}

let set_ordered () = flags := {!flags with ordered_completion = true}

let set_slothrop () = flags := {!flags with slothrop = true}

let set_small_lemmata_propagation k = 
 flags := {!flags with propagate_small_lemmata = k}
;;

let set_timeout f = flags := {!flags with timeout = f}
let set_ttimeout f = flags := {!flags with ttimeout = f}
let set_verbosity v = flags := {!flags with verbosity = v}
let set_theory_file f = flags := {!flags with s_theory_file = f}
let clear_output _ = flags := {!flags with no = true}

(* Available command line switches. *)
let sws = [
 ("-cert", Arg.Unit set_cert_output,
  "produces certifiable output");
 ("-ch", Arg.String set_completion_check,
  "supply rewrite system to check whether it is complete and joins the input equalities");
 ("-cp", Arg.String set_cpc,
  "critical pair criterion (prime, blocked, connected or all)");
 ("-ct", Arg.Unit set_output_trs,
  "output completed TRS");
 ("-g", Arg.String set_goal,
  "goal type (rewrite, node, or wm)");
 ("-help", Arg.Unit (fun () -> raise Help),
  "display this list of options");
 ("-h", Arg.Unit (fun () -> raise Help),
  "display this list of options");
 ("-is", Arg.String set_iso,
  "isomorphism checks (rename, rename+, perm, perm+ or auto)");
 ("-ix", Arg.String (set_index set_rindex),
  "indexing technique for rewriting (nv, pi, dt or ct)");
 ("-kp", Arg.Float set_kill_processes,
  "threshold to kill processes, > 1.0");
 ("-n", Arg.Unit (fun _ -> mascott := true),
  "use normalized completion");
 ("-no", Arg.Unit clear_output,
  "suppress output");
 ("-p", Arg.String set_output_proof,
  "output proof (cpfconv, cpftree, cpfsub, or szs)");
 ("-ss", Arg.String set_selection,
  "selection strategy (sum, max, old and slothrop are predefined)");
 ("-o", Arg.Unit set_ordered,
  "ordered completion");
 ("-s", Arg.String set_strategy,
  "termination strategy for TTT2, e.g. kbo, spoly1, stotal2");
 ("-sl", Arg.Int set_small_lemmata_propagation,
  "term size threshold for small lemmata used for all processes");
(* ("--slothrop", Arg.Unit set_slothrop,
  "Use Slothrop.");*)
 ("-st", Arg.Unit set_output_stats,
  "output statistics");
 ("-t", Arg.Float set_timeout,
  "global time limit");
 ("-T", Arg.Float set_ttimeout,
  "termination call time limit");
 ("-th", Arg.String set_theory_file,
  "theory S with respect to which completion performed");
 ("-tp", Arg.String set_cmd,
  "executable script for calling external termination prover");
 ("-ui", Arg.String (set_index set_uindex),
  "indexing technique for overlaps (nv, pi, dt)");
 ("-v", Arg.Int set_verbosity,
  "verbosity in {0, 1, 2}");
 ("-yesno", Arg.Set yesno,
  "output YES or MAYBE as first line of result");
]

(* Align the help message nicely. *)
let sws = Arg.alignx 80 sws;;

(*** FUNCTIONS ***********************************************************)
(*let usage cmd = Format.sprintf "Usage: %s [options] <file>@\n" cmd;;*)
let usage = "Usage: ./mkbtt [options] <file> [timeout]\n\nOptions:";;

let init_xml () =
 let module S = String in
 let open_tag s = "<"^s^">" in
 (* don't print attributes in closing tags *)
 let modify s = if S.contains s ' ' then S.sub s 0 (S.index s ' ') else s in
 let close_tag s = "</"^ modify s ^">" in
 F.set_formatter_tag_functions {
  F.mark_open_tag   = open_tag;
  F.mark_close_tag  = close_tag;
  F.print_open_tag  = (fun _ -> F.open_box 0);
  F.print_close_tag = (fun _ -> F.close_box());
 };
 F.set_print_tags true; F.set_mark_tags true;
 (* also enable tags for generated strings *)
 F.pp_set_formatter_tag_functions F.str_formatter {
  F.mark_open_tag   = open_tag;
  F.mark_close_tag  = close_tag;
  F.print_open_tag  = (fun _ -> F.pp_open_box F.str_formatter 0);
  F.print_close_tag = (fun _ -> F.pp_close_box F.str_formatter ());
 };
 F.pp_set_print_tags F.str_formatter true;
 F.pp_set_mark_tags F.str_formatter true;
;;

let configurate argv =
 F.pp_set_margin F.std_formatter 100; init_xml ();
 if Array.length argv = 1 then (Arg.usagex sws usage; exit 0);
 let file_is_set = ref true in
 let push_file n = 
  if !file_is_set then
   (file_is_set := false; 
   flags := {!flags with filename = n})
  else
   (Arg.usagex sws usage; exit 0)
 in
 Arg.parse sws push_file usage;
;;

let if_do b m = if b then m else return ()

let szs_completion_output p ns =
 History.joint_history [] ns >>= fun h ->
 W.get_options >>= fun o ->
 History.as_string p (C.filename o) h >>= fun s ->
 return (s, "Ground complete system derived.")
;;

let print_proof s p trs =
 W.get_options >>= fun o ->
 NS.er_contain_closed p >>= fun ns ->
 if C.output_proof o = SZS then (
  szs_completion_output p ns >>= fun (proof, explanation) ->
  Format.printf "PROOF:\n";
  Format.printf "%s SZS output start\n" "%";
  Format.printf "%s" proof;
  Format.printf "%s SZS output end\n" "%";
  return ())
 else 
 match Completion.goal o with
 Completion.Universal _ -> (
  if !yesno then Format.printf "YES\n%!";
  if C.output_proof o = CpfConversion then (
   Conversion.for_goal >>= CertOutput.fprintfx_conversion
  )
  else if C.output_proof o = CpfEquationalProofTree then (
   Conversion.for_goal >>= EquationalProofTree.plant_and_grow >>= fun t ->
   CertOutput.fprintfx_proof_trees [t]
  )
  else if C.output_proof o = CpfEquationalProofTreeWithLemmas then (
   Conversion.for_goal_with_lemmas >>= 
   CertOutput.fprintfx_subsumption_proof
  ) 
  else return ())
 | _ -> if !yesno then Format.printf "MAYBE\n%!"; return ()
;;

let print_disproof p trs =
 W.get_options >>= fun o ->
 NS.er_contain_closed p >>= fun ns ->
 if C.output_proof o = SZS then (
  szs_completion_output p ns >>= fun (proof, explanation) ->
  Format.printf "DISPROOF:\n";
  Format.printf "%s SZS output start\n" "%";
  Format.printf "%s" proof;
  Format.printf "%s SZS output end\n" "%";
  return ())
 else if C.output_proof o <> NoProof then 
  NS.project_e_closed p >>= fun es ->
  if es = [] then (
   if !yesno then Format.printf "NO\n%!";
   CertOutput.fprintfx_disproof p trs)
  else ( (* ceta does not know ground completeness *)
   if !yesno then Format.printf "MAYBE\n%!"; return ())
 else return ()
;;


let output (status, result) t =
 init_xml (); 
 let problem_name = Filename.basename !flags.filename in
 W.get_options >>= fun o ->
 if not (Completion.certification_output o) then
  St.print_basic status problem_name t; 
 (match Completion.mode o,status,result with
  | C.Completion,C.Sat,Some(C.SlothropResult r) ->
   if_do !flags.output_trs  (St.print_system r []) >>
   let e0 = !flags.axioms in
   if_do !flags.output_statistics (St.print_success e0 [] t)
  | C.Completion,C.Sat,Some(C.MKBttResult p) ->
   NS.project_r_closed p >>= fun r ->
   if Completion.certification_output o then (
    if !yesno then Format.printf "YES\n%!";
    CertOutput.fprintfx_completeness p r)
   else (
    if_do (!flags.output_proof <> NoProof) (print_proof status p r) >>
    NS.project_e_closed p >>= fun e ->
    if_do !flags.output_trs  (St.print_system r e) >>
    let e0 = !flags.axioms in
     if_do !flags.output_statistics (St.print_success e0 p t) >>
    return ())
  | C.Completion, C.Gup _, _
  | C.Completion, C.Unk, _
  | C.Completion, C.Tmo, _ ->
   if_do !yesno (Format.printf "MAYBE\n%!"; return ()) >>
   if_do !flags.output_statistics (St.print_timeout t)
  | C.Proof, C.Sat, Some (C.MKBttResult p) ->
   NS.project_r_closed p >>= fun r ->
   if_do (!flags.output_proof <> NoProof) (
    print_disproof p r) >>
   let e0 = !flags.axioms in
   if_do !flags.output_statistics (St.print_proof status e0 p t)
  | C.Proof, C.Uns, Some (C.MKBttResult p)
  | C.Proof, C.Noc, Some (C.MKBttResult p) ->
   NS.project_r_closed p >>= fun r ->
   if_do (!flags.output_proof <> NoProof) (
    print_proof status p r) >>
   let e0 = !flags.axioms in
   if_do !flags.output_statistics (St.print_proof status e0 p t)
  | C.Proof, C.Suc, Some (C.MKBttResult p) ->
   if (Completion.certification_output o && !yesno) then 
    Format.printf "MAYBE\n%!"; (* ground complete but no proof for ceta *)
   let e0 = !flags.axioms in
   if_do !flags.output_statistics (St.print_proof status e0 p t)
  | C.Proof, C.Unk, _
  | C.Proof, C.Gup _, _
  | C.Proof, C.Tmo, _ -> 
  (if !yesno then Format.printf "MAYBE\n%!";
   if_do !flags.output_statistics (St.print_timeout t))
  | C.CompletenessCheck _, C.Suc,  _ -> 
   log0 "Completeness and joinability of axioms verified."
  | C.CompletenessCheck _, C.Noc, _ ->
   log0 "Completeness and joinability of axioms not verified."
  | _ -> raise (C.GiveUp "Strange result state"))
 >> St.print_logs
;;

let goal_is_empty = function
 | C.Universal s
 | C.Existential s  -> List.is_empty s
;;

let read_wst_file filename so = 
 let chin = Pervasives.open_in filename in
 let lbuf = Lexing.from_channel chin in
 let ts = TrsParser.trs TrsLexer.token lbuf in
 let empty = U.Signature.empty 100 in
 let s = match so with Some s -> s | None -> empty in
 let p, s = TrsSyntax.to_problem_with ts s in
 liftm (RMonad.set s) >>
 if Problem.is_rp p then (
  flags := {!flags with preoriented=true};
  return (Some (Problem.get_strict p), Problem.get_weak p))
 else
  return (None, Problem.get_trs p)
;;

let read_xml_file filename =
 let chin = Pervasives.open_in filename in
 let p, s =
  try Xml.of_channel chin
  with Parsing.Parse_error ->
   failwith (Format.sprintf "File not proper defined: %s" filename)
 in
 liftm (RMonad.set s) >>
 if Problem.is_sp p then
  let trs = Problem.get_trs p in
  return (None, trs)
 else if Problem.is_rp p then (
  flags := {!flags with preoriented=true};
  return (Some (Problem.get_strict p), Problem.get_weak p))
 else
  failwith "Not a standard problem"
;;

let read_file f =
 if Filename.check_suffix f "tptp" then
  liftm (TPTPInput.of_file f) >>= fun (eqs, g, ck) ->
   flags := {!flags with axioms = eqs};
   W.log 2 ("Read TPTP file"^f) >>
   if !flags.ordered_completion && (not (goal_is_empty g)) then
     (match g with 
      C.Existential _ when !flags.certification_output ->
       liftm (RMonad.fail "certification of existential goals not supported")
     | _ ->
      flags := {!flags with goal = g; conjecture = Some (ck); mode = C.Proof};
     return (eqs, None, Some g))
   else
    return (eqs, None, None)
  else if (Filename.check_suffix f "trs") || 
          (Filename.check_suffix f "xml") then
   (if Filename.check_suffix f "trs" then
     read_wst_file f None 
    else
     read_xml_file f) >>= fun (strict, trs) ->
   let eqs = Trsx.to_equations trs in
   flags := {!flags with axioms = eqs};
   return (eqs, strict, None)
  else failwith "Unknown file format"
;;

let run_completion (eqs, strict, goal) =
 W.M.Equation.list_to_stringm eqs >>= fun s ->
 if not (!flags.slothrop) then
   Control.complete eqs strict goal
 else
  SlMain.go eqs >>= fun trs ->
  return (C.Suc, Some (C.SlothropResult trs))
;;

let check_completion (eqs,str,_) trs_filename =
 let eqs = match str with Some trs -> eqs @ (Trsx.to_equations trs) 
  | None -> eqs in
 liftm RMonad.get >>= fun s ->
 let add_option trs = function Some trs' -> Trs.union trs trs' | _ -> trs in
 read_wst_file trs_filename (Some s) >>= fun (str,trs) ->
 let trs = add_option trs str in
 W.M.Trsx.to_stringm trs >>= fun s ->
 W.M.Equation.list_to_stringm eqs >>= fun s ->
 Check.termination trs >>= fun is_terminating ->
 Check.confluence trs >>= fun is_confluent ->
 Check.join_axioms eqs trs >>= fun joins_axioms ->
 if not is_terminating then
  (log1 "System was not shown terminating." >> return (C.Noc, None))
 else if not is_confluent then
  (log1 "System is not confluent." >> return (C.Noc, None))
 else if not joins_axioms then
  (log1 "System does not join input equalities." >> return (C.Noc, None))
 else return (C.Suc, None)
;;

let work () = 
 (read_file !flags.filename >>= fun input ->
  set (W.initial_context_with !flags) >> (* flags changed while reading *)
  match !flags.mode with
   | C.CompletenessCheck cs -> check_completion input cs
   | _ -> run_completion input ) >>= fun r ->
 get >>= fun s' -> liftm RMonad.get >>= fun s -> return (r, s, s')
;;

let work_nonmonadic (s,s') =
 let r = RMonad.run s (run s' (work ())) in
 if Either.is_right r then Either.right r 
 else (C.Gup (Either.left r),None),s,s'
;;

let go () =
 liftm RMonad.get >>= fun s -> get >>= fun s' ->
 let t = !flags.timeout in
 log2 ("Timeout of "^(string_of_float t)) >>
  match 
   try 
    Process.run_timed (Process.Local t) work_nonmonadic (s,s')
   with e -> (
    if !yesno then None, 0.0 else raise e)
  with
   | None,_ -> output (C.Tmo, None) 0.0
   | Some (x,s,s'),r -> liftm (RMonad.set s) >> set s' >> output x (t -. r)
;;

let call_mascott () =
 let cpc = match !flags.cpc with
  | Prime -> "prime" | SReduce -> "sred" | All -> "all" | _ -> "none"
 in
 Mascott.Kernel.execute_with !flags.filename cpc !flags.output_trs
  !flags.kill_processes !flags.no !selection_strategy_string
  !flags.termination_strategy !flags.output_statistics !flags.timeout 
  !flags.s_theory_file !flags.ttimeout !flags.tool !flags.verbosity
  !flags.certification_output !output_proof !yesno
;;

let execute () =
 try
  configurate Sys.argv;
  if !mascott then 
   call_mascott ()
  else (
   let s = U.Signature.empty 200 in
   let s' = W.initial_context_with !flags in
   let r = RMonad.run s (W.Monad.run s' (go ())) in
   if Either.is_right r then Either.right r
   else failwith "No result found.")
 with
  | Help -> Arg.usagex sws usage;
  | e -> if !yesno then Format.printf "MAYBE\n%!" else
         Format.printf "%s\n%!" (Printexc.to_string e) 
;;

execute ()
