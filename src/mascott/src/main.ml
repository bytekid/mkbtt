(** Main file
@author Sarah Winkler
@since  2010/12/30 *)

(*** OPENS **************************************************************)
open Util;;

(*** SUBMODULES *********************************************************)
module E = Either;;
module C = Completion;;
module W = World;;
module Label = U.Label;;
module Term = U.Term;;
module RMonad = U.Monad;;
module WMonad = World.Monad;;
module TrsParser = U.MyTrsParser;;
module TrsLexer = U.MyTrsLexer;;
module TrsSyntax = U.MyTrsSyntax;;
module Problem = U.MyProblem;;
module St = Statistics;;
module NS = NodeState;;
module Trs = U.Trs;;
(*** OPENS (2) **********************************************************)
open WMonad;;
open Completion;;

(*** EXCEPTIONS *********************************************************)
exception Help;;

(*** GLOBALS ************************************************************)
let flags = ref C.default_options;;

let yesno = ref false

let set_timeout f = flags := {!flags with timeout = f};;
let set_ttimeout f = flags := {!flags with ttimeout = f};;
let set_kill_processes r = flags := {!flags with kill_processes = r};;
let set_output_trs () = flags := {!flags with output_trs = true};;
let set_output_stats () = flags := {!flags with output_statistics = true};;
let set_theory_file f = flags := {!flags with s_theory_file = f};;
let set_verbosity v = flags := {!flags with verbosity = v};;

let set_cpc s = 
 let cpc = 
  if (String.compare s "none") == 0 then C.NoCPC
  else if (String.compare s "connected") == 0 then C.Connected
  else if (String.compare s "all") == 0 then C.Mixed
  else if (String.compare s "sred") == 0 then C.SReducibility
  else C.Prime
 in flags := {!flags with cpc = cpc}
;;

let set_internal s =
 flags := {!flags with check_externally = false; termination_strategy=s};;
let set_ttool s = 
 flags := {!flags with tool=s; check_externally = true};;

let set_selection s =
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

let set_output_proof p =
 if String.compare p "szs" = 0 then
  flags := {!flags with output_proof = SZS}
 else if String.compare p "cpfconv" = 0 then
(*  flags := {!flags with output_proof = CpfConversion}*)
  failwith "conversion proofs with AC symbols not supported"
 else if String.compare p "cpftree" = 0 then
(*  flags := {!flags with output_proof = CpfEquationalProofTree}*)
  failwith "tree proofs with AC symbols not supported"
 else if String.compare p "cpfsub" = 0 then (
  flags := {!flags with output_proof = CpfEquationalProofTreeWithLemmas};
  yesno := true)
 else flags := {!flags with output_proof = NoProof}
;;


(*                         DEFINE SWITCHES                              *)
let sws = [
 ("-cp", Arg.String set_cpc,
  "critical pair criterion: sred, prime, connected, all or none");
 ("-ct", Arg.Unit set_output_trs,
  "output completed TRS");
 ("-kp", Arg.Float set_kill_processes,
  "threshold to kill processes, > 1.0");
 ("-n", Arg.Unit (fun _ -> ()),
  "dont do anything");
 ("-s", Arg.String set_internal,
  "check termination internally with given strategy");
 ("-st", Arg.Unit set_output_stats,
  "output statistics");
 ("-ss", Arg.String set_selection,
  "selection strategy");
 ("-t", Arg.Float set_timeout,
  "global time limit");
 ("-T", Arg.Float set_ttimeout,
  "termination call time limit");
 ("-th", Arg.String set_theory_file,
  "theory S with respect to which completion performed");
 ("-tp", Arg.String set_ttool,
  "executable to call external AC-termination prover");
 ("-v", Arg.Int set_verbosity,
  "verbosity in {0, 1, 2}");
 ("-yesno", Arg.Set yesno,
  "output only YES, NO, or MAYBE");
]

let sws = Arg.alignx 80 sws;;

let usage = "Usage: ./mkbtt [options] <file> [timeout]\n\nOptions:";;

(*** FUNCTIONS ***********************************************************)
let log0 = W.log 0;;
let log1 = W.log 1;;
let log2 = W.log 2;;

let configurate argv =
 if Array.length argv = 1 then (Arg.usagex sws usage; exit 0);
 let file_is_set = ref true in
 let push_file n = () (*
  if !file_is_set then
   (file_is_set := false;
   flags := {!flags with filename = n})
  else
   ((*Arg.usagex sws usage; exit 0*) Format.printf "file not set\n%!")*)
 in
 Arg.parse sws push_file usage;
;;

(*                             READ INPUT                               *)
let read_wst_file filename so =
 let chin = Pervasives.open_in filename in
 let lbuf = Lexing.from_channel chin in
 let ts = TrsParser.trs TrsLexer.token lbuf in
 liftm RMonad.get >>= fun s ->
 let p, s = TrsSyntax.to_problem_with ts s in
 liftm (RMonad.set s) >>
 let trs = Problem.get_trs p in
(* liftm (Trs.to_stringm trs) >>= fun s ->
 Format.printf "read%s\n%!" s;*)
 return trs
;;

let read_file filename =
 if Filename.check_suffix filename "tptp" then
  (liftm (TPTPInput.of_file filename) >>= fun ((eqs, g), th) ->
  (*Format.printf "%i Equations in %s:\n%!" (List.length eqs) filename ;
  iter (fun e -> (liftm (Equation.to_stringm e)) >>= fun s -> Format.printf "%s\n%!" s; return ()) eqs >>= fun _ -> 
  (match g with None -> return "none" | Some e -> liftm (Equation.to_stringm e)) >>= fun es ->
  Format.printf "Goal: %s\n%!" es;*)
  flags := {!flags with goal = g; axioms = eqs; theory = th};
  W.set_goal g >>
  W.set_axioms eqs >> 
  return eqs)
 else
  read_wst_file filename None >>= fun trs ->
  let eqs = Trsx.to_equations trs in
  W.set_axioms eqs >> return eqs
;;

(*                               OUTPUT                                 *)
let if_do b m = if b then m else return ()

let completion_output p ns =
 History.joint_history [] ns >>= fun h ->
 W.get_options >>= fun o ->
 History.as_string p (C.filename o) h >>= fun s ->
 return (s, "Ground complete system derived.")
;;


let print_proof s p =
 W.get_options >>= fun o ->
 if C.certification_output o && C.has_goal o then (
 let g = Option.the (C.goal o) in
 NS.project_r_closed p >>=
 W.M.ACRewrite.joinable (Equation.terms g) >>= fun yes ->
 if yes then (
  if !yesno then Format.printf "YES\n%!";
  if (*C.output_proof o = CpfConversion then
   Conversion.for_goal p >>= CertOutput.fprintfx_conversion
  else if C.output_proof o = CpfEquationalProofTree then
  Conversion.for_goal p >>= EquationalProofTree.plant_and_grow >>= fun t ->
  CertOutput.fprintfx_proof_trees [t]
  else if*) C.output_proof o = CpfEquationalProofTreeWithLemmas then 
   (Conversion.for_goal_with_lemmas p >>=
    CertOutput.fprintfx_subsumption_proof)
  else return ())
 else (Format.printf "MAYBE\n%!"; return ())
 )
 else
  let prooftype = if s=C.Uns then "Refutation" else "Proof" in
  let rec l i = if i<=0 then "" else "-"^(l (i-1)) in
  let line () = Format.printf "%s%s%s\n" "%" (l 76) "%" in
  W.get_options >>= fun o ->
  (NS.er_contain_closed p >>= fun ns ->
  completion_output p ns) >>= fun (proof, explanation) ->
  line ();
  Format.printf "oMKBtt result: %s\n" explanation;
  line ();
  let f = C.filename o in
  Format.printf "%s SZS output start %s for %s\n" "%" prooftype f;
  Format.printf "%s" proof;
  Format.printf "%s SZS output end\n" "%";
  line ();
  return ()
;;

let output (status, result) t =
 if (!flags).no then return () else
 let problem_name = Filename.basename !flags.filename in
 if (not (!yesno ||  (!flags.certification_output))) then 
  St.print_basic status problem_name t;
 begin
 match Completion.mode (!flags),status,result with
  | C.Completion,C.Sat,Some p ->
   if (!flags.output_proof <> NoProof) then 
    print_proof status p
   else
    NS.project_r_closed(*_unprotected *) p >>= W.M.Trsx.readable >>= fun r ->
    if_do !flags.output_trs  (St.print_system r) >>
    let e0 = !flags.axioms in
    if_do !flags.output_statistics (St.print_success e0 p t)
  | C.Completion, C.Gup _, _
  | C.Completion, C.Unk, _
  | C.Completion, C.Tmo, _ ->
   if_do !flags.output_statistics (St.print_timeout t)
  | C.CompletenessCheck _, C.Suc,  _ ->
   log0 "Completeness and joinability of axioms verified."
  | C.CompletenessCheck _, C.Noc, _ ->
   log0 "Completeness and joinability of axioms not verified."
  | _ -> raise (C.GiveUp "Strange result state")
 end >> 
 (if (!yesno || !flags.certification_output) then return () 
  else St.print_logs)
;;

let set_theory eqs sg =
 if String.length (!flags.s_theory_file) <> 0 then (
  liftm RMonad.get >>= fun sg ->
  read_wst_file !flags.s_theory_file (Some sg) >>= fun s ->
  flags := {!flags with s_theory = s};
  return ())
 else (
  liftm (Theories.Find.largest eqs) >>= fun (_,s,n) ->
(*  Format.printf "Recognized %s\n%!" n;*)
  flags := {!flags with s_theory = s; s_theory_file = n};
  return ())
;;

let work () =
 read_file !flags.filename >>= fun eqs ->
 liftm RMonad.get >>= fun sg ->
 set_theory eqs sg >>= fun _ ->
 W.Monad.set (W.initial_context_with !flags) >>
 let tstart = Unix.gettimeofday () in
 Control.complete eqs >>= fun result ->
 let d = (Unix.gettimeofday () -. tstart) in 
 output result d >> return ""
;;

let run_monad () =
 let s = Theories.sigma in
 let w = W.initial_context_with !flags in
 let r = RMonad.run s (W.Monad.run w (work ())) in
 if E.is_right r then E.right r else failwith "No result from Monad.run"
;;

let output no s = 
 if no then () else match s with
 | None,_ -> Format.printf "Timeout\n"
 | Some s,t -> if not (!yesno || !flags.certification_output) then 
  Format.printf "Total time: %f\n%s" (!flags.timeout-.t) s
;; 

let execute () =
 try
  configurate Sys.argv;
  let t = !flags.timeout in
  output (!flags.no || !flags.certification_output) 
   (Process.run_timed (Process.Local t) run_monad ())
 with | Help -> Arg.usagex sws usage;
;;

let execute_with file cp ct kp no ss s st t th tt tp v co pr yn =
  set_timeout t;
  set_ttimeout tt;
  set_kill_processes kp;
  if ct then set_output_trs ();
  if st then set_output_stats ();
  set_verbosity v;
  set_cpc cp;
  flags := {!flags with filename = file; no = no; certification_output = co};
  set_output_proof pr;
  set_theory_file th;
  if tp <> "" then set_ttool tp ;
  if s <> "" then set_internal s;
  if ss <> "" then set_selection ss;
  yesno := yn;
  let t = !flags.timeout in
  output no (Process.run_timed (Process.Local t) run_monad ())
;; 
