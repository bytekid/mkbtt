open Util;;

(*** MODULES *************************************************************)
module Var = Rewriting.Variable;;
module Fun = Rewriting.Function;;
module Term = U.Term;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module W = World;;
module Monad = W.Monad;;

open Monad;;
open SlPrefs
open SlDebug
open SlTerms
open SlTptp
open SlPrint
(*open Lexer
open SlParser*)
open SlRewriting
open SlHuet
open SlOrient
open SlLearning
open SlConjectures

module Tptp = SlTptp

(*** GLOBALS *************************************************************)
let options = ref Completion.default_options;;

(*** FUNCTIONS ***********************************************************)
let version_major = 1;;
let version_minor = 1;;
let version_sub = 0;;

let slothrop_version () = 
  Printf.sprintf "%d.%d.%d" version_major version_minor 
    version_sub
;;

let equation_of_clause allow_neg = function
    InputClause(n,st,Equal(s,t)::[]) -> true,(s,t)
  | InputClause(n,st,DisEqual(s,t)::[]) when allow_neg -> false,(s,t)
  | x -> failwith "Positive unit clauses only"
;;

let separate clauses = 
  let rec sep_h ax hp cj = function
      ((InputClause(s,Axiom,tl)) as a)::cs -> 
        sep_h (a::ax) hp cj cs
    | ((InputClause(s,Hypothesis,tl)) as h)::cs -> 
        sep_h ax (h::hp) cj cs
    | ((InputClause(s,Conjecture,tl)) as c)::cs -> 
        sep_h ax hp (c::cj) cs
    | [] -> (ax,hp,cj)
  in
    sep_h [] [] [] clauses
;;

let rec of_term t =
 match t with
(* index component of variable simplifies renaming, following BN98.
   all variables have initially index 0 *)
  | Term.Var x -> SlTerms.V(Var.to_string x, 0)
  | Term.Fun(f, args) -> 
   let fs = Fun.to_string f in
   let args' = List.map of_term args in
   SlTerms.T(fs, args')
;;

let clause_of_equation eq =
 let l, r = Equation.terms eq in
 let l', r' = of_term l, of_term r in
 Tptp.InputClause ("bla", Tptp.Axiom, [Tptp.Equal(l', r')])
;;

let tptp_of_eqs eqs =
 List.foldl (fun res r -> (clause_of_equation r) :: res) [] eqs
;; 
 
let true_term = T("_true",[]);;
let false_term = T("_false",[]);;
let eq_term a b = T("_eq",[a;b]);;
    
let refutation_axioms (s,t) = 
  let var = V("X",0) in
  let s_hat, t_hat = skolemize s, skolemize t in
  let ax1 = (eq_term var var),true_term in
  let ax2 = (eq_term s_hat t_hat),false_term in
    [ax1;ax2]
;;

let refutation_conjecture = 
  true, (true_term,false_term)
;;

let set_options () =
 let v = 
  if (Completion.verbosity !options) == 0 then "quiet"
  else if (Completion.verbosity !options) == 1 then "info"
  else "debug"
 in set_verbosity v;
 completion_mode := true;
;;

let go eqs =
 W.get_options >>= fun o -> 
 options := o; 
 SlChecker.options := o;
 set_options ();
 let axioms = tptp_of_eqs eqs in
(*
  let in_ch = open_in filename in
  let lexbuf = Lexing.from_channel in_ch in
  let clauses = Parser.start Lexer.token lexbuf in
  let axioms,hypotheses,conjectures = separate clauses in
  let axioms = identities in *)
  let ax_eq = snd (List.split 
                     (List.map (equation_of_clause false) axioms)) in
(*  let hyp_eq = snd (List.split 
                      (List.map (equation_of_clause false) hypotheses)) in
  let _conj_eq = List.map (equation_of_clause true) conjectures in
  let ax_eq,conj_eq = 
    if !refutation_mode then
      match _conj_eq with
          (false,(s,t))::[] ->
            (refutation_axioms(s,t)) @ _ax_eq,
            [refutation_conjecture]
        | _ ->
            failwith "Only one negated conjecture allowed in refutation mode."
    else
      _ax_eq, _conj_eq
  in *)
  let hyp_eq, conj_eq = [], [] in (* fake *)
  let identities = ax_eq@hyp_eq in 
    debug 
      (lazy 
         ("Input clauses: \n" ^ 
            (List.fold_left 
               (fun acc c -> acc ^ (if acc <> "" then "\n" else "") ^ 
                  (string_of_clause c)) "" axioms)));
    debug_if_l DEBUG (ax_eq <> []) 
      (lazy 
         ("Axioms: \n" ^ 
            (List.fold_left 
               (fun acc c -> acc ^ (if acc <> "" then "\n" else "") ^ 
                  (string_of_equation c)) "" ax_eq)));
    debug_if_l DEBUG (hyp_eq <> [])
      (lazy 
         ("Hypotheses: \n" ^ 
            (List.fold_left 
               (fun acc c -> acc ^ (if acc <> "" then "\n" else "") ^ 
                  (string_of_equation c)) "" hyp_eq)));
    debug_if_l DEBUG (conj_eq <> [])
      (lazy 
         ("Conjectures: \n" ^ 
            (List.fold_left 
               (fun acc (b,c) -> 
                  acc ^ (if acc <> "" then "\n" else "") ^ 
                    ((if b then string_of_equation 
                      else string_of_disequation) c)) "" conj_eq)));
    if !trust_axiom_orientation then 
      List.iter add_axiom ax_eq;
    let trs = 
    begin
      try
        let dp,unsolved = grind identities conj_eq 
          !find_all_completions 
        in 
          if unsolved != [] then
            (info "Deciding remaining conjectures...";
             List.iter (decide_conjecture dp) unsolved;
             Trs.empty)
          else
           SlChecker.get_trs dp
      with
          CONJECTURES_SOLVED -> Trs.empty
    end
    in
    (*alert "All conjectures solved.";*)
    return trs
;;

(*
let opt_specs =
  [ "--verbosity", 
    Arg.String set_verbosity, 
    "Set verbosity: quiet, info (default), debug, trace";
    
    "--quiet",
    Arg.Unit (fun () -> verbosity := QUIET),
    "Set verbosity to quiet";

    "--info",
    Arg.Unit (fun () -> verbosity := INFO),
    "Set verbosity to info";

    "--debug",
    Arg.Unit (fun () -> verbosity := DEBUG),
    "Set debug level to debug";

    "--trace",
    Arg.Unit (fun () -> verbosity := TRACE),
    "Set debug level to trace";

    "--debug-checker", 
    Arg.Unit (fun () -> debug_checker := true; 
                verbosity := TRACE),
    "Debug communication with the termination checker";

    "--debug-learning", 
    Arg.Unit (fun () -> debug_learning := true; 
                verbosity := TRACE),
    "Debug learning";

    "--debug-rewriting", 
    Arg.Unit (fun () -> debug_rewriting := true; 
                verbosity := TRACE),
    "Show normalization steps in detail";

    "--timeout", 
    Arg.Set_int timeout, 
    "Timeout in seconds";

    "-t", 
    Arg.Set_int timeout, 
    "Timeout in seconds";

(*     "--refute",  *)
(*     Arg.Set refutation_mode,  *)
(*     "Refute conjecture"; *)

    "--complete", 
    Arg.Set completion_mode, 
    "Search for a convergent completion";

    "--find-all", 
    Arg.Set find_all_completions, 
    "Find all completions";

    "--trust-input",
    Arg.Set trust_axiom_orientation,
    "Always orient axioms as given in the input";

    "--aprove",
    Arg.Unit Checker.use_aprove,
    "Use the AProVE termination checker (default)";

    "--tpa",
    Arg.Unit Checker.use_tpa,
    "Use the TPA termination checker";

    "--cmd",
    Arg.Set_string Checker.checker_cmd,
    "Command used to run AProVE";

    "--detect-divergence",
    Arg.Set detect_divergence,
    "Enable experimental divergence detection heuristic";

    "--version",
    Arg.Unit (fun () -> info ("Slothrop " ^ (slothrop_version()))),
    "Print the version number";

  ];;


let main() = 
  try
    Arg.parse opt_specs go 
      ("Usage: " ^ Sys.argv.(0) ^ " <file1.tptp> <file2.tptp> ...");
    exit 0
  with 
      Arg.Bad msg ->
	prerr_string (Sys.argv.(0) ^ ": " ^ msg ^ "\n");
	exit 1
;;

main();;
*)

