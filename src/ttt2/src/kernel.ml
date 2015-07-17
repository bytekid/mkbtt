(* Copyright 2008 Martin Korp, Christian Sternagel, Harald Zankl
 * GNU Lesser General Public License
 *
 * This file is part of TTT2.
 * 
 * TTT2 is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * TTT2 is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with TTT2. If not, see <http://www.gnu.org/licenses/>.
 *)

(*** OPEN *********************************************************************)
open Util;;
open Processors;;

(*** MODULES ******************************************************************)
module C = Complexity;;
module F = Format;;
module M = Rewritingx.Monad;;
module P = Problem;;
module Trs = Rewritingx.Trs;;

(*** TYPES ********************************************************************)
type termination = ST | RT;;
type strategy = FS | IS | OS;;
type language = AL | CL;;
type category = termination option * strategy option * language option;;
type complexity = DC | RC | PC;;

type t = {
 answer : bool ref;
 category : category ref;
 complexity : complexity option ref;
 conf : string ref;
 file : string ref;
 flags : bool ref;
 optimize : bool ref;
 proof : bool ref;
 stderr : bool ref;
 stdin : bool ref;
 strategy : string ref;
 time : bool ref;
 timeout : float option ref;
 xml : bool ref;
 xslt : string ref;
};;

(*** EXCEPTIONS ***************************************************************)
exception Help;;
exception Processors;;
exception Locate of string;;
exception Version;;

(*** GLOBALS ******************************************************************)
let version = "1.06";;
let usage = "Usage: ./ttt2 [options] <file> [timeout]\n\nOptions:";;

let default =
 "if standard then \
  (var | con | dp;tdg?;sccs?;sc*;edg?;( \
   sccs | sc | {ur?;matrix -dp -dim 2 -ib 2 -ob 2 -ur[2]}restore)*)[10] \
  else if relative then \
   trivial?;((unfold || lpo -quasi || matrix -dim 2 -ib 2 -ob 3 || matrix -dim 3 -ib 2 -ob 2)[2])* \
  else fail"
;;

let flags = {
 answer = ref true;
 category = ref (None,Some FS,None);
 complexity = ref None;
 conf = ref "";
 file = ref "";
 flags = ref false;
 optimize = ref false;
 proof = ref true;
 stderr = ref false;
 stdin = ref false;
 strategy = ref default;
 time = ref false;
 timeout = ref None;
 xml = ref false;
 xslt = ref "";
};;

let help =
 let error = F.sprintf "./ttt2: unknown %s option `%s'." in
 let set_category s =
  let set_category = function
   | "ST" -> flags.category := Triple.replace_fst (Some ST) !(flags.category)
   | "RT" -> flags.category := Triple.replace_fst (Some RT) !(flags.category)
   | "FS" -> flags.category := Triple.replace_snd (Some FS) !(flags.category)
   | "IS" -> flags.category := Triple.replace_snd (Some IS) !(flags.category)
   | "OS" -> flags.category := Triple.replace_snd (Some OS) !(flags.category)
   | "AL" -> flags.category := Triple.replace_thd (Some AL) !(flags.category)
   | "CL" -> flags.category := Triple.replace_thd (Some CL) !(flags.category)
   | s -> raise (Arg.Bad (error "category" s))
  in
  flags.category := (None,None,None);
  List.iter set_category (String.split ~d:"," s)
 in
 let set_complexity = function
  | "DC" -> flags.complexity := Some DC
  | "RC" -> flags.complexity := Some RC
  | "PC" -> flags.complexity := Some PC
  | s -> raise (Arg.Bad (error "complexity" s))
 in
 let help = [
  ("--answer",Arg.Clear flags.answer,
   "Does not print an answer. Normally, either `YES', `NO', or `MAYBE' \
    is printed. Here `YES' means that the given termination problem is \
    terminating, `NO' means that it is nonterminating and `MAYBE' means \
    that neither termination nor nontermination could be proved.");
  ("-a",Arg.Clear flags.answer,"See `--answer'.");
  ("--category",Arg.String set_category,
   "Defines the termination category. Each category can be specified by \
    definining the problem, the strategy as well as the language (startterms), \
    separated by a comma. Possible values for the problem are 
    `ST' (standard termination) and `FT' (full termination), values for \
    the strategy are \
    `FS' (full strategy), `IS' (innermost strategy), and `OS' (outermost \
    strategy). Finally possible languages are `AL' (all terms) and `CL' \
    (constructor-based terms). Note that you do not have to specify both \
    characteristics. Furthermore, per default the flags \
    `FS' and `AL' are specified.");
  ("-C",Arg.String set_category,"See `--category'.");
  ("--conf",Arg.Set_string flags.conf,
   "Specifies the configuration file. Detailed information about the \
    syntax of configuration files can be obtained by executing the command \
    `./ttt2 --help' or `./ttt2 -h'.");
  ("-c",Arg.Set_string flags.conf,"See `--conf'.");
  ("--complexity",Arg.String set_complexity,
   "Enables the complexity mode. TTT2 can compute either the derivational \
    complexity or runtime complexity of a given problem. To compute the \
    derivational complexity of the given problem just specify `RC'. To \
    compute the runtime complexity of the given problem one has to add \
    the flag `RC'. If TTT2 should compute the complexity according to the \
    information contained in the problem file just add the flag `PC'. \
    Note that not all processors yield feasible complexity bounds. So to \
    guarantee a successful complexity proof also an appropriate strategy \
    has to be chosen.");
  ("-cp",Arg.String set_complexity,"See `--complexity'.");
  ("-e",Arg.Set flags.stderr,
   "Print the answer on stderr instead of stdout.");
  ("--flags",Arg.Set flags.flags,
   "Prints the flags of the processors if the command `./ttt2 --processors' \
    or `./ttt2 -P' is executed.");
  ("-f",Arg.Set flags.flags,"See `--flags'.");
  ("--help",Arg.Unit (fun () -> raise Help),"Display this list of options.");
  ("-help",Arg.Unit (fun () -> raise Help),"See `--help'");
  ("-h",Arg.Unit (fun () -> raise Help),"See `--help'");
  ("--locate",Arg.String (fun s -> raise (Locate s)),
   "Prints all processors that matches the given regular expression. To \
    construct regular expressions the following operators can be used: \
    `.', `?', `*', `+', '[...]', and '[^...]'. Note that this operators \
    behave similarly as their pendants used for example in Unix based \
    systems."); 
  ("-l",Arg.String (fun s -> raise (Locate s)),"See `--locate'.");
  ("--optimize",Arg.Set flags.optimize,
   "Ensures that a complexity proof is optimized after its completion. \
    The strategies used to optimize a complexity proof have to be specified \
    in the given configuration file (see `--conf'). To gurantee a correct \
    application of the specified strategies, each strategy, that should be \
    used to optimize parts of the given complexity proof, has to be named \
    according to the following guidlines: O(1) indicates that the used \
    strategy proves constant complexity bounds and a name of the form O(n^m) \
    has to be used if the given strategy returns complexity bounds up to \
    degree m. Note that this flag is only taken into consideration if the \
    flag `--cp' is activated.");
  ("-o",Arg.Set flags.optimize,"See `--optimize'.");
  ("--processors",Arg.Unit (fun () -> raise Processors),
   "Prints all available processors.");
  ("-P",Arg.Unit (fun () -> raise Processors), "See `--processors'.");
  ("--proof",Arg.Clear flags.proof,
   "Does not print the generated proof. Per default the proof is printed.");
  ("-p",Arg.Clear flags.proof,"See `--proof'.");
  ("--strategy",Arg.Set_string flags.strategy,F.sprintf
   "Defines the strategy that should be used to prove termination or \
    nontermination. Detailed information about strategies can be obtained \
    by executing the command `./ttt2 --help' or `./ttt2 -h'. Per default the \
    strategy `%s' is taken." default); 
  ("-s",Arg.Set_string flags.strategy,"See `--strategy'.");
  ("--stderr",Arg.Set flags.stderr,"Prints the answer on STDERR.");
  ("--stdin",Arg.Set flags.stdin,"Reads the input from STDIN.");
  ("-",Arg.Set flags.stdin,"See `--stdin'.");
  ("--time",Arg.Set flags.time,
   "Prints the wall clock time needed by TTT2 to execute the given strategy.");
  ("-t",Arg.Set flags.time,"See `--time'.");
  ("--version",Arg.Unit (fun () -> raise Version),
   "Prints the version of TTT2.");
  ("-v",Arg.Unit (fun () -> raise Version),"See `--version'.");
  ("--xml",Arg.Set flags.xml,
   "Prints an XML proof. The currently used XML format is TTT2 internal
    and has to be transformed accordingly before certification.");
  ("-x",Arg.Set flags.xml,"See `--xml'.");
  ("--xslt",Arg.Set_string flags.xslt,
   "Postprocess the output using a comma-separated list of XSL files.");
 ] in
 Arg.alignx 80 help
;;

let strategy _ =
 let grammar =
  F.sprintf 
   "@[<2>A strategy is defined by the grammar@\n\
    @[<2>@\n\
    s ::= m | (s) | c | i | e@\n\
    c ::= s;s | s|s | s||s | if p then s else s@\n\
    i ::= s? | s* | s+ | sn* | s[f]*@\n\
    e ::= s%% | s! | s[f] | {s}o\
    @]@\n@\n%s@]"
   (String.align ~n:2 80
    "where `s' expresses the possible strategies of TTT2, `m' denotes the \
     name of any available processor, `p' denotes the name of any available \
     predicate, and `c', `i' and `e' defines the available combinators, \
     iterators and specifiers. Here combinators are used to combine two \
     strategies whereas iterators are used to repeat a given strategy a \
     designated number of times. In contrast, specifiers are used to control \
     the behavior of strategies. A strategy works on a termination problem. \
     Whenever TTT2 executes a strategy, internally, a so called proof object \
     is constructed which represents the actual termination proof. Depending \
     on the shape of the resulting proof object after applying a strategy \
     `s', we say that `s' succeeded or `s' failed. This should not be \
     confused with the possible answers of the prover: `YES', `NO', and \
     `MAYBE'. Here `YES' means that termination could be proved, `NO' \
     indicates a successful non-termination proof, and MAYBE refers to the \
     case when termination could neither be proved nor disproved. On success \
     of a strategy `s' it depends on the internal proof object whether the \
     final answer is `YES' or `NO'. On failure, the answer always is `MAYBE'. \
     Based on the two possibilities success or failure, the semantics of the \
     strategy operators is as follows.")
 in
 let combinators =
  F.sprintf "@[<4>Combinators:@\n%s@]"
   (String.itemize ~n:4 80
    ["s;s': First applies `s' to the given problem. If this fails, then \
     `s;s'' fails. Otherwise `s'' is applied to the resulting problems.";
     "s|s': Applies `s' to the given problem. If this succeeds, its \
     result is returned. Otherwise `s'' is applied to the given problem.";
     "s||s': Runs `s' and `s'' in parallel on the given problem. As soon \
     as at least one of `s' and `s'' succeeds, the resulting problem is \
     returned.";
     "if p then s else s': Applies `s' to the given problem if `p' is \
     satisfied by the underlying problem. Otherwise `s'' is applied."])
 in
 let iterators =
  F.sprintf "@[<4>Iterators:@\n%s@]"
   (String.itemize ~n:4 80
    ["s?: Applies `s' to the given problem. On success its result is \
     returned. Otherwise the original problem is returned unmodified.";
     "s*: Applies `s' recursively to the given problem until it cannot be \
     modified any more. Note that `s*' always is successful.";
     "s+: Applies `s' recursively to the given problem until it cannot be \
     modified any more. I.e., `s+' is successful if it can prove or \
     disprove termination of the given problem. Otherwise it fails. Note \
     that `s+ = s*;s' but `s+' is not equivalent to `s;s*'.";
     "sn*: Applies `s' recursively to the given problem until it cannot be \
     modified any more or `s' has been applied `n'-times. Note that `sn*' \
     always is successful.";
     "s[f]*: Applies `s' recursively to the given problem until it \
     cannot be modified any more or `f' seconds are elapsed. Note that \
     `s[f]*' always is successful."])
 in
 let specifiers =
  F.sprintf "@[<4>Specifiers:@\n%s@]"
   (String.itemize ~n:4 80
    ["s%: Applies `s' to the given problem. If `s' fails, the computation \
     is aborted and `s%' fails. Otherwise it succeeds.";
     "s!: Applies `s' to the given problem. If `s' proves or disproves \
     termination of the given problem, `s!' is successful. Otherwise it \
     fails.";
     "s[f]: Tries to modify a given problem via `s' for at most `f' \
     seconds. If `s' does not succeed or fail within `f' seconds, `s[f]' \
     fails. Otherwise `s[f]' returns the resulting problem. Hence it \
     succeeds (fails) if `s' succeeds (fails).";
     "{s}o: Applies `s' to the given problem. If `s' fails, `{s}o' fails. \
     Otherwise the modifier `o' is applied to the resulting problems."])
 in
 let miscellaneous =
  String.align ~n:2 80
   "Currently, TTT2 supports various processors, modifiers and predicates \
    to prove or disprove termination. Please see the complete list of \
    processors by executing the command `./ttt2 --processors' or \
    `./ttt2 -P'. Many processors can be configured in more detail by \
    adding flags. The available flags of the processors can be inferred by \
    adding `--flags' or `-f' to the previous commands. In addition you can \
    also add the flag `--help' or `-h' to any processor within a strategy to \
    obtain flag information on the fly (during a run of TTT2). Note that \
    in such a case the execution is immediately aborted when the \
    corresponding processor is reached. Last but not least you can also \
    search for processor by stating a regular expression. In that case use \
    the command `./ttt2 --locate [regexp]' or `./ttt2 -l [regexp]'."
 in
 F.printf "@\n@[<2>Strategy: %s@\n@\n%s@\n@\n%s@\n@\n%s@\n@\n%s@]@\n"
  grammar combinators iterators specifiers miscellaneous
;;

let config _ =
 F.printf "@\n@[Configuration File: %s@]@\n"
  (String.alignx ~n:2 60 80
   "Since strategies can get quite complex, TTT2 provides the opportunity \
    to specify a configuration file. This allows to abbreviate and connect \
    different strategies. Abbreviations are defined according to the \
    following syntax: `n = s' where `n' defines the name and `s' the \
    strategy of the abbreviation. By convention strategy abbreviations are \
    written in capital letters. To differ between different abbreviations, \
    each abbreviation has to be put in a separate line. Sometimes it might \
    be convenient to define a strategy over several lines. In that case \
    you have to add a `\' to the end of each line. Last but not least you \
    can add comments to a config file by putting a `#' in front of each line.")
;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

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

let open_in file =
 try open_in file
 with Sys_error _ -> failwith (F.sprintf "No such file: `%s'" file)
;;

let parse_config s =
 try
  let lexbuf = Lexing.from_channel (open_in !(flags.conf)) in
  ConfSyntax.expand s (ConfParser.conf ConfLexer.token lexbuf)
 with Parsing.Parse_error ->
  failwith (F.sprintf "File not proper defined: %s" !(flags.conf))
;;

let configurate argv =
 (* init printing *)
 F.pp_set_margin F.std_formatter 100; init_xml ();
 (* parse arguments *)
 if Array.length argv = 1 then (Arg.usagex help usage; exit 0);
 let file = ref true and timeout = ref true in
 let push s =
  if !file then (file := false; flags.file := s)
  else if !timeout then
   try flags.timeout := Some (float_of_string s); timeout := false;
   with Failure _ -> ()
  else (Arg.usagex help usage; exit 0)
 in
 Arg.parse help push usage;
 if !(flags.conf) = "" then ()
 else flags.strategy := parse_config !(flags.strategy)
;;

let parse_problem ?(stdin = false) file =
 let chin = if stdin then Pervasives.stdin else open_in file in
 (* parse problem file *)
 let (p,s) = try
  if Filename.check_suffix file "xml" then Xml.of_channel chin
  else if Filename.check_suffix file "trs" then WstParser.Trs.of_channel chin
  else WstParser.Srs.of_channel chin
 with Parsing.Parse_error ->
  failwith (F.sprintf "File not proper defined: %s" file)
 in
 (* check category and adapt complexity *)
 let map_category = function ST -> P.is_sp | RT -> P.is_rp in
 let map_strategy = function FS -> P.is_ft | IS -> P.is_it | OS -> P.is_ot in
 let map_language = function AL -> P.is_al | CL -> P.is_cl in
 let find map = Option.fold map (const true) in
 let is_category = find map_category (Triple.fst !(flags.category)) in
 let is_strategy = find map_strategy (Triple.snd !(flags.category)) in
 let is_language = find map_language (Triple.thd !(flags.category)) in
 if not (is_category p) then Format.printf "cat";
 if not (is_strategy p) then Format.printf "str";
 if not (is_language p) then Format.printf "lan";
 if is_category p && is_strategy p && is_language p then
  let language = function Some RC -> P.Constructor | _ -> P.All in
  (P.set_language (language !(flags.complexity)) p,s)
 else failwith "Category mismatch: wrong problem, strategy or language"
;;

let parse_strategy s t =
 try
  let s = Option.fold (F.sprintf "(%s)[%f]" s) s t in
  Parser.strategy Lexer.token (Lexing.from_string s)
 with Parsing.Parse_error ->
  failwith (F.sprintf "Strategy not proper defined: %s" s)
;;

let fprintf_proof fmt problem (p,s,status) t =
 ignore (M.run s
  (if !(flags.xml) then (
   (* print header *)
   F.fprintf fmt
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
     @{<certificationProblem \
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">";
   (* print goal *)
   F.fprintf fmt "@{<goal>";
   if Status.is_nonterminating status then F.fprintf fmt "<nonTermination/>@}"
   else F.fprintf fmt "<termination/>@}";
   (* print input and proof *)
   F.fprintf fmt "@{<input>@{<trsInput>"; P.fprintfx fmt problem >>= fun _ ->
   F.fprintf fmt "@}@}"; Proof.fprintfx status fmt p >>= fun _ ->
   M.return (F.fprintf fmt "@}"))
  else (
   if !(flags.answer) || !(flags.time) then Format.fprintf fmt "@\n";
   F.fprintf fmt "@[<1>Problem:@\n"; P.fprintfm fmt problem >>= fun _ ->
   F.fprintf fmt "@]@\n@\n@[<1>Proof:@\n"; Proof.fprintf fmt p >>= fun _ ->
   M.return (F.fprintf fmt "@]@\n"))))
;;

let fprintf fmt p proof t =
 (* print answer *)
 if !(flags.answer) then (
  let fmt = if !(flags.stderr) then F.err_formatter else fmt in
  let answer = Answer.of_status (Triple.thd proof) in
  F.fprintf fmt "%a" Answer.fprintf answer;
  if (Answer.is_yes answer) then (
   let to_string f = Option.fold C.to_string "?" (f (Triple.fst proof)) in
   let fprintfc c = F.fprintf fmt "(?,%s)" (to_string c) in
   if Option.is_some !(flags.complexity) then fprintfc Proof.complexity);
  F.fprintf fmt "@\n");
 (* print time *)
 if !(flags.time) then F.printf "Time: %.3f@\n" t;
 (* print proof *)
 if !(flags.proof) then
  if !(flags.xml) then (
   fprintf_proof F.str_formatter p proof t;
   let output = F.flush_str_formatter () in
   if !(flags.xslt) = "" then F.fprintf fmt "%s" output else
    let styles = String.split ~d:"," !(flags.xslt) in
    let translate o s = Xsltproc.translate ~style:s o in
    F.fprintf fmt "%s" (List.foldl translate output styles))
  else fprintf_proof fmt p proof t
;;

let run p s strategy = 
 let strategy = parse_strategy strategy None in
 let (p,_,s) = Main.run p s strategy in
 (Answer.of_status s,p)
;;

(* FIXME DELETE
let i = ref 0;;
*)

let optimize p s t =
 let run_timed f x = match t with
  | None -> Some (f x)
  | Some t -> fst (Process.run_timed (Process.Global t) f x)
 in
 let rec optimize p s strategy = function
  | [] -> (match Proof.critical p with
   | None -> (p,s)
   | Some (c,prs) ->
    let to_string = F.sprintf "CP%s" <.> C.to_string ~short:true in
    if C.(<=) c C.linear then (p,s) else try
     let strategy = parse_config (to_string (C.decrease c)) in
     optimize p s (parse_strategy strategy None) prs
    with 
     | C.Undefined -> (p,s)
     | Failure _ -> (p,s))
  | pr :: prs ->
   let ip = Processor.input pr and op = Processor.output pr in
   if List.length ip = 1 && List.length op = 1 then
    let ip = List.hd ip and op = List.hd op in
    let (is,iw) = P.get_sw ip and (os,_) = P.get_sw op in
    let ns = Trs.diff is os and nw = Trs.union iw os in
    let np = P.set_sw ns nw ip in
(* FIXME DELETE
i := !i + 1;
Format.printf "%d:@\n" !i;
P.fprintf Format.std_formatter np;
Format.printf "@\n%!";
*)
    match run_timed (Main.run np s) strategy with
     | None -> (p,s)
     | Some (q,s,status) ->
(* FIXME DELETE
Format.printf "Status: ";
Status.fprintf Format.std_formatter status;
Format.printf "@\n@\n%!";
*)
      if Status.is_terminating status then
       try optimize (Proof.optimize pr p q) s strategy prs
       with Failure _ -> (p,s)
      else (p,s)
   else (p,s)
 in
 optimize p s (parse_strategy "fail" None) []
;;

let execute () =
 try
  let start = Unix.gettimeofday () in
  configurate Sys.argv;
  let (p,s) = parse_problem ~stdin:!(flags.stdin) !(flags.file) in
  let strategy = parse_strategy !(flags.strategy) !(flags.timeout) in
  let (proof,s,status) = Main.run p s strategy in
  let (proof,s) =
   if !(flags.optimize) && Option.is_some !(flags.complexity) &&
     Status.is_terminating status then
    optimize proof s (Option.map ((+.) start) !(flags.timeout))
   else (proof,s)
  in
  let time = Unix.gettimeofday () -. start in
  fprintf F.std_formatter p (proof,s,status) time
 with
  | Failure s -> F.eprintf "./ttt2: %s@\n%!" s
  | Help -> Arg.usagex help usage; strategy (); config ()
  | Locate e -> Main.locate ~fs:!(flags.flags) (Str.regexp (e^"$"))
  | Processors -> Main.help ~fs:!(flags.flags) ()
  | Version -> F.printf "%s@\n" version
;;

