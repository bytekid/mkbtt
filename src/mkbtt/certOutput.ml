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

(** Generate output suitable for certification
@author Sarah Winkler
@since  2011/11/23 *)

open Util;;

(*** SUBMODULES **********************************************************)
module C = Completion;;
module W = World;;
module M = W.Monad;;
module Rule = U.Rule;;
module Term = U.Term;;
module Trs = U.Trs;;
module F = Format;;
module Pos = Rewriting.Position;;
module Status = Ttt2.Strategy.Status;;
module INode = IndexedNode;;
module EPT = EquationalProofTree;;
module Sub = U.Substitution;;

(*** OPENS (2) ***********************************************************)
open World;;
open World.Monad;;(*
let (>>=) = M.(>>=);;
let (>>) = M.(>>);;
let return = M.return;;
let lift = M.lift;;*)

(*** GLOBALS *************************************************************)
let version = "2.1"

(*** FUNCTIONS ***********************************************************)
(*
let html_replace s =
 let replace s (pat, rep) = 
(*   W.M.liftm (U.Monad.get >>= fun st -> U.Monad.return (U.Signature.find_fun st pat)) >>= fun _ -> *) 
   W.M.liftm (U.Monad.find_fun pat) >>= function
    | Left _ -> return s (* if no such function symbol exists return s *)
    | Right _ -> return (Str.global_replace (Str.regexp pat) rep s)
 in
 foldl replace s ["&amp;","&"]
;;*)

let fprintfx_goal fmt =
 W.get_options >>= fun o ->
 if Completion.mode o <> Completion.Proof then return ()
 else
  W.get_goal_state >>= fun s ->
  iter (fun e -> 
    let u, v = Equation.terms e in
    F.fprintf fmt "@{<equation>@,";
    W.M.Term.fprintfx fmt u >>= fun _ ->
    W.M.Term.fprintfx fmt v >>= fun _ ->
    F.fprintf fmt "@}"; return ()
  ) s.goal_eqs
;;

let fprintfx_input fmt name =
  F.fprintf fmt "@{<input>@ @{<%s>@ @{<equations>@ " name;
  W.get_options >>= fun o ->
(*  NodeState.all_nodes >>= map (lift Option.the <.> INode.by_id) >>= fun ns ->
  let axs = List.map Nodex.data (List.filter Nodex.is_axiom ns) in
  let es = Trs.of_list (List.map (Util.uncurry Rule.of_terms) axs) in
  W.M.Trsx.fprintfx fmt es >>= fun _ -> 
  F.fprintf fmt "@}";*)
(*  map INode.by_eqn (Completion.axioms o) >>= fun is ->
  map INode.by_id (List.map Option.the is) >>= fun ns ->
  let axs = List.map (Nodex.data <.> Option.the) ns in
  let es = Trs.of_list (List.map (Util.uncurry Rule.of_terms) axs) in
  W.M.Trsx.fprintfx fmt es >>= fun _ ->
  F.fprintf fmt "@}";*)
  map INode.by_id (Completion.axiom_ids o) >>= fun ns ->
  let trs = List.map (fun n -> let s,t = Nodex.data (Option.the n) in
   Rule.of_terms s t) ns in
  W.M.Trsx.fprintfx fmt (Trs.of_list trs) >>= fun _ ->
  F.fprintf fmt "@}";
  fprintfx_goal fmt >>= fun _ ->
  F.fprintf fmt "@}@}";
  return ()
;;

let fprintfx_input_output fmt trs =
  F.fprintf fmt "@{<input>@ @{<completionInput>@ @{<equations>@ ";
  W.get_options >>= fun o ->
  let es = List.map Equation.to_rule (Completion.axioms o) in
  W.M.Trsx.fprintfx fmt (Trs.of_list es) >>= fun _ ->
  F.fprintf fmt "@}";
  F.fprintf fmt "@{<trs>";
  W.M.Trsx.fprintfx fmt trs >>= fun _ ->
  F.fprintf fmt "@}@}@}";
  return ()
;;

let index_of s p =
 let rec idx s offset =
  if s = "" then -1 else
  if String.is_prefix p s then offset
  else idx (String.sub s 1 (String.length s - 1)) (offset + 1)
 in
idx s 0;;

let last_index_of s p =
 let rec lidx s last =
(*  Format.printf "last: %i\n%!" last;*)
  let i = index_of s p in
  if i < 0 then last else
  lidx (String.sub s (i+1) (String.length s - (i + 1))) (i+last +1)
 in lidx s (-1)
;;

let fprintfx_termination_proof fmt (s,pf) = 
  F.fprintf fmt "@{<myTerminationProof>";
  if (s <> Status.terminating) then failwith "not terminating";
  W.M.liftm (U.MyProof.fprintfx s fmt pf) >>= fun _ ->
  F.fprintf fmt "@}";
  return ()
;;

let fprintfx_ext_termination_proof fmt s =
 (* ExternalTermination.certify trs >>= fun s ->*)
(* html_replace s >>= fun s ->*)
 let b = index_of s "<trsTerminationProof>" in
 let e = last_index_of s "</trsTerminationProof>" in
 if b < 0 then failwith "no term proof found";
 let s' = String.sub s b (e + 22 - b) in
 F.fprintf fmt "%s" s';
 return ()
;;

let rec pos_fprintfx fmt p =
 if p <> Pos.root then 
  let i,p = Pos.split_first p in
  (* positions as positive integers *)
  F.fprintf fmt "@{<position>%i@}" (i+1);
  pos_fprintfx fmt p
;;

let fprintfx_equation_step fmt (s,p,rule,lr,t) =
 F.fprintf fmt "@{<equationStep>@ @{<positionInTerm>";
 pos_fprintfx fmt p;
 F.fprintf fmt "@}";
 W.M.Rule.fprintfx fmt rule >>= fun _ ->
 F.fprintf fmt "%s" (if lr then "<leftRight/>" else "<rightLeft/>"); 
 W.M.Term.fprintfx fmt t >>= fun _ ->
 F.fprintf fmt "@}@ ";
(* W.M.Term.to_stringm s >>= fun s ->
 W.M.Term.to_stringm t >>= fun t ->
 W.M.Rule.to_stringm rule >>= fun r ->
 let ps = Pos.to_string p in
 let lr = if lr then "left to right" else "right to left" in
 Format.fprintf F.std_formatter " %s = %s via %s from %s at %s \n%!" s t r lr ps;*)
 return ()
;;

let fprintfx_conversion fmt = function
 | [] -> return ()
 | (t,_,_,_,_) :: _ as seq ->
  F.fprintf fmt "@{<startTerm>";
  W.M.Term.fprintfx fmt t >>= fun _ ->
  F.fprintf fmt "@}";
(*  W.M.Term.to_stringm t >>= fun s ->
  Format.printf "starting at %s\n%!" s;*)
  iter (fprintfx_equation_step fmt) seq >>
  return ()
;;


let fprintfx_rule_subsumption_proof fmt (r,c) =
  F.fprintf fmt "@{<ruleSubsumptionProof>";
  W.M.Rule.fprintfx fmt r >>= fun _ ->
  F.fprintf fmt "@{<conversion>";
(*  W.M.Rule.to_stringm r >>= fun s ->
  Format.printf "conversion for rule %s\n%!" s;*)
  fprintfx_conversion fmt c >>= fun _ ->
  F.fprintf fmt "@}@}";
  return ()
;;

let fprintfx_equivalence_proof fmt trace =
  F.fprintf fmt "@{<equivalenceProof>@{<subsumptionProof>";
  iter (fprintfx_rule_subsumption_proof fmt) trace >>= fun _ ->
  F.fprintf fmt "@}@}";
  return ()
;;


let fprintfx_proof fmt trace trs =
  F.fprintf fmt "@{<proof>@{<completionProof>";
  F.fprintf fmt "@{<wcrProof>@{<joinableCriticalPairsAuto>@}@}";
  fprintfx_ext_termination_proof fmt trs >>= fun _ -> 
  fprintfx_equivalence_proof fmt trace >>= fun _ ->
  F.fprintf fmt "@}@}";
  return ()
;;

let fprintfx_origin fmt =
  F.fprintf fmt "@{<origin>@{<proofOrigin>@{<tool>";
  F.fprintf fmt "@{<name>%s@}@{<version>%s@}" "mkbTT" version;
  F.fprintf fmt "@}@}@}";
  return ()
;;

let print_header fmt =
  F.fprintf fmt "@?<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
  F.fprintf fmt "<?xml-stylesheet type=\"text/xsl\" href=\"cpfHTML.xsl\"?>";
  F.fprintf fmt " @{<certificationProblem \
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \
     xsi:noNamespaceSchemaLocation=\"cpf.xsd\">"
;;

let fprintfx_completeness p trs =
  let fmt = F.std_formatter in
  W.get_options >>= fun o ->
(*  let s = Completion.certification_strategy o in
  InterfaceTTT.prove_with trs s >>= fun proof ->*)
  Trace.trs2 p trs >>= fun (trs, trace) ->
  ExternalTermination.certify trs >>= fun proof ->
  print_header fmt;
  fprintfx_input_output fmt trs >>= fun _ ->
  F.fprintf fmt "@{<cpfVersion>%s@}" version;
  fprintfx_proof fmt trace proof >>= fun _ ->
  fprintfx_origin fmt >>= fun _ ->
  F.fprintf fmt "@}@.";
  F.print_newline ();
  return ()
;;

let fprintfx_equation_step' fmt = function
 | Conversion.Step (s, (i,b), p, t) -> 
    ((lift Option.the) <.> INode.by_id) i >>= fun n ->
    let rl = Nodex.brule true n in
    fprintfx_equation_step fmt (s, p, rl, b, t)
 | _ -> return ()

let fprintfx_conversion' fmt seq =
  F.fprintf fmt " @{<proof>@,@{<equationalProof>@,@{<conversion>@,";
  let s = Conversion.start_term seq in
  F.fprintf fmt "@{<startTerm>@,";
  W.M.Term.fprintfx fmt s >>= fun _ ->
  F.fprintf fmt "@}";
  iter (fprintfx_equation_step' fmt) seq >>= fun _ ->
  F.fprintf fmt "@}@}@}";
  return ()
;;

let fprintfx_conversion seq =
  let fmt = F.std_formatter in
  W.get_options >>= fun o ->
  print_header fmt;
  fprintfx_input fmt "equationalReasoningInput" >>= fun _ ->
  F.fprintf fmt "@{<cpfVersion>%s@}@," version;
  fprintfx_conversion' fmt seq >>= fun _ ->
  fprintfx_origin fmt >>= fun _ ->
  F.fprintf fmt "@}@.";
  F.print_newline ();
  return ()
;;

let fprintfx_subst fmt s =
 let fprintfx fmt s =
  Sub.fold (fun x t m ->
   m >>= fun _ ->
   let x = Term.make_var x in
   F.fprintf fmt "<substEntry>"; W.M.Term.fprintfx fmt x >>= fun _ ->
   W.M.Term.fprintfx fmt t >>= fun _ ->
   return (F.fprintf fmt "</substEntry>")) s (return ())
 in
 F.fprintf fmt "<substitution>"; fprintfx fmt s >>= fun _ ->
 return (F.fprintf fmt "</substitution>")
;;

let fprintfx_proof_trees fmt trees =
  let rec fprintfx_proof_tree' fmt = function
   | EPT.Refl u -> 
     F.fprintf fmt "@{<refl>@,";
     W.M.Term.fprintfx fmt u  >>= fun _ ->
     F.fprintf fmt "@}";
     return ()
   | EPT.Sym t -> 
     F.fprintf fmt "@{<sym>@,";
     fprintfx_proof_tree' fmt t >>= fun _ ->
     F.fprintf fmt "@}";
     return ()
   | EPT.Trans (t1,t2) ->
     F.fprintf fmt "@{<trans>@,";
     fprintfx_proof_tree' fmt t1 >>= fun _ ->
     fprintfx_proof_tree' fmt t2 >>= fun _ ->
     F.fprintf fmt "@}";
     return ()
   | EPT.Cong (f, ts) ->
     F.fprintf fmt "@{<cong>@,";
     liftm (U.Monad.fprintfx_fun fmt f) >>= fun _ ->
     iter (fprintfx_proof_tree' fmt) ts >>= fun _ ->
     F.fprintf fmt "@}";
     return ()
   | EPT.Assm (rl, sigma) ->
     F.fprintf fmt "@{<assm>@,";
     W.M.Rule.fprintfx fmt rl >>= fun _ ->
     fprintfx_subst fmt sigma >>= fun _ ->
     F.fprintf fmt "@}";
     return () 
  in
  let fprintfx_proof_tree fmt tree =
   F.fprintf fmt "@{<equationalProofTree>@,";
   fprintfx_proof_tree' fmt tree >>= fun _ ->
   F.fprintf fmt "@}";
  return ()
  in
  F.fprintf fmt " @{<proof>@,@{<equationalProof>@,";
  iter (fprintfx_proof_tree fmt) trees >>= fun _ ->
  F.fprintf fmt "@}@}";
  return ()
;;

let fprintfx_proof_trees trees =
  let fmt = F.std_formatter in
  W.get_options >>= fun o ->
  print_header fmt;
  fprintfx_input fmt "equationalReasoningInput" >>= fun _ ->
  F.fprintf fmt "@{<cpfVersion>%s@}@," version;
  fprintfx_proof_trees fmt trees >>= fun _ ->
  fprintfx_origin fmt >>= fun _ ->
  F.fprintf fmt "@}@.";
  F.print_newline ();
  return ()
;;


let fprintfx_disproof fmt trace proof trs =
  F.fprintf fmt "@{<proof>@{<equationalDisproof>@{<completionAndNormalization>";
  F.fprintf fmt "@{<trs>";
  W.M.Trsx.fprintfx fmt trs >>= fun _ ->
  F.fprintf fmt "@}";
  F.fprintf fmt "@{<completionProof>";
  F.fprintf fmt "@{<wcrProof>@{<joinableCriticalPairsAuto>@}@}";
  fprintfx_ext_termination_proof fmt proof >>= fun _ ->
  fprintfx_equivalence_proof fmt trace >>= fun _ ->
  F.fprintf fmt "@}@}@}@}";
  return ()
;;

let fprintfx_disproof p trs =
  let fmt = F.std_formatter in
  W.get_options >>= fun o ->
  Trace.trs2 p trs >>= fun (trs,trace) ->
  ExternalTermination.certify trs >>= fun proof ->
  print_header fmt;
  fprintfx_input fmt "equationalReasoningInput" >>= fun _ ->
  F.fprintf fmt "@{<cpfVersion>%s@}" version;
  fprintfx_disproof fmt trace proof trs >>= fun _ ->
  fprintfx_origin fmt >>= fun _ ->
  F.fprintf fmt "@}@.";
  F.print_newline ();
  return ()
;;

let fprintfx_rule_subsumption_proof_conv fmt seq =
  let l,r = Conversion.start_term seq, Conversion.end_term seq in
  F.fprintf fmt "@{<ruleSubsumptionProof>";
  W.M.Rule.fprintfx fmt (Rule.of_terms l r) >>= fun _ ->
  F.fprintf fmt "@{<conversion>@,";
  let s = Conversion.start_term seq in
  F.fprintf fmt "@{<startTerm>@,";
  W.M.Term.fprintfx fmt s >>= fun _ ->
  F.fprintf fmt "@}";
  iter (fprintfx_equation_step' fmt) seq >>= fun _ ->
  F.fprintf fmt "@}@}";
  return ()
;;

let fprintfx_subsumption_proof conv =
  let fmt = F.std_formatter in
  W.get_options >>= fun o ->
  print_header fmt;
  fprintfx_input fmt "equationalReasoningInput" >>= fun _ ->
  F.fprintf fmt "@{<cpfVersion>%s@}" version;
  F.fprintf fmt " @{<proof>@,@{<equationalProof>@,@{<subsumptionProof>";
  iter (fprintfx_rule_subsumption_proof_conv fmt) conv >>= fun _ ->
  F.fprintf fmt "@}@}@}";
  fprintfx_origin fmt >>= fun _ ->
  F.fprintf fmt "@}@.";
  F.print_newline ();
  return ()
;;
