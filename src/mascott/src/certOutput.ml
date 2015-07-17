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
 match Completion.goal o with 
  | None -> return ()
  | Some e ->
    let u, v = Equation.terms e in
    F.fprintf fmt "@{<equation>@,";
    W.M.Termx.readable u >>= W.M.Term.fprintfx fmt >>= fun _ ->
    W.M.Termx.readable v >>= W.M.Term.fprintfx fmt >>= fun _ ->
    F.fprintf fmt "@}"; return ()
;;

let theory o =
 let acs = function
  | Theory.AC f -> Theory.ac f
  | _ -> []
 in List.flat_map acs (Completion.theory o)
;;

let fprintfx_input fmt name =
  F.fprintf fmt "@{<input>@ @{<%s>@ @{<equations>@ " name;
  W.get_options >>= fun o ->
  let es = Trs.of_list (List.map Equation.to_rule (Completion.axioms o)) in
  let th = Trs.of_list (List.map Equation.to_rule (theory o)) in
  let axs = Trs.union es th in
  W.M.Trsx.readable axs >>= W.M.Trs.fprintfx fmt >>= fun _ ->
  F.fprintf fmt "@}";
  fprintfx_goal fmt >>= fun _ ->
  F.fprintf fmt "@}@}";
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
 W.M.Rulex.readable rule >>= W.M.Rule.fprintfx fmt >>= fun _ ->
 F.fprintf fmt "%s" (if lr then "<leftRight/>" else "<rightLeft/>");
 W.M.Termx.readable t >>= W.M.Term.fprintfx fmt >>= fun _ ->
 F.fprintf fmt "@}@ ";
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
  W.M.Termx.readable s >>= W.M.Term.fprintfx fmt >>= fun _ ->
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
   W.M.Termx.readable t >>= W.M.Term.fprintfx fmt >>= fun _ ->
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


let fprintfx_rule_subsumption_proof_conv fmt seq =
  let l,r = Conversion.start_term seq, Conversion.end_term seq in
  F.fprintf fmt "@{<ruleSubsumptionProof>";
  W.M.Rulex.readable (Rule.of_terms l r) >>= W.M.Rule.fprintfx fmt >>= fun _ ->
  F.fprintf fmt "@{<conversion>@,";
  let s = Conversion.start_term seq in
  F.fprintf fmt "@{<startTerm>@,";
  W.M.Termx.readable s >>= W.M.Term.fprintfx fmt >>= fun _ ->
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


