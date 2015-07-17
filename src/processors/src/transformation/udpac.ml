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

(*** OPENS ********************************************************************)
open Util;;
open Rewritingx;;

(*** MODULES ******************************************************************)
module C = Complexity;;
module F = Format;;
module M = Monad;;
module P = Problem;;
module Sig = Signature;;

(*** TYPES ********************************************************************)
type flags = { help : bool ref};;
type t = P.t * P.t;;

(*** GLOBALS ******************************************************************)
let code = "uac";;
let name = "AC-DP unlabeling";;
let comment = "Drops # from AC dependency pairs rooted by AC symbols.";;
let keywords = ["dp transformation";"termination"; "AC"];;
let flags = {help = ref false};;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.")]
 in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let return = M.return;;
let init _ = flags.help := false;;

(* Destructors *)
let get_ip = fst;;
let get_op = snd;;

(* Processor *)
let is_ac f = M.get >>= fun c -> return (Sig.is_theory Label.AC f c);;

let is_c f = M.get >>= fun c -> return (Sig.is_theory Label.C f c);;

let unlabel rl =
 let unlabel t = 
  is_ac (Option.the (Term.root t)) >>= fun f_is_ac ->
  if f_is_ac then Term.unlabel_dp t else return t
 in
 M.project unlabel (Rule.to_terms rl) >>= fun (l,r) ->
 return (Rule.of_terms l r)
;;


let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if not (P.is_edp p) then M.return None
 else
  M.map unlabel (Trs.to_list (P.get_dps p)) >>= fun dps ->
  M.map unlabel (Trs.to_list (P.get_srules p)) >>= fun s' ->
  M.map unlabel (Trs.to_list (P.get_eqssharp p)) >>= fun ess ->
  let dps, s', ess = Trs.of_list dps, Trs.of_list s', Trs.of_list ess in
  let l, s = P.get_language p, P.get_strategy p in
  let p' = P.make_edp l s ess dps (P.get_eqs p) (P.get_trs p) s' (P.get_dg p) in
  return (Some (p, p'))
;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.other;;

(* Compare Functions *)
let equal p q = P.equal (get_ip p) (get_ip q);;

(* Printers *)
let fprintf fs fmt p =
 F.fprintf fmt "@[<1>%s:@\n" name; P.fprintfm fmt (get_op p) >>= fun _ ->
 F.fprintf fmt "@\n"; List.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 F.fprintf fmt "@{<dpTrans>"; P.fprintfx fmt (get_op p) >>= fun _ ->
 F.fprintf fmt "@{<markedSymbols>true@}"; 
 (if Trs.is_empty (P.get_dps (get_op p)) then
  M.return (F.fprintf fmt "@{<dpProof>@{<pIsEmpty>@}@}")
 else (
  F.fprintf fmt "@{<dpProof>"; List.hd fs fmt >>= fun _ -> 
  M.return  (F.fprintf fmt "@}"))) 
 >>= fun _ -> M.return (F.fprintf fmt "@}")
;;

(*
let fprintfx fs prob =
  let op  = get_op prob in
  let dps = P.get_dps op in
  XO.node "dpTrans" [
    P.fprintfx op;
    XO.bool "markedSymbols" true;
    if Trs.is_empty dps
      then XO.node "dpProof" [XO.leaf "pIsEmpty"]
      else List.hd fs
];;*)
