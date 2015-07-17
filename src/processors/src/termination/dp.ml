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
type flags = {acn: bool ref; actk: bool ref; fresh : bool ref; help : bool ref};;
type t = P.t * P.t;;

(*** GLOBALS ******************************************************************)
let code = "dp";;
let name = "DP Processor";;
let comment = "Transforms the given TRS into a DP problem.";;
let keywords = ["dp transformation";"termination"];;
let flags = {acn = ref false; actk = ref false; fresh = ref true; help = ref false};;

let spec =
 let spec = [
  ("-acn",Arg.Set flags.acn,"No #s on AC symbols.");
  ("-actk",Arg.Set flags.actk,"Dependency pairs a la Toyama/Kusakari 98.");
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-sig",Arg.Clear flags.fresh,"Uses the original symbols as DP symbols.")]
 in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let return = M.return;;
let init _ = flags.acn := false; flags.fresh := true; flags.help := false;;

(* Destructors *)
let get_ip = fst;;
let get_op = snd;;

(* Processor *)
let is_ac f = M.get >>= fun c -> return (Sig.is_theory Label.AC f c);;

let label f t =
 is_ac (Option.the (Term.root t)) >>= fun f_is_ac ->
 if f_is_ac && !(flags.acn) then return t 
 else f t 
;;

let generate' f trs =
 let ds = Trs.def_symbols trs in
 let generate r m =
  label f (Rule.lhs r) >>= fun l ->
  let r = Rule.rhs r in
  let add m r = match Term.root r with
   | Some g when List.mem g ds && not (Term.is_proper_subterm r l) ->
     label f r >>= fun r -> m >>= (M.return <.> Trs.add (Rule.of_terms l r))
    | _ -> m
  in
  List.foldl add m (Term.subterms r)
 in
 Trs.fold generate (M.return Trs.empty) trs
;;

let generate f trs = 
 generate' f trs >>= fun dps ->
 M.return (Some (P.create_dp (Trs.unique dps) trs P.Complete))
;;

(* SW *)
let generate_ac eqs trs =
 Trs.extend trs >>= fun trs' ->
 let trsx = if !(flags.actk) then trs else trs' in
 generate' Term.label_dp trsx >>= fun dps ->
 let label_terms r = M.project (label Term.label_dp) (Rule.to_terms r) in
 let label r = label_terms r >>= fun (l,r) -> M.return (Rule.of_terms l r) in
 (M.lift Trs.of_list <.> (M.map label)) (Trs.to_list eqs) >>= fun eqssharp ->
 Trs.srules trs >>= fun s ->
 let edps = Trs.unique dps in
 let e = Trs.empty in
 let s = if !(flags.acn) || !(flags.actk) then e else s in
 M.return (Some (P.create_edp eqssharp edps eqs trs s P.Complete))
;;

let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.is_sp p then
  (if !(flags.fresh) then generate Term.label_dp (P.get_trs p)
  else generate M.return (P.get_trs p)) >>=
  (M.return <.> Option.map (Pair.make p <.> flip P.adapt p))
 (* SW *)
 else if P.is_ep p then
  generate_ac (P.get_eqs p) (P.get_trs p) >>=
  (M.return <.> Option.map (Pair.make p <.> flip P.adapt p))
 else M.return None
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
