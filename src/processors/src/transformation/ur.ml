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
module H = Hashtbl;;
module L = List;;
module M = Monad;;
module P = Problem;;

(*** TYPES ********************************************************************)
type flags = {ce : bool ref; help : bool ref};;
type term = Term.t = Var of Variable.t | Fun of Function.t * term list;;
type t = P.t * P.t;;

(*** GLOBALS ******************************************************************)
let code = "ur";;
let name = "Usable Rule Processor";;
let keywords = ["usable rules";"transformation"];;
let flags = {ce = ref false; help = ref false};;

let comment =
 "Removes all rules of the given DP Problem which are not usable. Note that \
  this processor is not sound if the given DP problem is duplicating."
;;

let spec =
 let spec = [
  ("-ce",Arg.Set flags.ce,
   "Returns the usable rules together with two projection rules if the DP \
    problem is duplicating.");
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.")]
 in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,L.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>) = M.(>>);;
let (>>=) = M.(>>=);;
let init _ = flags.ce := false; flags.help := false;;

(* Destructors *)
let get_ip = fst;;
let get_op = snd;;

(* Processor *)
let ur_term covered t trs =
 let rec ur_term = function
  | Var _ -> M.return Trs.empty
  | Fun (f,ts) as t ->
   if H.mem covered t then M.return Trs.empty
   else
    (H.add covered t (); M.map (flip Trs.etcap trs) ts) >>= fun ts ->
    let u = Fun (f,ts) in
    let ur = Trs.filter (Elogic.ground_matches u <.> Rule.lhs) trs in
    let rs = Trs.rhs ur in
    let ts = L.unique_hash (L.rev_append (Term.proper_subterms t) rs) in
    M.foldl (fun ur t -> M.lift (Trs.union ur) (ur_term t)) ur ts
 in
 ur_term t
;;

let ur dps trs =
 let covered = H.create 1000 in
 let union ur r = M.lift (Trs.union ur) (ur_term covered r trs) in
 M.foldl union Trs.empty (Trs.rhs dps)
;;

let add_ce dps ur =
 if Trs.is_duplicating dps || Trs.is_duplicating ur then
  M.fresh_fun >>= fun f -> M.add_ari f 2 >>
  M.create_var "x" >>= fun x -> M.create_var "y" >>= fun y ->
  let l = Fun (f,[Var x;Var y]) in
  let r = Rule.of_terms l (Var x) and r' = Rule.of_terms l (Var y) in
  M.return (Trs.add r (Trs.add r' ur))
 else M.return ur
;;

let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.is_dp p || P.is_edp p then
  let dps = P.get_dps p and trs = P.get_trs p in ur dps trs >>= fun ur ->
  if Trs.is_proper_subset ur trs then
   (if !(flags.ce) then add_ce dps ur else M.return ur) >>=
   (M.return <.> Option.some <.> Pair.make p <.> flip P.set_trs p)
  else M.return None
 else M.return None
;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.other;;

(* Compare Functions *)
let equal p q =
 P.equal (get_ip p) (get_ip q) && P.equal (get_op p) (get_op q)
;;

(* Printers *)
let fprintf fs fmt p =
 F.fprintf fmt "@[<1>%s:@\n" name; P.fprintfm fmt (get_op p) >>= fun _ ->
 F.fprintf fmt "@\n"; L.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 F.fprintf fmt "@{<usableRulesProc>";
 P.fprintfx fmt (get_op p) >>= fun _ ->
 List.hd fs fmt             >>= fun _ ->
 M.return(F.fprintf fmt "@}")
;;
