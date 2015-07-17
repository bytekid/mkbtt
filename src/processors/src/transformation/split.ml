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

(*** TYPES ********************************************************************)
type flags = {help : bool ref};;
type t = P.t * P.t list;;

(*** GLOBALS ******************************************************************)
let code = "split";;
let name = "Splitting Processor";;

let comment =
 "Splits a CP problem into a list of CP problems by combining each rewrite \
  rule with all other rules. So as result a list containing all minimal CP \
  problems is returned."
;;

let keywords = ["complexity";"transformation";"splitting"];;
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
let (>>) = M.(>>);;
let init _ = flags.help := false;;

(* Destructors *)
let get_ip = fst;;
let get_ops = snd;;

(* Processor *)
let generate fs p ps =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.is_cp p && List.for_all P.is_cp ps then
  let (s,w) = P.get_sw p in
  let trs = Trs.union s w in
  let subset s' w' = Trs.is_subset s' s && Trs.is_subset w' trs in
  let check s' w' = subset s' w' && Trs.is_empty (Trs.intersect s' w') in
  if List.for_all (uncurry check <.> P.get_sw) ps then
   let union (s',w') (s,w) = (Trs.union s' s,Trs.union w' w) in
   let combine (s',w') = union (s',w') <.> P.get_sw in
   let (s',w') = List.foldl combine (Trs.empty,Trs.empty) ps in
   if Trs.equivalent s' s && Trs.equivalent w' trs then Some (p,ps) else None
  else None
 else None
;;

let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.is_cp p then
  let (s,w) = P.get_sw p in
  let trs = Trs.union s w in
  let make r = P.set_sw (Trs.singleton r) (Trs.remove r trs) p in
  Some (p,Trs.map make s)
 else None
;;

(* Complexity Bounds *)
let complexity c _ = C.add c C.constant;;

(* Compare Functions *)
let equal p q = P.equal (get_ip p) (get_ip q);;

(* Printers *)
let fprintf fs fmt p =
 let fprintfi i fmt p =
  P.fprintfm fmt p >>= fun _ -> F.fprintf fmt "@\n"; List.nth fs i fmt
 in
 F.fprintf fmt "@[<1>%s:@\n" name;
 M.fprintfi fprintfi "@\n@\n" fmt (get_ops p) >>= fun _ ->
 M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 let fprintfxi i p =
  F.fprintf fmt "@{<component>"; P.fprintfx fmt p >>= fun _ ->
  List.nth fs i fmt >> M.return (F.fprintf fmt "@}")
 in
 F.fprintf fmt "@{<cpTrans>"; M.iteri fprintfxi (get_ops p) >>= fun _ ->
 M.return (F.fprintf fmt "@}")
;;
