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
module G = Graph;;
module M = Monad;;
module P = Problem;;

(*** TYPES ********************************************************************)
type flags = {help : bool ref};;
type t = P.t * P.t;;

(*** GLOBALS ******************************************************************)
let code = "tdg";;
let name = "TDG Processor";;
let keywords = ["dependency graph";"approximation";"termination"];;
let flags = {help = ref false};;

let comment =
 "Remove all edges from the current DG that are not contained in the \
  trivial DG (based on root comparison)."
;;

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
let init _ = flags.help := false;;

(* Destructors *)
let get_ip = fst;;
let get_op = snd;;

(* Processor *)
let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.is_dp p || (P.is_edp p) then
  let s = "left-hand side is a variable" in
  let root f = Option.the <.> Term.root <.> f <?> s in
  let is_arc (x,y) = root Rule.rhs x = root Rule.lhs y in
  let generate x y = if is_arc (x,y) then [(x,y)] else [] in
  match P.get_dg p with
   | P.Complete ->
    let g = G.generate generate (Trs.to_list (P.get_dps p)) in
    let n = Int.square (Trs.size (P.get_dps p)) in
    if G.size_edges g < n then Some (p,P.set_dg (P.Partial g) p) else None
   | P.Partial g -> 
    let g = G.restrict (Trs.to_list (P.get_dps p)) g in
    let h = G.filter_edges is_arc g and n = G.size_edges g in
    if G.size_edges h < n then Some (p,P.set_dg (P.Partial h) p) else None
 else None
;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.other;;

(* Compare Functions *)
let equal p q = P.equal (get_ip p) (get_ip q);;

(* Printers *)
let fprintf fs fmt p =
 F.fprintf fmt "@[<1>%s:@\n" name;
 P.fprintfm ~g:true fmt (get_op p) >>= fun _ ->
 F.fprintf fmt "@\n"; List.hd fs fmt >>= fun _ ->
 M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 F.fprintf fmt "@{<dependencyGraph>";
 P.fprintfx ~g:true fmt (get_op p) >>= fun _ ->
 List.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@}")
;;
