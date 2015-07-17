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
module L = List;;
module M = Monad;;
module Map = Map.Partial (Term);;
module P = Problem;;
module R = Rule;;
module S = Substitution;;

(*** TYPES ********************************************************************)
type flags = {help : bool ref};;
type t = P.t * P.t;;

(*** GLOBALS ******************************************************************)
let code = "odg";;
let name = "ODG Processor";;
let keywords = ["dependency graph";"approximation";"termination"];;
let flags = {help = ref false};;

let comment =
 "Remove all edges from l->r to l'->r' where ren(cap(r)) is not unifiable \
  with l' (previous edg implementation)."
;;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.")]
 in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,L.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let init _ = flags.help := false;;

(* Destructors *)
let get_idp = fst;;
let get_odp = snd;;

(* Processor *)
let is_arc trs ml mr (x,y) =
 let s = R.lhs x and t = R.rhs x and u = R.lhs y and v = R.rhs y in
 M.return (R.of_terms s (Map.find t mr)) >>= fun x' ->
 M.return (R.of_terms (Map.find u ml) v) >>= fun y' ->
 let s' = R.lhs x' and t' = R.rhs x' and u' = R.lhs y' in
 let root = Option.the <.> Term.root <?> "left-hand side is a variable" in
 if root t = root u then M.return (Elogic.are_unifiable t' u')
 else M.return false
;;

let generate trs ml mr ns =
 M.foldl (fun g n -> M.foldl (fun ms m ->
  M.lift (fun c -> if c then m::ms else ms) (is_arc trs ml mr (n,m))) [] ns >>=
  (M.return <.> flip (G.add n) g)) G.empty ns
;;

let filter_edges trs ml mr g =
 G.fold (fun n ms m -> m >>= fun g ->
  let add m ms c = M.return (if c then m::ms else ms) in
  M.foldl (fun ms m -> is_arc trs ml mr (n,m) >>= add m ms) [] ms >>=
  (M.return <.> flip (G.add n) g)) g (M.return G.empty)
;;

let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.is_dp p then
  let trs = P.get_trs p and dps = P.get_dps p and g = P.get_dg p in
  let (ls,rs) = match g with
   | P.Complete -> (Trs.lhs dps,Trs.rhs dps)
   | P.Partial g -> (L.map R.lhs (G.in_nodes g),L.map R.rhs (G.out_nodes g))
  in
  let ls = L.unique_hash ls and rs = L.unique_hash rs in
  (* configurate computation *)
  let modify_lhs = fun l _ -> M.return l in 
  let modify_rhs = Trs.tcap in
  let add f m t = f t >>= (M.return <.> flip (Map.add t) m) in
  M.foldl (add (flip modify_rhs trs)) Map.empty rs >>= fun mr ->
  M.foldl (add (flip modify_lhs (Trs.invert trs))) Map.empty ls >>= fun ml ->
  (* compute arcs *)
  match g with
   | P.Complete ->
    generate trs ml mr (Trs.to_list dps) >>= fun g ->
    let n = Int.square (Trs.size dps) and m = G.size_edges g in
    M.return (if m < n then Some (p,P.set_dg (P.Partial g) p) else None)
   | P.Partial g ->
    let g = G.restrict (Trs.to_list dps) g in
    filter_edges trs ml mr g >>= fun h ->
    let n = G.size_edges g and m = G.size_edges h in
    M.return (if m < n then Some (p,P.set_dg (P.Partial h) p) else None)
 else M.return None
;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.other;;

(* Compare Functions *)
let equal p q = P.equal (get_idp p) (get_idp q);;

(* Printers *)
let fprintf fs fmt p =
 F.fprintf fmt "@[<1>%s:@\n" name;
 P.fprintfm ~g:true fmt (get_odp p) >>= fun _ ->
 F.fprintf fmt "@\n"; L.hd fs fmt >>= fun _ ->
 M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 F.fprintf fmt "@{<dependencyGraph>";
 P.fprintfx ~g:true fmt (get_odp p) >>= fun _ ->
 L.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@}")
;;
