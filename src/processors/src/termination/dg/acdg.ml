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
module Fun = Function;;
module R = Rule;;
module S = Substitution;;
module Sig = Signature;;
(*** TYPES ********************************************************************)
type flags = {help : bool ref; i : bool ref};;
type t = P.t * P.t;;

(*** GLOBALS ******************************************************************)
let code = "acdg";;
let name = "AC-EDG Processor";;
let keywords = ["dependency graph";"approximation";"AC-termination"];;
let flags = {help = ref false; i = ref false};;

let comment =
 "Remove all edges from the current DG that are not contained in the \
  AC-EDG (approximation of AC-DG based on AC-unification)."
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
let init _ = flags.help := false; flags.i := false;;

(* Destructors *)
let get_ip = fst;; (* input problem *)
let get_op = snd;; (* output problem *)

let is_ac f = M.is_theory Label.AC f
let ac_count trs =
 M.filter is_ac (Trs.funs trs) >>= fun acs ->
 M.return (List.length acs)
;;

let reduces_to s trs f g = 
 let lroot = Option.the <.> Term.root <.> Rule.lhs in
 let crs = List.map lroot (Trs.to_list (Trs.filter Rule.is_collapsing trs)) in
 let unlab f = try Sig.drop_dp f s with Not_found -> f in
 let trs = Trs.to_list trs in
 let rroot r = 
  match Term.root (Rule.rhs r) with None -> [] | Some f ->[f] 
 in
 let rec rs fs =
  let rls = List.filter (fun r -> List.mem (lroot r) fs) trs in
  let fs' = List.unique ((List.flat_map rroot rls) @ fs) in
  if List.equal fs' fs then fs else rs fs'
 in 
 let g_no_dp = not (Sig.is_dp g s) in
 let fs = rs [f] in (List.mem g fs) || (List.intersect fs crs <> [] && g_no_dp)
;;

(* Processor *)
(* ets are terms in sharped equations except f(x,y) *)
let is_arc ets mr trs dps (x,y) =
 Rule.to_stringm x >>= fun sx ->
 Rule.to_stringm y >>= fun sy ->
 let t = R.rhs x and u = R.lhs y in
 Term.to_stringm u >>= fun su ->
 Term.to_stringm (Map.find t mr) >>= fun st ->
(* Format.printf " (unify %s and %s) " st su;*)
 (* (1) *)
 let ts = L.filter (Elogic.are_unifiable t) ets in
 let all = L.rev_append dps (Trs.to_list trs) in
((if not (L.exists (fun dp -> Term.root (Rule.rhs dp) = (Term.root u)) all)
  then M.return false 
 else if L.exists ((=) (Term.root u) <.> Term.root) ts then M.return true
 (* (2) *)
 else Aclogic.are_unifiable (Map.find t mr) u) (*>>= fun b ->
  if b then M.return true
  else
   let root = Option.the <.> Term.root in
   is_ac (root t) >>= fun f_is_ac ->
   ac_count trs >>= fun n ->
   M.get >>= fun s -> 
   if (not (f_is_ac)) || n = 1 then M.return false
   else M.return (reduces_to s trs (root t) (root u))) >>= fun a ->
   if a then Format.printf "Arc between %s and %s? %s\n%!" sx sy (if a then "yes" else "no"); M.return a*)
);;

let generate ets mr trs ns =
 M.foldl (fun g n -> M.foldl (fun ms m ->
  M.lift (fun c -> if c then m::ms else ms) (is_arc ets mr trs ns (n,m))) [] ns >>=
  (M.return <.> flip (G.add n) g)) G.empty ns
;;

let filter_edges ets mr trs dps g =
 G.fold (fun n ms m -> m >>= fun g ->
  let add m ms c = M.return (if c then m::ms else ms) in
  M.foldl (fun ms m -> is_arc ets mr trs dps (n,m) >>= add m ms) [] ms >>=
  (M.return <.> flip (G.add n) g)) g (M.return G.empty)
;;

let nontrivial = function
 | Term.Fun(f, [x;y]) -> M.is_theory Label.AC f >>= fun ac -> 
   M.return (not (ac && (List.for_all Term.is_var [x;y])))
 | _ -> M.return true
;;

let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.is_edp p then (* if equational DP problem *) (
  let trs = P.get_trs p and dps = P.get_dps p and g = P.get_dg p in
  M.filter nontrivial (Trs.terms (P.get_eqssharp p)) >>= fun ets ->
  let (ls,rs) = match g with
   | P.Complete -> (Trs.lhs dps,Trs.rhs dps)
   | P.Partial g -> (L.map R.lhs (G.in_nodes g),L.map R.rhs (G.out_nodes g))
  in
  let rs = L.unique_hash rs in
  let add f m t = f t >>= (M.return <.> flip (Map.add t) m) in
  M.foldl (add (flip Trs.ac_rencap trs)) Map.empty rs >>= fun mr ->
  (* compute arcs *)
  match g with
   | P.Complete ->
    generate ets mr trs (Trs.to_list dps) >>= fun g ->
    let n = Int.square (Trs.size dps) and m = G.size_edges g in
    M.return (if m < n then ((*Format.printf "progress in graph\n%!";*) Some (p,P.set_dg (P.Partial g) p)) else ((*Format.printf "no progress in graph\n%!";*) None))
   | P.Partial g ->
    let g = G.restrict (Trs.to_list dps) g in
    filter_edges ets mr trs (Trs.to_list dps) g >>= fun h ->
    let n = G.size_edges g and m = G.size_edges h in
    M.return (if m < n then Some (p,P.set_dg (P.Partial h) p) else None))
 else M.return None
;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.other;;

(* Compare Functions *)
let equal p q = P.equal (get_ip p) (get_ip q) 

(* Printers *)
let fprintf fs fmt p =
 F.fprintf fmt "@[<1>%s:@\n" name;
 P.fprintfm ~g:true fmt (get_op p) >>= fun _ ->
 F.fprintf fmt "@\n"; L.hd fs fmt >>= fun _ ->
 M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 F.fprintf fmt "@{<dependencyGraph>";
 P.fprintfx ~g:true fmt (get_op p) >>= fun _ ->
 L.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@}")
;;
