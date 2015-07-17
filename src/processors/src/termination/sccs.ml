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
type t = P.t * (Rule.t list,Rule.t list) either list * P.t list;;

(*** GLOBALS ******************************************************************)
let code = "sccs";;
let name = "SCC Processor";;
let comment = "Computes the SCCs of the given DP problem.";;
let keywords = ["strongly connected components";"termination"];;
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
let get_ip = Triple.fst;;
let get_sccs = Triple.snd;;
let get_ops = Triple.thd;;

(* Processor *)
let is_simplified sccs g =
 try 
  let scc = List.hd sccs in
  G.is_trivial_scc scc g || List.length scc < G.size_nodes g
 with Failure _ -> false
;;

let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.is_dp p || (P.is_edp p) then match P.get_dg p with
  | P.Complete -> None
  | P.Partial g ->
   let g = G.restrict (Trs.to_list (P.get_dps p)) g in
   let sccs = G.sccs g in
   if List.length sccs > 1 || is_simplified sccs g then
    let create scc =
     let p = P.set_dps (Trs.of_list scc) p in
     P.set_dg (P.Partial (G.restrict scc g)) p
    in
    Some (Triple.insert_fst p (List.foldr (fun scc (sccs,ps) ->
     if G.is_trivial_scc scc g then (Left scc :: sccs,ps)
     else (Right scc :: sccs,create scc :: ps)) ([],[]) sccs))
   else None
 else None
;;

(* Properties *)
let is_empty p = List.for_all Problem.is_empty (get_ops p);;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.other;;

(* Compare Functions *)
let equal p q = P.equal (get_ip p) (get_ip q);;

(* Printers *)
let fprintf ?(s = false) fs fmt p =
 let fprintfi i fmt p =
  P.fprintfm fmt p >>= fun _ -> F.fprintf fmt "@\n"; List.nth fs i fmt
 in
 F.fprintf fmt "@[<1>%s:@\n" name;
 (if s then
  let count_sccs = either (const 0) (const 1) in
  let count_rules = either (const 0) List.length in
  let count (n,m) scc = ((+) n (count_sccs scc),(+) m (count_rules scc)) in
  let (n,m) = List.foldl count (0,0) (get_sccs p) in
  let j = let j = Trs.size (P.get_dps (get_ip p)) in j*j in
  let g = P.get_dg (get_ip p) in
  let i = match g with P.Complete -> j | P.Partial g -> G.size_edges g in
  F.fprintf fmt "#sccs: %d@\n#rules: %d@\n#arcs: %d/%d@\n" n m i j);
 M.fprintfi fprintfi "@\n@\n" fmt (get_ops p) >>= fun _ ->
 M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 let fprintfx fmt i rs =
  let fprintfx_proof i = function
   | Right _ -> (try List.nth fs i fmt with _ -> failwith "Here") >> M.return (i + 1)
   | _ -> M.return i
  in
  let dps = Trs.of_list (Either.strip rs) in
  F.fprintf fmt "@{<component>@{<dps>";
  Trs.fprintfx fmt dps >>= fun _ ->
  F.fprintf fmt "@}";
  F.fprintf fmt "@{<realScc>%s@}" (either (const"false") (const"true") rs);
  fprintfx_proof i rs  >>= fun i ->
  F.fprintf fmt "@}";
  M.return i
 in
 F.fprintf fmt "@{<depGraphProc>";
 M.foldl (fprintfx fmt) 0 (get_sccs p) >>= fun _ ->
 M.return (F.fprintf fmt "@}")
;;
