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
type flags = {
  help : bool ref;
  label : bool ref;
 };;
type t = P.t * P.t;;

(*** GLOBALS ******************************************************************)
let code = "dup";;
let name = "Duplication Transformation Processor";;
let keywords = ["duplication transformation";"transformation"];;
let flags = {
 help = ref false;
 label = ref false;
};;

let comment =
 "Transform a standard problem into relative problem such that
 duplicating rules are in strict component."
;;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-label",Arg.Set flags.label ,"Label steps with dh(R_d,R_nd).");
 ] in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let init _ = 
 flags.help := false;
 flags.label := false;
;;

(* Destructors *)
let get_ip = fst;;
let get_op = snd;;

(*
let cut_ss s ss =
 let dup s = Rule.is_duplicating (Rewrite.step_get_rule s) in
 if dup s then []
 else 
  try 
   let i = List.position dup ss in
   List.take i ss
  with Not_found -> ss
;;

let cut_j (l,r) (ls,rs) = 
 let ls = cut_ss l ls in
 let rs = cut_ss r rs in
 (ls,rs)
;;

let cut_d d = 
 Diagram.make (Diagram.peak d) (List.map (cut_j (Diagram.peak d)) (Diagram.joins d))
;;

let trivial_s ss = ss = [];;

let trivial_j j = 
 let (ls,rs) = j in
 trivial_s ls && trivial_s rs
;;
 
let trivial d = List.exists trivial_j (Diagram.joins d);;
*)

let label_step n s = 
 let m = if Rule.is_duplicating (Rewrite.step_get_rule s) then n - 1 else n in
 (Rewrite.step_add_lab_left s n,m)
;;

let rec label_seq n = function
 | [] -> []
 | s::ss -> let (s,n) = label_step n s in s::label_seq n ss
;;

let label_d d =
 let (l,r) = Diagram.peak d in
 let (l,nl) = label_step 0 l and (r,nr) = label_step 0 r in
 let js = List.map (fun (ls,rs) -> label_seq nl ls, label_seq nr rs) (Diagram.joins d) in
 Diagram.make (l,r) js
;;

let label cds = List.map label_d cds;;

(* Processor *)
let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.is_sp p then 
  let trs = P.get_trs p in
  let strict,weak = Trs.partition Rule.is_duplicating trs in
  let s = P.get_strategy p and l = P.get_language p in
  Diagram.critical trs >>= fun cds ->
  let cds = if !(flags.label) then label cds else cds in
  M.return (Some (p,P.make_crp l s trs strict weak cds))
 else M.return None
;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.constant;;

(* Compare Functions *)
let equal p q = P.equal (get_ip p) (get_ip q);;

(* Printers *)
let fprintf fs fmt p =
 F.fprintf fmt "@[<1>%s:@\n" name; P.fprintfm fmt (get_op p) >>= fun _ ->
 F.fprintf fmt "@\n"; List.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 F.fprintf fmt "@{<rtTrans>";
 P.fprintfx fmt (get_op p) >>= fun _ ->
 List.hd fs fmt            >>= fun _ ->
 M.return(F.fprintf fmt "@}")
;;
