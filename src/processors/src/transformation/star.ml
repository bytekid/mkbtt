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
type t = P.t * P.t;;

(*** GLOBALS ******************************************************************)
let code = "star";;
let name = "Star Transformation Processor";;
let keywords = ["star transformation";"transformation"];;
let flags = {help = ref false};;

let comment =
 "Transform a standard problem into relative problem 
  (depending on context)."
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

let rec context_of_term t p = match t with
 | Term.Var _ -> M.return t
 | Term.Fun (f,ts) -> 
   let i,q = Position.split_first p in
   (*TODO*)
   M.set_curry ~arity:1 f i >>= fun fi -> 
   context_of_term (List.nth ts i) q >>= fun ti ->
   M.return (Term.Fun(fi,[ti]))
;;

let of_pos rule pl pr = 
 M.map (context_of_term (Rule.lhs rule)) pl >>= fun cl ->
 M.map (context_of_term (Rule.rhs rule)) pr >>= fun cr ->
 M.return 
  (Trs.of_list (List.map (uncurry Rule.of_terms) (List.product cl cr)))
;;

let of_rule_x rule x = 
 let (pl,pr) = Rule.map (Term.subterm_pos x) rule in
 match pr with
  | [] -> 
   M.return (Trs.empty,Trs.empty)
  | [p] -> of_pos rule pl [p] >>= fun weak ->
   M.return (Trs.empty,weak)
  | pr -> of_pos rule pl pr >>= fun strict ->
   M.return (strict,Trs.empty)
;;

let merge (s0,w0) (s1,w1) = (Trs.union s0 s1,Trs.union w0 w1);;

let of_rule rule = 
 let xl = List.map Term.make_var (Term.vars (Rule.lhs rule)) in
 let e = M.return (Trs.empty,Trs.empty) in
 List.foldr (fun x -> M.lift2 merge (of_rule_x rule x)) e xl 
;;

(* precondition: left-linear TRS *)
(* Processor *)
let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.is_sp p then 
  let trs = P.get_trs p in
  let e = M.return (Trs.empty,Trs.empty) in
  Trs.fold (fun rule acc -> M.lift2 merge acc (of_rule rule)) e trs 
   >>= fun (strict, weak) ->
  let s = P.get_strategy p and l = P.get_language p in
  Diagram.critical trs >>= fun cds ->
  M.return (Some (p,P.make_crp l s trs strict weak cds) )
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
 F.fprintf fmt "@{<starTrans>";
 P.fprintfx fmt (get_op p) >>= fun _ ->
 List.hd fs fmt            >>= fun _ ->
 M.return(F.fprintf fmt "@}")
;;
