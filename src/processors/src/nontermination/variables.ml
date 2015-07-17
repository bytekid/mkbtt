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
module C = Context;;
module F = Format;;
module M = Monad;;
module P = Problem;;
module S = Substitution;;

(*** TYPES ********************************************************************)
type flags = {help : bool ref};;
type t = P.t * Loop.t;;

(*** GLOBALS ******************************************************************)
let code = "var";;
let name = "Fresh Variable Processor";;
let keywords = ["variable condition";"nontermination"];;
let flags = {help = ref false};;

let comment =
 "Defines a processor which proves nontermination of a given TRS by checking \
  whether a left-hand side is a variable or a right-hand side introduces a \
  fresh variable."
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
let get_loop = snd;;
let get_ip = fst;;

(* Processor *)
let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.is_sp p then
  try
   let r = Trs.find (not <.> Rule.is_rewrite_rule) (P.get_trs p) in
   let l = Rule.lhs r and r = Rule.rhs r in
   let (c,s) = match l with
    | Term.Var x ->  (C.Hole,S.singleton x r)
    | _ ->
     let xs = Term.vars l and ys = Term.vars r in
     let y = List.find (not <.> flip List.mem xs) ys in
     let p = List.hd (Term.var_pos y r) and s = S.singleton y l in
     (C.of_term p (S.apply_term s r),s)
   in
   Some (p,Loop.make [l] c s)
  with Not_found -> None
 else None
;;

(* Compare Functions *)
let equal p q = P.equal (get_ip p) (get_ip q);;

(* Printers *)
let fprintf fs fmt p =
 F.fprintf fmt "@[<1>%s:@ " name; Loop.fprintf fmt (get_loop p) >>= fun _ ->
 F.fprintf fmt "@\n"; List.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 F.fprintf fmt "@{<variableConditionViolated>@}"; List.hd fs fmt
;; 
