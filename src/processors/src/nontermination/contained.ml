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
let code = "con";;
let name = "Containment Processor";;
let keywords = ["contained";"nontermination"]
let flags = {help = ref false};;

let comment =
 "This processor checks if the given TRS contains a rewrite rule such that \
  the right-hand side contains an instance of the left-hand side."
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

(*** EXCEPTIONS ***************************************************************)
exception Not_contained;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let init _ = flags.help := false;;

(* Destructors *)
let get_loop = snd;;
let get_ip = fst;;

(* Processor *)
let contained r =
 let rec contained = function
  | [] -> raise Not_contained
  | r :: rs ->
   let u = Rule.lhs r and v = Rule.rhs r in
   let rec check = function
    | [] -> contained rs
    | t :: ts ->
     try (r,List.hd (Term.subterm_pos t v),Elogic.match_term t u)
     with Elogic.Not_matchable -> check ts
   in
   check (Term.subterms v)
 in
 contained (Trs.to_list r)
;;

let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.is_sp p then
  try
   let (r,q,s) = contained (P.get_trs p) in
   let c = C.of_term q (Rule.rhs r) in
   Some (p,Loop.make [Rule.lhs r] c s)
  with Not_contained -> None
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
 Loop.fprintfx fmt (get_loop p) (get_ip p) >>= fun _ -> List.hd fs fmt
;; 
