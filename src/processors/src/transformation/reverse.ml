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
let code = "rev";;
let name = "String Reversal Processor";;
let comment = "Reverses the left- and right-hand sides of a SRS.";;
let keywords = ["reverse";"transformation"];;
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
let init _ = flags.help := false;;

(* Destructors *)
let get_ip = fst;;
let get_op = snd;;

(* Processor *)
let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 let (s,w) = P.get_sw p in
 if Trs.is_srs s && Trs.is_srs w then
  let s' = Trs.reverse s and w' = Trs.reverse w in
  if Trs.equal s' s && Trs.equal w' w then None else Some (p,P.set_sw s' w' p) 
 else None
;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.constant;;

(* Compare Functions *)
let equal p q = P.equal (get_op p) (get_op q);;

(* Printers *)
let fprintf fs fmt p =
 F.fprintf fmt "@[<1>%s:@\n" name; P.fprintfm fmt (get_op p) >>= fun _ ->
 F.fprintf fmt "@\n"; List.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 F.fprintf fmt "@{<reverse>"; P.fprintfx fmt (get_op p) >>= fun _ ->
 List.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@}")
;;