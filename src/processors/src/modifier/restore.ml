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
type t = (P.t * P.t) * P.t;;

(*** GLOBALS ******************************************************************)
let code = "restore";;
let name = "Restore Modifier";;
let comment = "Restores the original TRS within the given DP problem.";;
let keywords = ["weak rules";"modifier"];;
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
let get_ips = fst;;
let get_fst = fst <.> get_ips;;
let get_snd = snd <.> get_ips;;
let get_op = snd;;

(* Processor *)
let solve fs p q =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 let trs = P.get_trs p and ur = P.get_trs q in
 if P.is_dp p && P.is_dp q && not (Trs.equivalent ur trs) then
  Some ((p,q),P.set_trs trs q)
 (* FIXME in general None should be returned; this however rewquires some
 changes in the ttt2 strategy files *)
 else Some ((p,q),q)
;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.constant;;

(* Compare Functions *)
let equal p q =
 P.equal (get_fst p) (get_fst q) && P.equal (get_snd p) (get_snd q) &&
 P.equal (get_op p) (get_op q)
;;

(* Printers *)
let fprintf fs fmt p =
 F.fprintf fmt "@[<1>%s:@\n" name; P.fprintfm fmt (get_op p) >>= fun _ ->
 F.fprintf fmt "@\n"; List.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 F.fprintf fmt "@{<restore>"; P.fprintfx fmt (get_op p) >>= fun _ ->
 List.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@}")
;;
