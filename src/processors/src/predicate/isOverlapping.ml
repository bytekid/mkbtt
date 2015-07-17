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
module F = Format;;
module M = Monad;;

(*** TYPES ********************************************************************)
type flags = {help : bool ref; strict : bool ref; weak : bool ref};;

(*** GLOBALS ******************************************************************)
let code = "overlapping";;
let name = "Overlapping Predicate";;
let keywords = ["is overlapping";"predicate"];;

let comment =
 "Checks if the given problem admits a system that is overlapping."
;;

let flags = {help = ref false; strict = ref false; weak = ref false};;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-s",Arg.Set flags.help,
   "Checks if the rules which should be oriented strictly form an overlapping
    TRS.");
  ("-w",Arg.Set flags.help,
   "Checks if the rules which should be oriented weakly form an overlapping
    TRS.")]
 in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let init _ = flags.help := false; flags.strict := false; flags.weak := false;;

let solve fs =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if !(flags.strict) then Trs.is_overlapping <.> fst <.> Problem.get_sw
 else if !(flags.weak) then Trs.is_overlapping <.> snd <.> Problem.get_sw
 else Problem.existsm Trs.is_overlapping
;;
