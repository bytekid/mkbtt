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

(* Writing a wrapper for a predicate:
 * There is one reasons why you have to write a wrapper for your predicate:
 * It is necessary that each predicate has the same signature because
 * otherwise they cannot be combined via the strategy.
 * Writing a wrapper for your predicate is quite easy. Simply extract the
 * problem from the given state and apply your predicate.
 *
 * Register a new predicate:
 * After you have written a wrapper for your predicate, you have to register
 * it by editing the list [predicates] below. Each entry has to be a tripple
 * consisting of the shortcut of the predicate, the wrapper, and the help
 * messages. Important: To increase the readability of the wrapper file, the
 * name of the newly added wrapper should be identical to the shortcut you
 * have chosen for your predicate and registered by editing the list
 * [processor].
 *)

(*** MODULES ******************************************************************)
module M = Monad;;
module S = State;;

(*** OPENS ********************************************************************)
open Util;;
open Processors;;
open Rewritingx;;
open Predicate;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let apply solve = M.get >>= (M.return <.> solve <.> S.get_problem);;
let applym solve = M.get >>= (M.liftr <.> solve <.> S.get_problem);;

(* predicates *)
let applicative = applym <.> Applicative.solve;;
let collapsing = apply <.> Collapsing.solve;;
let constructor = apply <.> Constructor.solve;;
let dummy = apply <.> Dummy.solve;;
let duplicating = apply <.> Duplicating.solve;;
let erasing = apply <.> Erasing.solve;;
let flat = apply <.> Flat.solve;;
let full = apply <.> Full.solve;;
let ground = apply <.> Ground.solve;;
let innermost = apply <.> Innermost.solve;;
let leftGround = apply <.> LeftGround.solve;;
let leftLinear = apply <.> LeftLinear.solve;;
let linear = apply <.> Linear.solve;;
let overlapping = applym <.> Overlapping.solve;;
let overlay = applym <.> Overlay.solve;;
let outermost = apply <.> Outermost.solve;;
let relative = apply <.> Relative.solve;;
let rightGround = apply <.> RightGround.solve;;
let rightLinear = apply <.> RightLinear.solve;;
let shallow = apply <.> Shallow.solve;;
let srs = apply <.> Srs.solve;;
let standard = apply <.> Standard.solve;;
let strongly_nonoverlapping = applym <.> StronglyNonOverlapping.solve;;
let trs = apply <.> Trs.solve;;

(*** GLOBALS ******************************************************************)
let processors = [
 (Applicative.code,applicative,Applicative.help);
 (Collapsing.code,collapsing,Collapsing.help);
 (Constructor.code,constructor,Constructor.help);
 (Dummy.code,dummy,Dummy.help);
 (Duplicating.code,duplicating,Duplicating.help);
 (Erasing.code,erasing,Erasing.help);
 (Flat.code,flat,Flat.help);
 (Full.code,full,Full.help);
 (Ground.code,ground,Ground.help);
 (Innermost.code,innermost,Innermost.help);
 (LeftGround.code,leftGround,LeftGround.help);
 (LeftLinear.code,leftLinear,LeftLinear.help);
 (Linear.code,linear,Linear.help);
 (Overlapping.code,overlapping,Overlapping.help);
 (Overlay.code,overlay,Overlay.help);
 (Outermost.code,outermost,Outermost.help);
 (Relative.code,relative,Relative.help);
 (RightGround.code,rightGround,RightGround.help);
 (RightLinear.code,rightLinear,RightLinear.help);
 (Shallow.code,shallow,Shallow.help);
 (Srs.code,srs,Srs.help);
 (Standard.code,standard,Standard.help);
 (StronglyNonOverlapping.code,strongly_nonoverlapping,StronglyNonOverlapping.help);
 (Trs.code,trs,Trs.help);
];;
