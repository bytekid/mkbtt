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

(*** TYPES ********************************************************************)
type t = State.t;;

(*** FUNCTIONS ****************************************************************)
(* Constructors *)
let init = State.zero;;
let add_state = State.max;;

(* Fresh Symbols *)
let create_state n = let p = State.of_int n in pair p <.> State.max p;;
let fresh_state = Pair.create <.> State.next;;

(* Search Functions *)
let find_state n = const (State.of_int n);;
let find_state_name p = const (State.to_int p);;

(* Miscellaneous *)
let copy = State.copy;;

(* Printers *)
let fprintf = State.fprintf;;
let to_string = State.to_string;;
