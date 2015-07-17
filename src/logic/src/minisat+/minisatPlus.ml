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

(*** OPENS **************************************************************)
open Bigarray;;

(*** EXTERNAL ***********************************************************)
external init : int -> int -> int -> unit = 
 "camlidl_accessMinisatP_initSolving"
(** initializes Minisat+ for solving *)

external add_constraint : string list -> int list -> int -> int -> unit =
 "camlidl_accessMinisatP_addConstraint"
(** adds a pseudo-boolean constraint to the Minisat+ solver *)

external solve : unit -> string list =
 "camlidl_accessMinisatP_solve"

(*** TYPES ***************************************************************)

(*** EXCEPTIONS **********************************************************)

(*** SUBMODULES **********************************************************)

(*** GLOBALS *************************************************************)

(*** FUNCTIONS ***********************************************************)

(*** TESTS ***************************************************************)
