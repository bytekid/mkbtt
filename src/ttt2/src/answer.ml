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

(*** MODULES ******************************************************************)
module F = Format;;

(*** TYPES ********************************************************************)
type status = Status.t = Fail | Open | Terminating | Nonterminating;;
type t = Maybe | No | Yes;;

(*** FUNCTIONS ****************************************************************)
(* Constructors *)
let maybe = Maybe;;
let no = No;;
let yes = Yes;;

let of_status = function
 | Fail -> Maybe
 | Open -> Maybe
 | Terminating -> Yes
 | Nonterminating -> No
;;

(* Properties *)

let is_maybe = function Maybe -> true | _ -> false;;
let is_no = function No -> true | _ -> false;;
let is_yes = function Yes -> true | _ -> false;;

(* Printers *)
let fprintf fmt = function
 | Maybe -> F.fprintf fmt "MAYBE"
 | No -> F.fprintf fmt "NO"
 | Yes -> F.fprintf fmt "YES"
;;

let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;
