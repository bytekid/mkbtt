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
type t = Fail | Open | Terminating | Nonterminating;;

(*** FUNCTIONS ****************************************************************)
(* Properties *)
let is_fail = function Fail -> true | _ -> false;;
let is_unfinished = function Open -> true | _ -> false;;
let is_terminating = function Terminating -> true | _ -> false;;
let is_nonterminating = function Nonterminating -> true | _ -> false;;
let is_complete = function Terminating | Nonterminating -> true | _ -> false;;
let is_success = not <.> is_fail;;

(* Constructors *)
let fail = Fail;;
let unfinished = Open;;
let terminating = Terminating;;
let nonterminating = Nonterminating;;

(* Miscellaneous *)
let collect s = function
 | Terminating -> s
 | Fail -> if is_nonterminating s then Nonterminating else Fail
 | Open -> if is_terminating s then Open else s
 | Nonterminating -> Nonterminating
;;

let combine s = function
 | Open -> s
 | Terminating -> Terminating
 | Fail -> Fail
 | Nonterminating -> Nonterminating
;;

(* Printers *)
let fprintf fmt = function
 | Fail -> Format.fprintf fmt "Fail"
 | Open -> Format.fprintf fmt "Open"
 | Terminating -> Format.fprintf fmt "Terminating"
 | Nonterminating -> Format.fprintf fmt "Nonterminating"
;;

let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;
