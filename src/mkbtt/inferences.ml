(* Copyright 2010 Sarah Winkler
 * GNU Lesser General Public License
 *
 * This file is part of MKBtt.
 * 
 * MKBtt is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * MKBtt is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with MKBtt. If not, see <http://www.gnu.org/licenses/>.
 *)

(** Inferences for (ordered) multicompletion with termination tools.
@author Sarah Winkler
@since  2010/11/01 *)

(*** OPENS (1) ***********************************************************)
open Util;;


(*** SUBMODULES **********************************************************)
module C = Completion;;
module W  = World;;
module Monad = W.Monad;;

(*** OPENS ***************************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

let log2 s = (*Format.printf "%s\n" s;*) W.log 2 s

let union = List.union

let deduce n =
 W.get_options >>= fun o -> 
 if (C.is_ordered o) then OMKBtt.deduce n
 else MKBtt.deduce n
;;

let deduce_rewrite n =
 deduce n >>= fun ns ->
 MKBtt.rewrite_open ns >>= fun ns' ->
 MKBtt.gc ns >>= fun ns -> 
 return (union ns ns')
;;

let unfinished_processes =
 W.get_options >>= fun o ->
 if (C.is_ordered o) then OMKBtt.unfinished_processes
 else MKBtt.unfinished_processes
;;

