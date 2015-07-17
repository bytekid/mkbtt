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

(** Auxiliary functions and state monad for node selection
@author Sarah Winkler
@since  2010/10/25
*)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module W = World;;
module Monad = W.Monad;;

(*** OPENS (2) ***********************************************************)
open Monad;;
open World;;

(*** FUNCTIONS ***********************************************************)

let inc_count =
 W.get_choice_state >>= fun c ->
 W.set_choice_state {ccount = c.ccount + 1} >>
 return (c.ccount + 1)
;;

