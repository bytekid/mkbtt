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

(*** MODULES ******************************************************************)
module F = Format;;

(*** OPENS ********************************************************************)
open Util;;

(*** INCLUDES *****************************************************************)
include Prelude.Position;;

(*** FUNCTIONS ****************************************************************)
(* Printers *)
let fprintfx_pos fmt pi = F.fprintf fmt "@{<position>%d@}" (pi+1);;

let fprintfx fmt p =
 F.fprintf fmt "@{<positionInTerm>%a@}" (List.fprintf fprintfx_pos "") (to_list p)
;;

let to_stringx = F.flush_str_formatter <.> fprintfx F.str_formatter;;
