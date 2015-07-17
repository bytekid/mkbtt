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
open Prelude;;
open Util;;

(*** MODULES ******************************************************************)
module Make (R : REWRITING) = struct
 (*** MODULES *****************************************************************)
 module F = R.Function;;
 module T = Term.Make (R);;
 module V = R.Variable;;
 
 (*** TYPES *******************************************************************)
 type term = T.t = Var of V.t | State of State.t | Fun of F.t * term list;;

 (*** INCLUDES ****************************************************************)
 include Replacement.Make (R.Variable) (State);;
 
 (*** FUNCTIONS ***************************************************************)
 (* Apply Substitutions *)
 let rec apply_term s = function
  | Var x as t -> catch Not_found (T.make_state <.> find x) t s
  | State _ as t -> t
  | Fun (f,ts) -> Fun (f,List.map (apply_term s) ts)
 ;;
end
