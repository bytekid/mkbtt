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

(*** SUBMODULES *********************************************************)
module SymbolSubstitution : sig
 type t
end

(*** EXCEPTIONS *********************************************************)
exception Not_renamable;;

(*** VALUES *************************************************************)
val rule_symmetric : U.Rule.t -> bool

val eqns_invariant :
 Equation.t list -> SymbolSubstitution.t -> bool

val trs_invariant :
 U.Trs.t -> SymbolSubstitution.t -> bool

val orientation_symmetric : 
 U.Rule.t -> U.Trs.t -> SymbolSubstitution.t

val cardinalities_match :
   ((Equation.t * (int * int)) list *
   (U.Rule.t * (int * int)) list *
   (U.Rule.t * (int * int)) list) ->
   ((Equation.t * (int * int)) list *
   (U.Rule.t * (int * int)) list *
   (U.Rule.t * (int * int)) list) ->
   bool

val processes_match :
  Types.CompletionProcess.t * 
   ((Equation.t * (int * int)) list * 
   (U.Rule.t * (int * int)) list *
   (U.Rule.t * (int * int)) list) ->
  Types.CompletionProcess.t * 
   ((Equation.t * (int * int)) list * 
   (U.Rule.t * (int * int)) list *
   (U.Rule.t * (int * int)) list) ->
  bool World.Monad.t

val renaming_possible : 
 (Equation.t * (int * int)) list -> bool
