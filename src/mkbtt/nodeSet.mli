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

(*** VALUES *************************************************************)
val project_e : Nodex.CP.t -> int list -> Equation.t list World.Monad.t
val project_e_with_class : 
 Nodex.CP.t -> int list -> (Equation.t * (int * int)) list World.Monad.t
val project_e_closed : 
 Nodex.CP.t -> int list -> Equation.t list World.Monad.t
val project_r : Nodex.CP.t -> int list -> U.Trs.t World.Monad.t
val project_r_with_class : 
 Nodex.CP.t -> int list -> (U.Rule.t * (int * int)) list World.Monad.t
val project_r_closed : Nodex.CP.t -> int list -> U.Trs.t World.Monad.t
val project_r_open : Nodex.CP.t -> int list -> U.Trs.t World.Monad.t
val project_c : Nodex.CP.t -> int list -> U.Trs.t World.Monad.t
val project_c_with_class : 
 Nodex.CP.t -> int list -> (U.Rule.t * (int * int)) list World.Monad.t
val of_axioms : Equation.t list -> int list World.Monad.t
val of_axioms_strict : U.Trs.t -> int list World.Monad.t
val to_string : int list -> string World.Monad.t
val to_stringm : int list -> string World.Monad.t
