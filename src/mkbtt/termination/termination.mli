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
val trs_terminates : U.Trs.t -> bool World.Monad.t 

val preserved : 
 U.Rule.t * U.Rule.t -> U.Trs.t -> (bool * bool) World.Monad.t

val preserved_by_rule : 
 U.Rule.t -> U.Trs.t -> bool World.Monad.t

val preserved_parallel : 
 U.Trs.t -> U.Trs.t -> (bool * bool) World.Monad.t
