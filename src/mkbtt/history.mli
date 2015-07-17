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

(*** VALUES **************************************************************)
val sort_in : int list -> int list -> int list

val smallest_occurrence : 
 int -> Nodex.occurrence World.Monad.t

val joint_history :
  (Nodex.occurrence * int) list ->
  int list -> (Nodex.occurrence * int) list World.Monad.t

val as_string :
  CompletionProcessx.t ->
  string -> (Nodex.occurrence * int) list -> string World.Monad.t


