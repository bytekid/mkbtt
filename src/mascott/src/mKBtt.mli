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
val term_checks_one_dir :
 U.Rule.t -> CompletionProcessx.t list -> 
 CompletionProcessx.t list World.Monad.t

val orient : 
 int -> unit World.Monad.t

val rewrite_open : 
 int list -> int list World.Monad.t

val rewrite_open_with :
 int list -> int -> int list World.Monad.t

val rewrite_closed :
 int list -> int list World.Monad.t

val rewrite_closed_with :
 int list -> int -> int list World.Monad.t

val deduce : 
 int -> int list World.Monad.t

val deduce_rewrite : int -> int list World.Monad.t

val unfinished_processes : 
 CompletionProcessx.t list World.Monad.t

val gc : 
 int list -> int list World.Monad.t
