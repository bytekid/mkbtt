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
module M = Monad;;

(*** OPENS ********************************************************************)
open Util;;

(*** INCLUDES *****************************************************************)
include Prelude.Context;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Printers *)
let rec fprintfx fmt = function
 | Hole -> M.return (F.fprintf fmt "<box/>")
 | More (f,us,c,vs) ->
  F.fprintf fmt "<funContext>"; M.fprintfx_fun fmt f >>= fun _ ->
  F.fprintf fmt "<before>"; M.iter (Term.fprintfx fmt) us >>= fun _ ->
  F.fprintf fmt "</before>"; fprintfx fmt c >>= fun _ ->
  F.fprintf fmt "<after>"; M.iter (Term.fprintfx fmt) vs >>= fun _ ->
  M.return (F.fprintf fmt "</after></funContext>")
;;

let to_stringx t =
 fprintfx F.str_formatter t >>= (M.return <.> F.flush_str_formatter)
;;
