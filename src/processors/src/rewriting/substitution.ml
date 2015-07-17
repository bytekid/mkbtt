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
include Prelude.Substitution;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Printers *)
let fprintfx fmt s =
 let fprintfx fmt s =
  fold (fun x t m ->
   m >>= fun _ ->
   let x = Term.make_var x in
   F.fprintf fmt "<substEntry>"; Term.fprintfx fmt x >>= fun _ ->
   Term.fprintfx fmt t >>= fun _ ->
   M.return (F.fprintf fmt "</substEntry>")) s (M.return ())
 in
 F.fprintf fmt "<substitution>"; fprintfx fmt s >>= fun _ ->
 M.return (F.fprintf fmt "</substitution>")
;;

let to_stringx t =
 fprintfx F.str_formatter t >>= (M.return <.> F.flush_str_formatter)
;;
