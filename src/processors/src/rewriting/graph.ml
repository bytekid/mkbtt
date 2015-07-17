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
module R = Rule;;
module T = Util.Triple;;

(*** OPENS ********************************************************************)
open Util;;

(*** INCLUDES *****************************************************************)
include Graph.Make (Rule);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Printers *)
let fprintfm fmt g =
 let fprintfm fmt (n,m) =
  R.fprintfm fmt n >>= fun _ -> Format.fprintf fmt "@ ->@ "; R.fprintfm fmt m
 in
 M.fprintf fprintfm "@\n" fmt (edges g)
;;

let to_stringm g =
 fprintfm F.str_formatter g >>= (M.return <.> F.flush_str_formatter)
;;

let fprintfx fmt g =
 let fprintfx fmt (n,m) =
  F.fprintf fmt "@{<arc>@{<lhs>"; R.fprintfx fmt n >>= fun _ ->
  F.fprintf fmt "@}@{<rhs>"; R.fprintfx fmt m >>= fun _ ->
  M.return (F.fprintf fmt "@}@}")
 in
 F.fprintf fmt "@{<dpGraph>";
 M.iter (fprintfx fmt) (edges g) >>= fun _ ->
 M.return (F.fprintf fmt "@}")
;;

let to_stringx g =
 fprintfx F.str_formatter g >>= (M.return <.> F.flush_str_formatter)
;;
