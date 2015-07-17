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
include Prelude.Term;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Labeling and Unlabeling Functions *)
let update_root l = function
 | Var _ as t -> M.return t
 | Fun (f,ts) -> l f >>= (M.return <.> flip make_fun ts)
;;

let rec update_term l = function
 | Var _ as t -> M.return t
 | Fun (f,ts) -> l f >>= (flip M.lift (M.map (update_term l) ts) <.> make_fun)
;;

let label_dp = update_root M.set_dp;;
let label_height = update_term <.> flip M.set_height;;

let unlabel_dp =
 update_root (fun f -> M.ite (M.is_dp f) M.drop_dp M.return f)
;;

let unlabel_height =
 update_term (fun f -> M.ite (M.is_height f) M.drop_height M.return f)
;;

let label_ac = update_root (flip M.set_theory Label.AC);;

(* Printers *)

let rec fprintfx fmt = function
 | Var x ->
  F.fprintf fmt "@{<var>"; M.fprintfx_var fmt x >>= fun _ ->
  M.return (F.fprintf fmt "@}")
 | Fun (f,ts) ->
  F.fprintf fmt "@{<funapp>"; M.fprintfx_fun fmt f >>= fun _ ->
  M.iter (fprintfx_arg fmt) ts >>= fun _ ->
  M.return (F.fprintf fmt "@}")
and fprintfx_arg fmt t =
 F.fprintf fmt "@{<arg>"; fprintfx fmt t >>= fun _ ->
 M.return (F.fprintf fmt "@}")
;;

let to_stringx t =
 fprintfx F.str_formatter t >>= (M.return <.> F.flush_str_formatter)
;;
