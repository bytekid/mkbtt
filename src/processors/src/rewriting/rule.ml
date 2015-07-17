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
include Prelude.Rule;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let return = M.return;;

(* Printers *)
let fprintfx fmt r =
 F.fprintf fmt "@{<rule>@{<lhs>"; Term.fprintfx fmt (lhs r) >>= fun _ ->
 F.fprintf fmt "@}@{<rhs>"; Term.fprintfx fmt (rhs r) >>= fun _ ->
 M.return (F.fprintf fmt "@}@}")
;;

let to_stringx r =
 fprintfx F.str_formatter r >>= (M.return <.> F.flush_str_formatter)
;;

let extend rule =
 let l, r = to_terms rule in
 match l with
  | Term.Var _ -> return None
  | Term.Fun(f,_) ->
   M.is_theory Label.AC f >>= fun f_is_ac ->
   if not f_is_ac then return None
   else
    M.fresh_var >>= fun x ->
    let l,r = Term.Fun(f,[Term.Var x; l]), Term.Fun(f,[Term.Var x; r]) in
    return (Some (of_terms l r))
;;
