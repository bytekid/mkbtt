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
module Fun = Function;;
module M = Monad;;
module Var = Variable;;

(*** OPENS ********************************************************************)
open Prelude;;
open Util;;

(*** MODULES ******************************************************************)
module Make (L : LABEL) = struct
 (*** MODULES *****************************************************************)
 module Con = Context.Make (L);;
 module M = M.Make (L);;
 module T = Term.Make (L);;

 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type term = T.t = Var of Var.t | Fun of Fun.t * term list;;
 type context = Con.t = Hole | More of Fun.t * term list * context * term list;;
 
 (*** INCLUDES ****************************************************************)
 include Replacement.Make (Var) (Term.Make (L));;
 
 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;

 (* Apply Substitutions *)
 let rec apply_term s = function
  | Var x as t -> apply x t s
  | Fun (f,ts) -> Fun (f,List.map (apply_term s) ts)
 ;;
 
 let rec apply_context s = function
  | Hole -> Hole
  | More (f,us,c,vs) ->
   let g = List.map (apply_term s) in More (f,g us,apply_context s c,g vs)
 ;;
 
 (* Properties *)
 let is_renaming s = is_bijective s && for_all (const T.is_var) s;;

 (* Printers *)
 let fprintfm fmt s =
  let fprintfm fmt s =
   foldi (fun i x t m ->
    m >>= fun _ ->
    if i > 0 then F.fprintf fmt "@\n@[" else F.fprintf fmt "@[";
    M.fprintf_var fmt x >>= fun _ ->
    F.fprintf fmt "@ ->@ "; T.fprintfm fmt t >>= fun _ ->
    M.return (F.fprintf fmt "@]")) s (M.return ())
  in
  F.fprintf fmt "@["; fprintfm fmt s >>= fun _ -> M.return (F.fprintf fmt "@]")
 ;;

 let to_stringm s =
  fprintfm F.str_formatter s >>= (M.return <.> F.flush_str_formatter)
 ;; 
end
