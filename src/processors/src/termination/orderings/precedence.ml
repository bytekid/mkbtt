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

(*** OPENS ********************************************************************)
open Util;;
open Rewritingx;;

(*** MODULES ******************************************************************)
module F = Format;;
module Fun = Function;;
module M = Monad;;

(*** INCLUDES *****************************************************************)
include Index.Make (Fun) (Int);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Printers *)
let fprintf fmt p =
 let rec fprintf fmt = function
  | [] -> ()
  | [(f,_)] -> Fun.fprintf fmt f
  | (f,i) :: ((_,j) :: _ as p) ->
   if i = j then F.fprintf fmt "%a@ ~@ %a" Fun.fprintf f fprintf p
   else F.fprintf fmt "%a@ >@ %a" Fun.fprintf f fprintf p
 in
 let p = List.sort (fun f g -> compare (snd g) (snd f)) (to_list p) in
 F.fprintf fmt "@[%a@]" fprintf p
;;

let fprintfm fmt p =
 let rec fprintfm = function
  | [] -> M.return ()
  | [(f,_)] -> M.fprintf_fun fmt f
  | (f,i) :: ((_,j) :: _ as p) ->
   M.fprintf_fun fmt f >>= fun _ ->
   if i = j then F.fprintf fmt "@ ~@ " else F.fprintf fmt "@ >@ ";
   fprintfm p
 in
 let p = List.sort (fun f g -> ~-1 * compare (snd f) (snd g)) (to_list p) in
 F.fprintf fmt "@["; fprintfm p >>= fun _ ->
 M.return (F.fprintf fmt "@]")
;;

let fprintfx fmt p =
  F.fprintf fmt "@{<statusPrecedence>";
  M.iter (fun(f,i) ->
    F.fprintf fmt "@{<statusPrecedenceEntry>";
    M.fprintfx_fun fmt f >>= fun _ ->
    M.find_ari f         >>= fun a ->
    F.fprintf fmt "@{<arity>%i@}" a;
    F.fprintf fmt "@{<precedence>%i@}" i;
    F.fprintf fmt "@{<lex>@}";
    M.return(F.fprintf fmt "@}")
  ) (to_list p) >>= fun _ ->
  M.return(F.fprintf fmt "@}")
;;
