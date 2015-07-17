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
open Logic;;
open Rewritingx;;

(*** MODULES ******************************************************************)
module F = Format;;
module Fun = Function;;
module M = Monad;;
module N = Number;;
module Map = Map.Make (Fun) (N);;

(*** TYPES ********************************************************************)
type t = Map.t * N.t;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Constructors and Destructors *)
let empty w0 = (Map.empty,w0);;
let add f w = Pair.apply (Map.add f w) id;;
let make ws w0 = List.foldl (flip (uncurry add)) (empty w0) ws;;
let w0 = snd;;

(* Search Functions *)
let weight f = Map.find f <.> fst;;

(* Printers *)
let fprintf fmt wf =
 let rec fprintf fmt = function
  | [] -> ()
  | [(f,i)] -> F.fprintf fmt "w(%a) = %a" Fun.fprintf f N.fprintf i
  | (f,i) :: ((_,j) :: _ as ws) ->
   F.fprintf fmt "w(%a)@ =@ " Fun.fprintf f;
   if N.gt i j then F.fprintf fmt "%a@\n" N.fprintf i;
   fprintf fmt ws
 in
 let compare f g = N.compare (snd g) (snd f) in
 let ws = List.sort compare (Map.to_list (fst wf)) in
 F.fprintf fmt "@[w0@ =@ %a@\n%a@]" N.fprintf (snd wf) fprintf ws
;;

let fprintfm fmt wf =
 let rec fprintfm = function
  | [] -> M.return ()
  | [(f,i)] ->
   F.fprintf fmt "w("; M.fprintf_fun fmt f >>= fun _ ->
   M.return (F.fprintf fmt ")@ =@ %a" N.fprintf i)
  | (f,i) :: ((_,j) :: _ as ws) ->
   F.fprintf fmt "w("; M.fprintf_fun fmt f >>= fun _ ->
   F.fprintf fmt ")@ =@ ";
   if N.gt i j then F.fprintf fmt "%a@\n" N.fprintf i;
   fprintfm ws
 in
 let compare f g = N.compare (snd g) (snd f) in
 let ws = List.sort compare (Map.to_list (fst wf)) in
 F.fprintf fmt "@[w0@ =@ %a@\n" N.fprintf (snd wf); fprintfm ws >>= fun _ ->
 M.return (F.fprintf fmt "@]")
;;
