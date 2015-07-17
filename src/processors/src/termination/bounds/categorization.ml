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

module Funs = struct
 type t = Fun.t list;;

 let compare fs = let f = List.sort Fun.compare in compare (f fs) <.> f;;
 let fprintf = List.fprintf Fun.fprintf ",";;
end

module M = Monad;;
module Map = Map.Make (Fun) (Funs);;

(*** TYPES ********************************************************************)
type 'a m = 'a M.t;;
type t = Map.t;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

let add f c =
 M.drop_height f >>= fun g ->
 let fs = try Map.find g c with Not_found -> [] in
 M.return (if List.mem f fs then c else Map.replace g (f::fs) c)
;;

let create n = Map.empty;;

let remove f c =
 M.drop_height f >>= fun g ->
 try M.return (Map.replace g (List.remove f (Map.find g c)) c)
 with Not_found -> M.return c
;;

let category f c =
 M.ite (M.is_height f) M.drop_height M.return f >>= fun g ->
 M.return (try Map.find g c with Not_found -> [])
;;

let funs c = Map.fold (const List.rev_append) c [];;

let find f c =
 let info f = M.drop_height f >>= fun g -> M.lift (pair g) (M.get_height f) in
 M.ite (M.is_height f) info (M.return <.> flip pair 0) f >>= fun (g,h) ->
 let fs = try Map.find g c with Not_found -> [] in
 M.filter (fun f -> M.get_height f >>= (M.return <.> (<=) h)) fs
;;

let copy = id;;
let compare = Map.compare;;
let fprintf = Map.fprintf;;
