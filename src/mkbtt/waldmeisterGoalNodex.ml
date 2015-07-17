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

(**  Waldmeister-like goal nodes. 
 @author Sarah Winkler
 @since  2010/11/10 *)

(*** OPENS ***************************************************************)
open Util;;
open Types;;
(*** SUBMODULES (1) ******************************************************)
module Term = U.Term;;
module CP = CompletionProcessx;;
module W = World;;
module M = W.Monad;;

(*** FUNCTIONS ***********************************************************)
include Types.WaldmeisterGoalNode

let (>>=) = M.(>>=);;
let return = M.return;;
let union = List.union;;
let inter = List.intersect;;
let diff = List.diff;;
let term n = n.term;;
let processes n = n.processes;;
let bprocesses b n = (if b then fst else snd) (processes n);;
let content n = term n, processes n;;
let origins n = n.origins;;
let borigin b n = (if b then fst else snd) (origins n);;

let to_stringm n = 
 W.M.Term.to_stringm (term n) >>= fun ts ->
 let ps1 = CP.set_to_string (fst (processes n)) in
 let ps2 = CP.set_to_string (snd (processes n)) in
 return ("<" ^ ts ^ ", " ^ ps1 ^ ", " ^ ps2 ^ ">")
;;

let create t p0 p1 o0 o1 = make t (p0,p1) (o0,o1)

let add_processes1 (ps, h) n =
 let (ps1, ps2) = processes n in 
 let (h1, h2) = origins n in
 {n with processes = (union ps ps1,ps2); origins = (h :: h1,h2)}
;;

let add_processes2 (ps, h) n =
 let (ps1, ps2) = processes n in
 let (h1, h2) = origins n in
 {n with processes = (ps1,union ps ps2); origins = (h1,h::h2)}
;;

let process_inter n = let p1, p2 = processes n in inter p1 p2

let all_processes n = let p1, p2 = processes n in union p1 p2

let bcontains_prefix b p n = 
 List.exists (fun q -> CP.is_prefix_of q p) (bprocesses b n)
;;

let inter_contains_process p n = List.mem p (process_inter n)

let map f n = {n with processes = Pair.map f (processes n)}

let has_non_empty_processes n =
 let (ps0, ps1) = processes n in 
 not (List.is_empty (union ps0 ps1))
;;

let restrict_to_process p n = map (fun ps -> inter [p] ps) n

let remove_processes ps = map (fun qs -> diff qs ps)

let merge p0' p1' o0' o1' n =
 let p0,p1 = processes n in
 let p0,p1 = union p0 p0', union p1 p1' in
 let o0,o1 = origins n in
 let o0,o1 = o0'@o0,o1'@o1 in
 {n with processes = p0,p1; origins=o0,o1}
;;
