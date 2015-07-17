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
open Processors;;
open Rewritingx;;

(*** MODULES ******************************************************************)
module F = Format;;
module M = Monad;;

(*** TYPES ********************************************************************)
type t = {problem : Problem.t; proof : Proof.t; status : Status.t};;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Constructors *)
let make problem proof s = {problem = problem; proof = proof; status = s};;
let set_problem p s = {s with problem = p};;
let set_proof p s = {s with proof = p};;
let set_status status s = {s with status = status};;
let update_status status s = set_status (Status.combine s.status status) s;;

(* Access Functions *)
let get_problem s = s.problem;;
let get_proof s = s.proof;;
let get_status s = s.status;;

(* Printers *)
let fprintf fmt s =
 F.fprintf fmt "Problem:@\n"; Problem.fprintfm fmt s.problem >>= fun _ ->
 F.fprintf fmt "@\nProof:@\n"; Proof.fprintf fmt s.proof >>= fun _ ->
 M.return (F.fprintf fmt "@\nStatus:@\n%a" Status.fprintf s.status)
;;

let to_string p =
 fprintf F.str_formatter p >>= (M.return <.> F.flush_str_formatter)
;;
