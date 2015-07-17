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

(*** MODULES ******************************************************************)
module F = Format;;
module S = State;;

(*** TYPES ********************************************************************)
type t = {agent : S.t option; violations : S.t list; states : S.t list};;

(*** FUNCTIONS ****************************************************************)
(* Constructors and Destructors *)
let make p vs ps = {agent = p; violations = vs; states = ps};;
let singleton p = make (Some p) [] [p];;

let create ps =
 try
  if List.length ps > 1 then make None ps ps
  else make (Some (List.hd ps)) [] ps
 with Failure _ -> failwith "rhs empty"
;;

let add p r =
 if List.mem p r.states then r else
  let vs = p :: r.violations and ps = p :: r.states in
  make None (Option.fold (flip List.cons vs) vs r.agent) ps
;;

let extend ps r = List.foldl (flip add) r ps;;

let set_agent p r =
 if List.mem p r.states then {r with agent = Some p; violations = []}
 else make (Some p) [] (p :: r.states)
;;

let agent r = r.agent;;
let states r = r.states;;
let violations r = r.violations;;

(* Iterators *)
let fold f r d = List.foldl (flip f) d r.states;;
let iter f r = List.iter f r.states;;

let map f r =
 make (Option.map f r.agent) (List.map f r.violations) (List.map f r.states)
;;

(* Properties *)
let is_det r = Option.is_some r.agent && List.length r.states <= 1;;
let is_quasi_det r = Option.is_some r.agent;;

(* Scan Functions *)
let mem p = List.mem p <.> states;;

(* Search Functions *)
let filter c r =
 let ps = List.filter c r.states in
 if ps = [] then None else
  let p = Option.fold (fun q -> if c q then Some q else None) None r.agent in
  Some (make p (List.filter c r.violations) ps)
;;

let remove p = filter ((<>) p);;

(* Miscellaneous *)
let det r =
 try {r with states = [Option.the r.agent]}
 with Failure _ -> failwith "unknown agent"
;;

let diff r r' = filter (not <.> flip List.mem r'.states) r;;
let inter r r' = filter (flip List.mem r'.states) r;;
let max r = List.foldl1 S.max r.states;;
let min r = List.foldl1 S.min r.states;;
let size r = List.length r.states;;

(* Compare Functions *)
let compare = compare;;
let equal r r' = compare r r' = 0;;

(* Printers *)
let fprintf fmt r =
 let fprintf fmt p =
  if Option.fold ((<>) p) true r.agent then State.fprintf fmt p
  else Format.fprintf fmt "%a*" State.fprintf p
 in
 F.fprintf fmt "@[%a@]" (List.fprintf fprintf ",") (states r)
;;

let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;
