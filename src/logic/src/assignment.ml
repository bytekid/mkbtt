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
module F = Formula;;
module Ass = Map.Make (Util.Int);;
module Pss = Map.Make (P);;

(*** OPENS ********************************************************************)
open Util;;

(*** TYPES ********************************************************************)
type t = Number.t Ass.t * bool Pss.t;;

(*** FUNCTIONS ****************************************************************)
let empty : t = (Ass.empty, Pss.empty)
let add_a k v (ass,pss) : t = (Ass.add (F.a_id (F.unwrap_a k)) v ass, pss);;
let add_p k v (ass,pss) : t = (ass, Pss.add k v pss);;

let find_a k (ass,_) = Ass.find (F.a_id (F.unwrap_a k)) ass;;
let find_p k (_,pss) = Pss.find k pss;;
(*val_of is safe variant of find_p (optimizations might delete variables)*)
let val_of k ass = try find_p k ass with Not_found -> false;;

let fpfa ppf (k,v) = Format.fprintf ppf "%a: %a" 
 Formula.fprintf_a k Number.fprintf v ;;
let fpfp ppf (k,v) = Format.fprintf ppf "%a: %b"
 Formula.fprintf_p k v;;

let fprintf ppf (ass, pss) = 
 let av = Ass.fold (fun k v acc -> (Formula.arith k,v)::acc) ass [] in
 let pv = Pss.fold (fun k v acc -> (k,v)::acc) pss [] in
 Format.fprintf ppf "@[%a@]" (List.fprintf fpfa "@\n") (List.rev av);
 Format.fprintf ppf "@[%a@]" (List.fprintf fpfp "@\n") (List.rev pv);
;;

let negate (ass,pss) =
 let anegate k v = F.(~!) (F.(<=>) (F.arith k) (F.constant v)) in
 let pnegate p v = if v then F.(~!) p else p in
 let av = Ass.fold (fun k v f -> F.(<|>) (anegate k v) f) ass F.bot in
 Pss.fold (fun k v f -> F.(<|>) (pnegate k v) f) pss av
;;
