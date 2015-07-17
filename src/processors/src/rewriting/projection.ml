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

(*** INCLUDES *****************************************************************)
include Filtering;;

(*** FUNCTIONS ****************************************************************)
let to_filter i = Collapsing i;;
let of_filter = function Collapsing i -> i | _ -> failwith "not a projection";;

(* Constructors *)
let add f = Map.add f <.> to_filter;;
let of_list = List.foldl (flip (uncurry add)) Map.empty;;
let to_list sp = Map.fold (fun f -> cons <.> pair f <.> of_filter) sp [];;
let singleton f = singleton f <.> to_filter;;

(* Search Functions *)
let find f = of_filter <.> find f;;
