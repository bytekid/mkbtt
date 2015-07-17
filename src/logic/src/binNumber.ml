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

open Monad;;
open Util;;
open Formula;;
(*** FUNCTIONS ****************************************************************)
(*include BinNat;; *)
(*include BinInt;; *)
(*include BinRat;;  *)
(*include BinReal;; *)
include BinMinf;;

let cache a = 
 let k = a_id a in
 let spec = a_spec a in
 get >>= fun s -> 
 let tbl = State.get_tbl s in
 (if not (Hashtbl.mem tbl k) then (
   fresh spec >>= fun f -> 
   Hashtbl.add tbl k f;
   return tbl
  ) else return tbl) >>= fun tbl ->
 return (Hashtbl.find tbl k)
;;

let of_arith a = cache a;;
