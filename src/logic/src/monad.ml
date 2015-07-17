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
open Operators;;

(*** MODULES ******************************************************************)
module F = Formula;;

(*** INCLUDES *****************************************************************)
include Monad.State(State);;

(*** FUNCTIONS ****************************************************************)
let fresh_id = get >>= fun s -> update State.incr_id >> return s.State.id;;
let fresh_bool = fresh_id >>= fun x -> return (F.prop x);;
let fresh_arith spec = fresh_id >>= fun id -> return (F.arith ~spec:(Some spec) id);;
let fresh_arith_none = fresh_id >>= fun id -> return (F.arith id);;

let add_side_constraint f = get >>= fun s ->
 Hashtbl.add s.State.sc f true;
 set s
;;

let get_side_constraint () = get >>= fun s ->
 return (Hashtbl.fold (fun k _ acc -> k <&> acc) s.State.sc F.top)
;;

let get_obits () = get >>= fun s -> return s.State.obits;;
let get_dbits () = get >>= fun s -> return s.State.dbits;;

let use_fresh_bit = function
 | F.Top -> return F.Top
 | F.Bot -> return F.Bot
 | b -> fresh_bool >>= fun f ->
  add_side_constraint (f <<->> b) >>
  return f
;;
