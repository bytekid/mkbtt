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
module S = Signature;;

(*** OPENS ********************************************************************)
open Util;;

(*** INCLUDES *****************************************************************)
include Prelude.Monad;;

(*** FUNCTIONS ****************************************************************)
(* Constructors and Destructors *)

let employ e f = get >>= fun s -> try set (f s) with Not_found -> fail e;;
let add_rlab f = employ "not_found" <.> S.add_rlab f;;
let incr_curry = employ "not_found" <.> S.incr_curry;;
let modify_curry f = employ "not_found" <.> S.modify_curry f;;
let modify_height f = employ "not_found" <.> S.modify_height f;;
let modify_rlab f = employ "not_found" <.> S.modify_rlab f;;
let modify_slab f = employ "not_found" <.> S.modify_slab f;;
let employ f = get >>= fun s -> let (x,s) = f s in set s >> return x;;
let set_curry ?(arity = ~-1) f = employ <.> S.set_curry ~arity:arity f;;
let add_int ?(arity = ~-1) f = employ <.> S.add_int ~arity:arity f;;
let set_dp = employ <.> S.set_dp;;
let set_height f = employ <.> S.set_height f;;
let set_rlab f = employ <.> S.set_rlab f;;
let set_qlab f = employ <.> S.set_qlab f;;
let set_slab f = employ <.> S.set_slab f;;
let set_theory f = employ <.> S.set_theory f;;
let find e f x = get >>= Util.catch Not_found (return <.> f x) (fail e);;
let drop_curry = find "not found" S.drop_curry;;
let drop_dp = find "not found" S.drop_dp;;
let drop_height = find "not found" S.drop_height;;
let drop_int = find "not found" S.drop_int;;
let drop_rlab = find "not found" S.drop_rlab;;
let drop_slab = find "not found" S.drop_slab;;
let drop_theory = find "not found" S.drop_theory;;

(* Scan Function *)
let compute f = get >>= (return <.> f);;
let is_curry = compute <.> S.is_curry;;
let is_dp = compute <.> S.is_dp;;
let is_height = compute <.> S.is_height;;
let is_int = compute <.> S.is_int;;
let is_rlab = compute <.> S.is_rlab;;
let is_slab = compute <.> S.is_slab;;
let is_theory th = compute <.> (S.is_theory th);;
let is_some_theory = compute <.> (S.is_some_theory);;

(* Search Functions *)
let get_curry = find "not found" S.get_curry;;
let get_height = find "not found" S.get_height;;
let get_int = find "not found" S.get_int;;
let get_rlab = find "not found" S.get_rlab;;
let get_slab = find "not found" S.get_slab;;
let get_theory = find "not found" S.get_theory;;

(* Printers *)
let fprintfx_fun fmt = update <.> S.fprintfx_fun fmt;;
let fprintfx_var fmt = update <.> S.fprintfx_var fmt;;
let to_stringx_fun = employ <.> S.to_stringx_fun;;
let to_stringx_var = employ <.> S.to_stringx_var;;
