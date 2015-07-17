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
open Prelude;;
open Util;;

(*** MODULES ******************************************************************)
module E = Monad.Error;;
module U = Util;;

module Make (L : LABEL) = struct
 (*** MODULES *****************************************************************)
 module S = Signature.Make (L);;

 (*** TYPES *******************************************************************)
 type error = string;;
 type label = L.t;;
 
 (*** INCLUDES ****************************************************************)
 include Monad.Transformer.State (S) (E);;
 
 (*** FUNCTIONS ***************************************************************)
 (* Access Functions *)
 let ap_comb m n = m >>= fun f -> E.map_error f <.> n;;
 let map_comb f m = ap_comb (return f) m;;
 
 (* Error Handling *)
 let catch h m = (fun s -> E.catch (flip h s) (m s));;
 let fail e = liftm (E.fail e);;
 
 (* Evaluation Functions *)
 let eval s = E.run <.> eval s;;
 let execute s = E.run <.> execute s;;
 let run s = E.run <.> run s;;
 
 (* Fresh Symbols *)
 let employ f =
  get >>= fun s ->
  try let (x,s) = f s in set s >> return x with Failure e -> fail e
 ;;

 let create_fun a = employ <.> S.create_fun a;;
 let create_fun_name = employ <.> S.create_fun_name;;
 let fresh_fun = employ S.fresh_fun;;
 let create_var = employ <.> S.create_var;;
 let create_var_name = employ <.> S.create_var_name;;
 let fresh_var = employ S.fresh_var;;
 
 (* Constructors and Destructors *)
 let add_ari f = update <.> S.add_ari f;;
 let add_fun f a = update <.> S.add_fun f a;;
 let add_fun_name f = update <.> S.add_fun_name f;;
 let add_var x = update <.> S.add_var x;;
 let label ?(arity = ~-1) f = employ <.> S.label ~arity:arity f;;
 let replace_ari f = update <.> S.replace_ari f;;
 let replace_fun f a = update <.> S.replace_fun f a;;
 let replace_fun_name f = update <.> S.replace_fun_name f;;
 let replace_var x = update <.> S.replace_var x;;

 let replace_lab f l =
  get >>= fun s -> try set (S.replace_lab f l s) with Failure e -> fail e
 ;;

 (* Search Functions *)
 let find e f x = get >>= Util.catch Not_found (return <.> f x) (fail e);;
 let drop_lab p = find "not found" (S.drop_lab p);;
 let find_ari = find "not found" S.find_ari;;
 let find_fun = find "not found" S.find_fun;;
 let find_fun_name = find "not found" S.find_fun_name;;
 let find_lab = find "not found" S.find_lab;;
 let find_var = find "not found" S.find_var;;
 let find_var_name = find "not found" S.find_var_name;;
 let pick_lab p = find "not found" (S.pick_lab p);;
 let search_lab p = find "not found" (S.search_lab p);;
 let unlabel = find "not found" S.unlabel;;
 let compute f = get >>= (return <.> f);; 
 let labs = compute <.> S.labs;;
 let origin = compute <.> S.origin;;
 
 (* Scan Functions *)
 let is_ari n = compute <.> S.is_ari n;;
 let is_defined_fun = compute <.> S.is_defined_fun;;
 let is_defined_var = compute <.> S.is_defined_var;;
 let is_lab p = compute <.> S.is_lab p;;
 let is_labeled = compute <.> S.is_labeled;;
 let is_unlabeled = compute <.> S.is_unlabeled;;
 let mem_ari = compute <.> S.mem_ari;;
 let mem_fun = compute <.> S.mem_fun;;
 let mem_fun_name = compute <.> S.mem_fun_name;;
 let mem_var = compute <.> S.mem_var;;
 let mem_var_name = compute <.> S.mem_var_name;;

 (* Printers *)
 let fprintf_fun fmt = update <.> S.fprintf_fun fmt;;
 let fprintf_var fmt = update <.> S.fprintf_var fmt;;
 let to_string_fun = employ <.> S.to_string_fun;;
 let to_string_var = employ <.> S.to_string_var;;
end
