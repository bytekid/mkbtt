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
module Make (R : REWRITING) = struct
 (*** MODULES *****************************************************************)
 module S = R.Signature;;
 module R = R.Monad;;
 module M = Monad.Transformer.State (Status) (R);;

 (*** INCLUDES ****************************************************************)
 include Monad.Make (M);;

 (*** TYPES *******************************************************************)
 type 'a m = 'a R.t;;
 type error = R.error;;
 type state = S.t * Status.t;;

 (*** FUNCTIONS ***************************************************************)
 (* State Modifications *)
 let get = M.liftm R.get >>= fun s -> M.get >>= (return <.> Pair.make s);;
 let set s = M.liftm (R.set (fst s)) >> M.set (snd s);;
 let adopt f = get >>= fun s -> let (x,s) = f s in set s >> return x;;
 let modify f = adopt (Pair.create <.> f);;
 let update f = get >>= (set <.> f);;
 let with_state f m = m >>= fun x -> update f >> return x;;
 
 (* Error Handling *)
 let catch h m = (fun s -> R.catch (flip h s) (m s));;
 let fail e = M.liftm (R.fail e);;

 (* Access Functions *)
 let ap_comb m n =
  let combine = Either.map id (fun ((x,status),s) -> (x,(s,status))) in
  let split = Either.map id (fun (x,(s,status)) -> ((x,status),s)) in
  m >>= fun f -> R.map_comb (split <.> f <.> combine) <.> n
 ;;

 let map_comb f = ap_comb (return f);;
 let liftm = M.liftm;;
 
 (* Evaluation Functions *)
 let eval s m =
  let m = m >>= fun x -> get >>= (return <.> Pair.make x) in
  R.run (fst s) (M.run (snd s) m)
 ;;

 let execute s m =
  Either.map id Pair.flip (R.eval (fst s) (M.execute (snd s) m))
 ;;

 let run s m = R.run (fst s) (M.run (snd s) m);;

 (* Signature Functions *)
 let add_ari f = M.liftm <.> R.add_ari f;;
 let add_fun f a = M.liftm <.> R.add_fun f a;;
 let add_fun_name f = M.liftm <.> R.add_fun_name f;;
 let add_var x = M.liftm <.> R.add_var x;;
 let replace_ari f = M.liftm <.> R.replace_ari f;;
 let replace_fun f a = M.liftm <.> R.replace_fun f a;;
 let replace_fun_name f = M.liftm <.> R.replace_fun_name f;;
 let replace_var x = M.liftm <.> R.replace_var x;;
 let create_fun a = M.liftm <.> R.create_fun a;;
 let create_fun_name = M.liftm <.> R.create_fun_name;;
 let create_var = M.liftm <.> R.create_var;;
 let create_var_name = M.liftm <.> R.create_var_name;;
 let fresh_fun = M.liftm R.fresh_fun;;
 let fresh_var = M.liftm R.fresh_var;;
 let find_ari = M.liftm <.> R.find_ari;;
 let find_fun = M.liftm <.> R.find_fun;;
 let find_fun_name = M.liftm <.> R.find_fun_name;;
 let find_var = M.liftm <.> R.find_var;;
 let find_var_name = M.liftm <.> R.find_var_name;;
 let is_ari a = M.liftm <.> R.is_ari a;;
 let is_defined_fun = M.liftm <.> R.is_defined_fun;;
 let is_defined_var = M.liftm <.> R.is_defined_var;;
 let mem_ari = M.liftm <.> R.mem_ari;;
 let mem_fun = M.liftm <.> R.mem_fun;;
 let mem_fun_name = M.liftm <.> R.mem_fun_name;;
 let mem_var = M.liftm <.> R.mem_var;;
 let mem_var_name = M.liftm <.> R.mem_var_name;;
 let fprintf_fun fmt = M.liftm <.> R.fprintf_fun fmt;;
 let fprintf_var fmt = M.liftm <.> R.fprintf_var fmt;;
 let to_string_fun = M.liftm <.> R.to_string_fun;;
 let to_string_var = M.liftm <.> R.to_string_var;;

 (* Constructors *)
 let add_state = M.update <.> Status.add_state;;

 (* Fresh Symbols *)
 let employ f = M.get >>= fun s -> let (p,s) = f s in M.set s >> return p;;
 let create_state = employ <.> Status.create_state;;
 let fresh_state = employ Status.fresh_state;;
 
 (* Search Functions *)
 let find f = M.get >>= (return <.> f);;
 let find_state n = find (Status.find_state n);; 
 let find_state_name p = find (Status.find_state_name p);; 
end
