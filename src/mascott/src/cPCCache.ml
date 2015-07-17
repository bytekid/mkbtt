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

(** Auxiliary functions and state for critical pair criteria
@author Sarah Winkler
@since  2010/10/25
*)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module H = Hashtbl;;
module Term = U.Term;;
module Rule = U.Rule;;
module Elogic = U.Elogic;;
module W  = World;;
module Monad = W.Monad;;
module N = IndexedNode;;

(*** OPENS (2) ***********************************************************)
open Monad;;
open World;;

(*** TYPES ***************************************************************)
type overlap =
 U.Term.t *
 ((U.Rule.t * CompletionProcessx.t list * (int * bool)) *
 Rewriting.Position.t *
 (U.Rule.t * CompletionProcessx.t list * (int * bool))) *
 U.Substitution.t

(*** FUNCTIONS ***********************************************************)

(***** cache redundant critical pairs (incomplete but faster) *****)

let get_key t =
 fst (Termx.normalize t)
;;

let lookup_redundant_processes_for_term t =
 W.get_cpc_state >>= fun c ->
 let key = get_key t in
 let r = try H.find c.cpc_cache key with Not_found -> [] in
 return r
;;

let store_redundant_for_term t pset =
 W.get_cpc_state >>= fun c ->
 H.add c.cpc_cache (get_key t) pset;
 return ()
;;

let add_redundant_for_term t pset =
 W.get_cpc_state >>= fun c ->
 lookup_redundant_processes_for_term t >>= fun pset' ->
 let pset = List.union pset pset' in
 H.add c.cpc_cache (get_key t) pset;
 return ()
;;

let caching_active =
 W.get_options >>= fun o -> return (Completion.do_cpc_caching o)
;;

(* auxiliary functions used by multiple CPC implementations *)

let is_encompassment term ((i, b), pos) =
 N.brule b i >>= fun rule ->
 let lhs = Rule.lhs rule in
 W.M.Term.rename lhs >>= fun lhs' ->
 return (Elogic.matches (Term.subterm pos term) lhs')
;;

