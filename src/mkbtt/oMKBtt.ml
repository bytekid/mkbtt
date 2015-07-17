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

(** Inferences for ordered multicompletion with termination tools.
@author Sarah Winkler
@since  2010/11/01 *)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module W  = World;;
module Monad = W.Monad;;
module St = Statistics;;
module N = IndexedNode;;
module NS = NodeState;;

(*** OPENS (2) ***********************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

(* ------------------------------------------------------------------ *)
(*  REWRITE inference                                                  *)
(* ------------------------------------------------------------------ *)


(* Filters out those processes of e which allow to add rule to 
   constraints *)
let filter_eqlabels e r = 
 MKBtt.term_checks_one_dir r e >>= fun e' ->
 St.add_n_ordrewrite_tcalls (List.length e) >>
 St.add_n_ordrewrite_ok (List.length e') >>
 let l, r = U.Rule.to_terms r in
 let l,r,c0,c1,_ = MKBtt.normalize l r e [] [] in
 N.create_constraint (U.Rule.of_terms l r) c0 c1 >>
 return e'
;;

(* ------------------------------------------------------------------ *)
(*  DEDUCE inference                                                  *)
(* ------------------------------------------------------------------ *)

let deduce n =
 let t_start = Unix.gettimeofday () in
 let ls = N.brcec in (* take closed rule and equation labels *)
 MKBtt.bdeduce ls n true >>= fun ns1 ->
 MKBtt.bdeduce ls n false >>= fun ns2 ->
 St.add_t_deduce (Unix.gettimeofday () -. t_start) >>
 return (List.union ns1 ns2)
;;

(* ------------------------------------------------------------------ *)
(*  check for success                                                 *)
(* ------------------------------------------------------------------ *)

(* processes not yet finished are those which occur in open labels *)
let unfinished_processes =
 let add f l n = f n >>= fun l' -> return (List.union l l') in
 NS.open_nodes >>= fun ns -> foldl (add N.reo) [] ns
;;



