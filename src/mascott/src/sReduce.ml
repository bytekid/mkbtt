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

(** Critical pair criterion S-reducibility
@author Sarah Winkler
@since  2011/06/26
*)
(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module C = Completion;;
module St = Statistics;;
module O = Types.Overlap;;
module W  = World;;
module Monad = W.Monad;;
module Term = U.Term;;
module N = IndexedNode;;
module NI = NodeTermIndex;;

(*** OPENS (2) ***********************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

(* filters  processes out of process set for which this cp is redundant.
   rl1 is already instantiated. *)
let is_s_reducible t = 
 if Term.is_var t then return false
 else
  let check ((n,b),p) red = 
   if red then return true else
   (N.brule b n >>= fun rl ->
    W.M.ACRewrite.rewrite_with_at t rl p >>= function
     | [] -> return false
     | t' :: _ -> return true)
  in
  W.M.Termx.funs_pos t >>= fun pos ->
  NI.s_encompassments (t,pos) >>= fun rulenodes ->
  foldr check false rulenodes 
;;

(* looks up redundant processes in hashtable, and reduced different set
   if entry was found for this overlap. otherwise, redundnats are
   computed and stored. note that due to caching, fewer redundants might
   be returned than actually possible *)
let filter_nonredundant o _ ps =
(*  W.M.Term.to_stringm (O.source o) >>= fun ts ->*)
  is_s_reducible (O.source o) >>= fun b ->
 (* Format.printf "%s is reducible:%i\n%!" ts (if b then 1 else 0);*)
  return (if b then [] else ps)
;;

