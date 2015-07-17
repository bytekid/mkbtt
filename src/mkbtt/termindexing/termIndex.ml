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

(** module type for term indexing
@author Sarah Winkler
@since  2009/06/26 *)

(*** OPENS ***************************************************************)
open Util;;

(*** MODULES *************************************************************)
module Pos = Rewriting.Position;;
module Term = U.Term;;
module M = U.Monad;;

(*** SUBMODULES **********************************************************)
module type ENTRY_TYPE =
    sig
      type t
      val compare : t -> t -> int
      val to_string : t -> string
    end

module type T =
  sig
    type entry
    type t
    val make : unit -> t
    val insert : t -> (Term.t * entry) -> t M.t
    val delete : t -> (Term.t * entry) -> t M.t
    val variant_candidates :
        t -> Term.t -> (entry list) M.t
    val generalization_candidates :
        t -> Term.t -> (entry list) M.t
    val unification_candidates :
        t -> Term.t -> (entry list) M.t
    val encompassment_candidates :
        t -> Term.t -> ((entry * Pos.t) list) M.t
    val encompassment_candidates_below_root :
        t -> Term.t -> ((entry * Pos.t) list) M.t
    val overlap2_candidates :
        t -> Term.t -> ((entry * Pos.t) list) M.t
    val overlap1_candidates :
        t -> Term.t -> ((entry * Pos.t) list) M.t
    val overlap1_candidates_below_root :
        t -> Term.t -> ((entry * Pos.t) list) M.t
    val size : t -> int M.t
    val is_empty : t -> bool M.t
  end

module TermEntry =
  struct
    type t = Term.t
    let compare = Term.compare
    let to_string t = Term.to_string t
  end

module type TERMINDEX = T with type entry = TermEntry.t

module EntryList = functor (Entry: ENTRY_TYPE) ->
 struct
 let lex f g (a, b) (a', b') =
  let c = f a a' in
  if c <> 0 then c
  else g b b'
 ;;

 let cmp = Entry.compare;;
 let inter = List.intersect ~c:cmp;;
 let union = List.union ~c:cmp;;
 let add x = union [x]
 let pair_union = List.union ~c:(lex cmp Pos.compare);;
 let diff = List.diff ~c:cmp;;
 let pair_diff = List.diff ~c:(lex cmp Pos.compare);;
 let mem = List.mem ~c:cmp;;
 let remove = List.remove ~c:cmp;;
 let pair_remove = List.remove ~c:(lex cmp Pos.compare);;
 let map_union f = List.fold_left union [] <.> (List.map f);;
 let pair_map_union f = List.fold_left pair_union [] <.> (List.map f);;
 let equal = List.equal ~c:cmp;;
 let pair_equal = List.equal ~c:(lex cmp Pos.compare);;
 end

