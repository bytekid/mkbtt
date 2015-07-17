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

(*** SUBMODULES **********************************************************)
module type ENTRY_TYPE =
    sig
      type t
      val compare : t -> t -> int
      val to_string : t -> string
    end

module type T =
  sig
    (*** TYPES ***********************************************************)
    type entry
    type t

    (*** VALUES **********************************************************)
    (** {3 Constructors and Destructors} *)
 
    val make : unit -> t
    (** [make ()] returns an empty index. *)

    val insert : t -> (U.Term.t * entry) -> t U.Monad.t
    (** [insert i (t,v)] inserts term [t] associated with value [v] into
    index [i]. *)   

    val delete : t -> (U.Term.t * entry) -> t U.Monad.t
    (** [delete i (t,v)] removes term [t] associated with value [v] from
    index [i]. *)

    (** {3 Retrieval Operations} 
    Note that not all implementations achive perfect filtering, i.e., 
    postprocessing may be required.
    *)

    val variant_candidates :
        t -> U.Term.t -> (entry list) U.Monad.t
    (** [variant_candidates i t] returns all entries associated with a
    term [u] such that [u] is a variant of [t]. *)

    val generalization_candidates :
        t -> U.Term.t -> (entry list) U.Monad.t
    (** [generalization_candidates i t] returns all entries associated with 
    a term [u] such that [t] matches [u]. *)

    val unification_candidates :
        t -> U.Term.t -> (entry list) U.Monad.t
    (** [unification_candidates i t] returns all entries associated with 
    a term [u] such that [t] unifies with [u]. *) 

    val encompassment_candidates :
        t -> U.Term.t -> 
        ((entry * Rewriting.Position.t) list) U.Monad.t
    (** [encompassment_candidates i t] returns a list of pairs [(v,p)] such 
    that entry [v] was inserted with a term [u] and [t|p] matches [u]. *)

    val encompassment_candidates_below_root :
        t -> U.Term.t ->
        ((entry * Rewriting.Position.t) list) U.Monad.t
    (** [encompassment_candidates i t] returns a list of pairs [(v,p)] such 
    that entry [v] was inserted with a term [u], [p] is a position below
    the root and [t|p] matches [u]. *)

    val overlap2_candidates :
        t -> U.Term.t -> 
        ((entry * Rewriting.Position.t) list) U.Monad.t
    (** [overlap2_candidates i t] returns a list of pairs [(v,p)] such 
    that entry [v] was inserted with a term [u] and [u|p] unifies with 
    [t]. *) 

    val overlap1_candidates :
        t -> U.Term.t -> 
        ((entry * Rewriting.Position.t) list) U.Monad.t
    (** [overlap1_candidates i t] returns a list of pairs [(v,p)] such 
    that entry [v] was inserted with a term [u] and [t|p] unifies with 
    [u]. *)

    val overlap1_candidates_below_root :
        t -> U.Term.t ->
        ((entry * Rewriting.Position.t) list) U.Monad.t
    (** [overlap1_candidates i t] returns a list of pairs [(v,p)] such 
    that entry [v] was inserted with a term [u], [p] is a non-root position
    and [t|p] unifies with [u]. *)

    val size : t -> int U.Monad.t
    (** [size i] returns the number of entries in [i]. *)

    val is_empty : t -> bool U.Monad.t
    (** [is_empty i] returns [true] iff there are no entries in [i]. *)
  end

module TermEntry : ENTRY_TYPE

module type TERMINDEX = T with type entry = TermEntry.t

module EntryList :
  functor (Entry : ENTRY_TYPE) ->
    sig
      val lex :
        ('a -> 'b -> int) -> ('c -> 'd -> int) -> 'a * 'c -> 'b * 'd -> int
      val cmp : Entry.t -> Entry.t -> int
      val inter : Entry.t list -> Entry.t list -> Entry.t list
      val union : Entry.t list -> Entry.t list -> Entry.t list
      val add : Entry.t -> Entry.t list -> Entry.t list
      val pair_union :
        (Entry.t * Rewriting.Position.t) list ->
        (Entry.t * Rewriting.Position.t) list -> (Entry.t * Rewriting.Position.t) list
      val diff : Entry.t list -> Entry.t list -> Entry.t list
      val pair_diff :
        (Entry.t * Rewriting.Position.t) list ->
        (Entry.t * Rewriting.Position.t) list -> (Entry.t * Rewriting.Position.t) list
      val mem : Entry.t -> Entry.t list -> bool
      val remove : Entry.t -> Entry.t list -> Entry.t list
      val pair_remove :
        Entry.t * Rewriting.Position.t -> (Entry.t * Rewriting.Position.t) list -> (Entry.t * Rewriting.Position.t) list
      val map_union : ('a -> Entry.t list) -> 'a list -> Entry.t list
      val pair_map_union :
        ('a -> (Entry.t * Rewriting.Position.t) list) -> 'a list -> (Entry.t * Rewriting.Position.t) list
      val equal : Entry.t list -> Entry.t list -> bool
      val pair_equal :
        (Entry.t * Rewriting.Position.t) list -> (Entry.t * Rewriting.Position.t) list -> bool
    end
