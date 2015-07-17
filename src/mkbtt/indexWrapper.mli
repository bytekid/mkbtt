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

(*** SUBMODULES *********************************************************)
module Make(Entry: TermIndex.ENTRY_TYPE) : sig
 (*** TYPES ***********************************************************)
 type entry = Entry.t
 type t

 (*** VALUES **********************************************************)
 (** {3 Constructors and Destructors} *)
 val make : Completion.options -> t * t
 (** [make o] returns an empty index pair [(ridx, uidx)] of a rewrite
 index [ridx] and a unification index [uidx], depending on the index 
 types fixed in the option record [o]. *)

 val insert : (U.Term.t * entry) -> t -> t U.Monad.t
 (** [insert i (t,v)] inserts term [t] associated with value [v] into
 index [i]. *)

 val delete : (U.Term.t * entry) -> t -> t U.Monad.t
 (** [delete i (t,v)] removes term [t] associated with value [v] from
 index [i]. *)

 (** {3 Retrieval Operations} 
 Note that not all implementations achive perfect filtering, i.e., 
 postprocessing may be required. *)

 val variant_candidates :
  U.Term.t -> t -> (entry list) U.Monad.t
 (** [variant_candidates i t] returns all entries associated with a
 term [u] such that [u] is a variant of [t]. *)

 val generalization_candidates :
  U.Term.t -> t -> (entry list) U.Monad.t
 (** [generalization_candidates i t] returns all entries associated with 
 a term [u] such that [t] matches [u]. *)

 val unification_candidates :
  U.Term.t -> t -> (entry list) U.Monad.t
 (** [unification_candidates i t] returns all entries associated with 
 a term [u] such that [t] unifies with [u]. *)

 val encompassment_candidates :
   U.Term.t -> t -> ((entry * Rewriting.Position.t) list) U.Monad.t
 (** [encompassment_candidates i t] returns a list of pairs [(v,p)] such 
 that entry [v] was inserted with a term [u] and [t|p] matches [u]. *)

 val encompassment_candidates_below_root :
  U.Term.t -> t -> ((entry * Rewriting.Position.t) list) U.Monad.t
 (** [encompassment_candidates i t] returns a list of pairs [(v,p)] such 
 that entry [v] was inserted with a term [u], [p] is a position below
 the root and [t|p] matches [u]. *)

 val overlap2_candidates :
  U.Term.t -> t -> ((entry * Rewriting.Position.t) list) U.Monad.t
 (** [overlap2_candidates i t] returns a list of pairs [(v,p)] such 
 that entry [v] was inserted with a term [u] and [u|p] unifies with 
 [t]. *)

 val overlap1_candidates :
  U.Term.t -> t -> ((entry * Rewriting.Position.t) list) U.Monad.t
 (** [overlap1_candidates i t] returns a list of pairs [(v,p)] such 
 that entry [v] was inserted with a term [u] and [t|p] unifies with 
 [u]. *)

 val overlap1_candidates_below_root :
  U.Term.t -> t -> ((entry * Rewriting.Position.t) list) U.Monad.t
 (** [overlap1_candidates i t] returns a list of pairs [(v,p)] such 
 that entry [v] was inserted with a term [u], [p] is a non-root position
 and [t|p] unifies with [u]. *)

 val size : t -> int U.Monad.t
  (** [size i] returns the number of entries in [i]. *)

 val is_empty : t -> bool U.Monad.t
end
