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

(** Wrapper for different term indexing techniques
 @author Sarah Winkler
 @since  2009/07/26 *)

(** Provides wrapper functions for term indexing *)

(*** MODULES *************************************************************)
module M = U.Monad;;

open M;;

module Make
    = functor (Entry: TermIndex.ENTRY_TYPE) ->
   struct

module MyDTree = DiscriminationTree.Make(Entry);;
module MyTrie = Trie.Make(Entry);;
module MyNaiveIndex = NaiveIndex.Make(Entry);;
module MyCodeTree = CodeTree.Make(Entry);;
module C = Completion;;
(*** TYPES ***************************************************************)
type entry = Entry.t

type t =    
   DiscTree of MyDTree.t
 | Trie of MyTrie.t
 | Naive of MyNaiveIndex.t
 | CodeTree of MyCodeTree.t

(*** EXCEPTIONS **********************************************************)
exception Not_implemented of string

(*** GLOBALS *************************************************************)
(*** FUNCTIONS ***********************************************************)
let make_one indexoption =
  match indexoption with
   | C.DiscriminationTrees -> DiscTree(MyDTree.make ())
   | C.PathIndexing -> Trie(MyTrie.make ())
   | C.Naive -> Naive(MyNaiveIndex.make ())
   | C.CodeTrees -> CodeTree(MyCodeTree.make ())
;;

let make o = 
 make_one (C.rewrite_index o), make_one (C.unification_index o)
;;

let insert entry = function
  | DiscTree t -> MyDTree.insert t entry >>= fun d -> return (DiscTree d)
  | Trie t -> MyTrie.insert t entry >>= fun d -> return (Trie d)
  | Naive t -> MyNaiveIndex.insert t entry >>= fun d -> return (Naive d)
  | CodeTree t -> MyCodeTree.insert t entry >>= fun d -> return (CodeTree d)
;;

let empty_rindex o = make_one (C.rewrite_index o)

let delete entry = function
  | DiscTree t -> MyDTree.delete t entry >>= fun d -> return (DiscTree d)
  | Trie t -> MyTrie.delete t entry >>= fun d -> return (Trie d)
  | Naive t -> MyNaiveIndex.delete t entry >>= fun d -> return (Naive d)
  | CodeTree t -> MyCodeTree.delete t entry >>= fun d -> return (CodeTree d)
;;

let variant_candidates term = function
  | DiscTree t -> MyDTree.variant_candidates t term
  | Trie t -> MyTrie.variant_candidates t term
  | Naive t -> MyNaiveIndex.variant_candidates t term
  | CodeTree t -> MyCodeTree.variant_candidates t term
;;

let generalization_candidates term = function
  | DiscTree t -> MyDTree.generalization_candidates t term
  | Trie t -> MyTrie.generalization_candidates t term
  | Naive t -> MyNaiveIndex.generalization_candidates t term
  | CodeTree t -> MyCodeTree.generalization_candidates t term
;;

let unification_candidates term = function
  | DiscTree t -> MyDTree.unification_candidates t term
  | Trie t -> MyTrie.unification_candidates t term
  | Naive t -> MyNaiveIndex.unification_candidates t term
  | CodeTree t ->
   raise (Not_implemented "Unification with code trees")
;;

let encompassment_candidates term = function
  | DiscTree t -> MyDTree.encompassment_candidates t term
  | Trie t -> MyTrie.encompassment_candidates t term
  | Naive t -> MyNaiveIndex.encompassment_candidates t term
  | CodeTree t -> MyCodeTree.encompassment_candidates t term
;;

let encompassment_candidates_below_root term = function
  | DiscTree t -> MyDTree.encompassment_candidates_below_root t term
  | Trie t -> MyTrie.encompassment_candidates_below_root t term
  | Naive t -> MyNaiveIndex.encompassment_candidates_below_root t term
  | CodeTree t -> MyCodeTree.encompassment_candidates_below_root t term
;;

let overlap1_candidates_below_root term = function
  | DiscTree t -> MyDTree.overlap1_candidates_below_root t term 
  | Trie t -> MyTrie.overlap1_candidates_below_root t term
  | Naive t -> MyNaiveIndex.overlap1_candidates_below_root t term
  | CodeTree _ -> raise (Not_implemented "Overlaps with code trees")
;;

let overlap1_candidates term = function
  | DiscTree t -> MyDTree.overlap1_candidates t term
  | Trie t -> MyTrie.overlap1_candidates t term
  | Naive t -> MyNaiveIndex.overlap1_candidates t term
  | CodeTree _ -> raise (Not_implemented "Overlaps with code trees")
;;


let overlap2_candidates term = function
  | DiscTree t -> MyDTree.overlap2_candidates t term
  | Trie t -> MyTrie.overlap2_candidates t term
  | Naive t -> MyNaiveIndex.overlap2_candidates t term
  | CodeTree _ -> raise (Not_implemented "Overlaps with code trees")
;;

let size index =
 match index with
  | DiscTree t -> MyDTree.size t
  | Trie t -> raise (Not_implemented "Size for tries")
  | Naive t -> MyNaiveIndex.size t
  | CodeTree t -> raise (Not_implemented "Size for code tree")
;;

let is_empty index = 
 match index with
  | DiscTree t -> MyDTree.is_empty t
  | Trie t -> MyTrie.is_empty t
  | Naive t -> MyNaiveIndex.is_empty t
  | CodeTree t -> MyCodeTree.is_empty t

end
