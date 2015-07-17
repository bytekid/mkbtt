(* indexComparison.ml *)
(** Comapre to index implementations for equivalence
@author Sarah Winkler
@since  2008/11/26 *)

(** This module allows to compare different implementations of indexing;
    only for testing purposes *)

(*** MODULES *************************************************************)
module Pos = Rewriting.Position;;
module Rewriting = Processors.Rewritingx;;
module M = U.Monad;;
module Term = U.Term;;
open M;;

module Make = functor (Entry: TermIndex.ENTRY_TYPE) ->
   struct

(*** SUBMODULES **********************************************************)
module Idx1 = DiscriminationTree.Make(Entry);;
module Idx2 = NaiveIndex.Make(Entry);;
module EL = TermIndex.EntryList(Entry);;

(*** TYPES ***************************************************************)

(* maintain two index representations *)
type entry = Entry.t

type t = (Idx1.t) * (Idx2.t)

(*** EXCEPTIONS **********************************************************)
exception Not_implemented of string

(*** FUNCTIONS ***********************************************************)
let make_one o =
 (Idx1.make (), Idx2.make())
;;

let make o = make_one o, make_one o

let flip = Util.flip

let insert entry = apply (flip Idx1.insert entry) (flip Idx2.insert entry)

let delete entry = apply (flip Idx1.delete entry) (flip Idx2.delete entry)

let ren = Termx.fresh_vars

let set_pos_to_string set =
 let add s (t, p) = (Entry.to_string t)^"@"^(Pos.to_string p)^", "^s in
 List.fold_left add "" set
;;

let set_to_string set =
 let add s t = (Entry.to_string t)^", "^s in
 List.fold_left add "" set
;;

let variant_candidates term (i1, i2) =
(* make up for nonlinearity supported only by some techniques *)
 ren term >>= fun term -> 
 Idx1.variant_candidates i1 term >>= fun v1 ->
 Idx2.variant_candidates i2 term >>= fun v2 ->
 Term.to_stringm term >>= fun s ->
 (try 
  assert (EL.equal v1 v2);
  with
  exn ->
  (Format.printf "vars for %s in index1 but not index 2: %s\n" s (set_to_string (EL.diff v1 v2));
  Format.printf "vars for %s in index2 but not index 1: %s\n" s (set_to_string (EL.diff v2 v1));
  Format.printf "vars in index1: %s\n%!" (set_to_string v1);
  Format.printf "vars in index2: %s\n%!" (set_to_string v2);
 raise exn ));
 return v1
;;

let generalization_candidates term (i1, i2) =
(* make up for nonlinearity supported only by some techniques *)
 ren term >>= fun term ->
 Idx1.generalization_candidates i1 term >>= fun v1 ->
 Idx2.generalization_candidates i2 term >>= fun v2 ->
 assert (EL.equal v1 v2);
 return v1
;;

let unification_candidates term (i1, i2) =
(* make up for nonlinearity supported only by some techniques *)
 ren term >>= fun term ->
 Idx1.unification_candidates i1 term >>= fun v1 ->
 Idx2.unification_candidates i2 term >>= fun v2 ->
 assert (EL.equal v1 v2);
 return v1
;;

let encompassment_candidates term (i1, i2) =
(* make up for nonlinearity supported only by some techniques *)
 ren term >>= fun term ->
 Idx1.encompassment_candidates i1 term >>= fun v1 ->
 Idx2.encompassment_candidates i2 term >>= fun v2 ->
 Term.to_stringm term >>= fun s -> 
 (try
   assert (EL.pair_equal v1 v2);
 with
  exn ->
  (Format.printf "encs for %s in index1 but not index 2: %s\n" s (set_pos_to_string (EL.pair_diff v1 v2));
  Format.printf "encs for %s in index2 but not index 1: %s\n" s (set_pos_to_string (EL.pair_diff v2 v1));
  Format.printf "encs in index1: %s\n%!" (set_pos_to_string v1);
  Format.printf "encs in index2: %s\n%!" (set_pos_to_string v2);
 raise exn ));
 return v1
;;

let encompassment_candidates_below_root term (i1, i2) =
(* make up for nonlinearity supported only by some techniques *)
 ren term >>= fun term ->
 Idx1.encompassment_candidates_below_root i1 term >>= fun v1 ->
 Idx2.encompassment_candidates_below_root i2 term >>= fun v2 ->
 assert (EL.pair_equal v1 v2);
 return v1
;;

let overlap2_candidates term (i1, i2) =
 ren term >>= fun term -> (* make linear *)
 Idx1.overlap2_candidates i1 term >>= fun v1 ->
 Idx2.overlap2_candidates i2 term >>= fun v2 ->
(* try*)
  assert (EL.pair_equal v1 v2);
(* with
  exn -> (
  Format.printf "overlaps2 4 %s in index1 but not index 2: %s\n" (Term.to_string term) (set_pos_to_string (EL.pair_diff v1 v2));
  Format.printf "overlaps2 4 %s in index2 but not index 1: %s\n" (Term.to_string term) (set_pos_to_string (EL.pair_diff v2 v1));
 raise exn);*)
 return v1
;;


let overlap1_candidates_below_root term (i1, i2) =
 ren term >>= fun term -> (* make linear *)
 Idx1.overlap1_candidates_below_root i1 term >>= fun v1 ->
 Idx2.overlap1_candidates_below_root i2 term >>= fun v2 ->
 Term.to_stringm term >>= fun s ->
 (try
  assert (EL.pair_equal v1 v2);
 with
  exn ->
  (Format.printf "overlaps1 for %s in index1 but not index 2: %s\n" s (set_pos_to_string (EL.pair_diff v1 v2));
  Format.printf "overlaps1 for %s in index2 but not index 1: %s\n" s (set_pos_to_string (EL.pair_diff v2 v1));
  Format.printf "overlaps1 in index1: %s\n%!" (set_pos_to_string v1);
  Format.printf "overlaps1 in index2: %s\n%!" (set_pos_to_string v2);
 raise exn ));
 return v1
;;

let overlap1_candidates term (i1, i2) =
 ren term >>= fun term -> (* make linear *)
 Idx1.overlap1_candidates i1 term >>= fun v1 ->
 Idx2.overlap1_candidates i2 term >>= fun v2 ->
 (*try*)
  assert (EL.pair_equal v1 v2);
 (*with
  exn ->
  Format.printf "Index 1 is %s\n" (Idx1.to_tikz index1);
(*  Format.printf "Index 2 is %s\n" (Idx2.to_string index2);*)
  Format.printf "overlaps1 4 %s in index1 but not index 2: %s\n" (Term.to_string term) (set_pos_to_string (EL.pair_diff v1 v2));
  Format.printf "overlaps1 4 %s in index2 but not index 1: %s\n" (Term.to_string term) (set_pos_to_string (EL.pair_diff v2 v1));
  Format.printf "overlaps1 in index1: %s\n" (set_pos_to_string v1);
 raise exn
 );*)
 return v1
;;

let check f1 f2 (i1, i2) =
 f1 i1 >>= fun s1 ->
 f2 i2 >>= fun s2 ->
 assert (s1 == s2);
 return s1
;;

let size = check Idx1.size Idx2.size

let is_empty = check Idx1.is_empty Idx2.is_empty 

end (* make *)
