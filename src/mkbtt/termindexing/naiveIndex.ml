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

(*** OPENS ********************************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Fun = Rewriting.Function;;
module Pos = Rewriting.Position;;
module Var = Rewriting.Variable;;
module Term = U.Term;;
module Sub = U.Substitution;;
module M = U.Monad;;
module Elogic = U.Elogic;;

open M;;

module Make (*: TermIndex.T with type entry = Entry.t*)
    = functor (Entry: TermIndex.ENTRY_TYPE) ->
   struct

(*** TYPES ***************************************************************)
type entry = Entry.t

type t = (Term.t * Entry.t) list

(*** FUNCTIONS ***********************************************************)
let empty = []

let make () = empty

let insert index (term, value) =
 Term.rename term >>= fun term -> 
 (*Term.to_stringm term >>= fun s ->
 Format.printf "NVindex %s with%s\n" s (Entry.to_string value);*)
 return ((term, value) :: index)
;;

let filter_entry p = List.filter (p <.> snd)

let filter p  = List.filter (p <.> fst)

let delete index (_, value) = 
 return (filter_entry (fun e -> Entry.compare e value <> 0) index)
;;

let variant_candidates index term = 
 let tset = filter (Elogic.is_variant term) index in
 let v = List.map snd tset in
 return v
;;

let generalization_candidates index term =
 let tset = filter (Elogic.matches term) index in
 (*Format.printf "Matches %s: "(Term.to_string term);
 Setx.iter (fun (t, _) -> Format.printf "%s "(Term.to_string t)) tset;
 Format.printf "\n";*)
 return (List.map snd tset)
;;

let strict_generalization_candidates index term =
 let m t = (Elogic.matches term t) && not (Elogic.is_variant term t) in
 let tset = filter m index in
 (*Format.printf "Matches %s: "(Term.to_string term);
 Setx.iter (fun (t, _) -> Format.printf "%s "(Term.to_string t)) tset;
 Format.printf "\n";*)
 return (List.map snd tset)
;;

let unification_candidates index term = 
 let tset = filter (Elogic.are_unifiable term) index in
 return (List.map snd tset)
;;

let encompassment_candidates_below_root index term =
 let pos_st = Termx.nonvar_pos_proper_subterms term in (* below root *)
 foldl
  (fun r (t, p) ->
   generalization_candidates index t >>= fun l ->
   return ((List.map (fun n -> (n, p)) l) @ r))
  []
  pos_st >>= fun res ->
 return res
;;

let encompassment_candidates index term =
 strict_generalization_candidates index term >>= fun at_root ->
 encompassment_candidates_below_root index term >>= fun below ->
 let root = flip Pair.make Pos.root in
 return (List.rev_append (List.map root at_root) below)
;;

(* returns (s, p) for entry with term s such that term|p unifies with s
   and term|p is not a variable                                             *)
(* (term plays the role of the lhs of the first rule in an overlap)     *)
let overlap1_candidates index term =
 (* remove eps here: otherwise these critical pairs occur twice *)
 let pos_st = Termx.nonvar_pos_proper_subterms term in
  foldl
   (fun s (t, p) -> 
     unification_candidates index t >>= fun t_unif ->
     return ((List.map (fun n -> (n, p)) t_unif) @ s)
  )
  []
  ((term, Pos.root) :: pos_st) >>= fun res ->
 return res
;;

let overlap1_candidates_below_root index term =
 let pos_st = Termx.nonvar_pos_proper_subterms term in
 foldl
   (fun s (t, p) ->
     unification_candidates index t >>= fun t_unif ->
     return ((List.map (fun n -> (n, p)) t_unif) @ s)
  )
  []
  pos_st >>= fun res ->
 return res
;;

(* return (u, pos) pairs such that u|pos and term are
   unifiable. TODO: avoid u|pos a variable *)
let retrieve_context_unif' term (term2, value) =
 let pset = Term.funs_pos term2 in
 List.fold_left 
  (fun s p -> let t' = Term.subterm p term2 in
             if Elogic.are_unifiable t' term then
              (value, p) :: s
             else
              s
             ) 
  []
  pset 
;;

(* returns (s, p) for entry with term s such that s|p unifies with term
   and s|p is not a variable                                             *)
(* (term plays the role of the lhs of the second rule in an overlap)     *)
let overlap2_candidates index term =
 let res = List.concat (List.map (retrieve_context_unif' term) index) in
 return res
;;

let size index = return (List.length index)

let is_empty i = size i >>= fun s -> return (s=0)

let to_string = 
 List.join 
 (fun (t : (Term.t * Entry.t)) -> Entry.to_string (snd t)) ""
;;

end (* Make *)
