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

(** Some utility functions for history output.
@author Sarah Winkler
@since  2010/11/12 *)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Term = U.Term;;
module Rule = U.Rule;;
module CP = CompletionProcessx;;
module T = Types;;
module W = World;;
module Monad = W.Monad;;
module N = IndexedNode;;
module O = Types.Overlap;;
(*** OPENS ***************************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

let has_p qs p = List.exists (fun q -> CP.is_prefix_of q p) qs;;

let ostring o p i s t f =
 let s, t = Term.to_string s, Term.to_string t in
 match fst o with
 | T.Node.Axiom -> 
  Printf.sprintf "cnf(%i, axiom, %s = %s, '%s')\n" i s t f
 | T.Node.SAxiom ->
  Printf.sprintf "cnf(%i, S-axiom, %s = %s, '%s')\n" i s t f
 | T.Node.Deduce o ->
  Printf.sprintf
  ("cnf(%i, plain, %s = %s, inference(deduce, [status(thm)],[%i, %i]))\n")
  i s t (fst (O.mom o)) (fst (O.dad o))
 | T.Node.Rewrite o ->
   Printf.sprintf
   "cnf(%i, plain, %s = %s, inference(%s, [status(thm)],[%i, %i]))\n"
   i s t "rewrite" (fst (O.mom o)) (fst (O.dad o))
 | T.Node.Extend (n, _) ->
  Printf.sprintf
  ("cnf(%i, plain, %s = %s, inference(extend, [status(thm)],[%i]))\n")
  i s t (fst n)
;;

let orient_occurrence p i =
 N.constraints i >>= fun (c0,c1) ->
 if has_p c0 p then return (Some (i, true))
 else if has_p c1 p then return (Some (i, false))
 else return None
;;

let orient_string i occ = 
 let pt (i, b) = N.brule b i >>= fun r -> return (Rule.to_string r) in
 match occ with
 | None -> return ""
 | Some n' ->  
   pt n' >>= fun rs ->
   return (Printf.sprintf "cnf(%i.1, plain, %s, inference(orient, [status(thm)], [%i]))\n\n" i rs i)
;;

let occurrence_to_string p f str o =
 let i = snd o in
 N.data i >>= fun (s, t) ->
 let os = ostring o p i s t f in
 orient_occurrence p i >>= orient_string i >>= fun os' ->
 return (str ^ os ^ "\n" ^ os')
;;

let occurrence_has p o =
 match o with
  | T.Node.Axiom
  | T.Node.SAxiom -> true
  | T.Node.Extend (_, ps) -> has_p ps p
  | T.Node.Deduce o
  | T.Node.Rewrite o -> has_p (O.processes o) p
;;

let occurrence_parents = function
 | T.Node.Axiom
 | T.Node.SAxiom -> []
 | T.Node.Deduce o
 | T.Node.Rewrite o -> [fst (O.mom o); fst (O.dad o)]
 | T.Node.Extend (n,_) -> [fst n]
;;

let is_smaller o o' =
 match o with
 | T.Node.Axiom -> true
 | _ ->
  let ps = occurrence_parents o in
  let m = List.fold_left max (List.hd ps) ps in
  List.exists (fun j -> j >= m) (occurrence_parents o')
;;

let rec sort_in unsorted sorted =
 let rec insert x = function
  | [] -> [x]
  | y :: ys when x > y -> x :: y :: ys
  | y :: ys when x=y -> y :: ys
  | y :: ys -> y :: (insert x ys)
 in
 match unsorted with
  | [] -> sorted
  | x :: xs -> sort_in xs (insert x sorted)
;;
 
let rec smaller_occurrence p o = function
  | [] -> o
  | o' :: os ->
   if occurrence_has p o' && is_smaller o' o then
    smaller_occurrence p o' os
   else smaller_occurrence p o os
;;

let rec some_occurrence_for p = function
  | [] -> failwith ( (CP.to_string p) ^ " does not occur in node.")
  | o :: os ->
   if occurrence_has p o then o
   else some_occurrence_for p os
;;

let smallest_occurrence_for p i =
 try
  N.parents i >>= fun ps ->
  let o = some_occurrence_for p ps in
  return (smaller_occurrence p o ps)
 with Not_found -> failwith ((string_of_int i) ^ " not found")
;;

let rec smaller_occurrence o = function
  | [] -> o
  | o' :: os ->
   if is_smaller o' o then
    smaller_occurrence o' os
   else smaller_occurrence o os
;;

let rec some_occurrence = function
  | [] -> failwith ("no occurrence found.")
  | o :: os -> o
;;

let smallest_occurrence i =
 try
  N.parents i >>= fun ps ->
  let o = some_occurrence ps in
  return (smaller_occurrence o ps)
 with Not_found -> failwith ((string_of_int i) ^ " not found")
;;


let rec joint_history os = function
 | [] -> return os
 | n :: ns ->
  smallest_occurrence n >>= fun o ->
  let parents = occurrence_parents o in
  joint_history ((o, n) :: os) (sort_in parents ns)
;;

let as_string p f = 
 foldl (occurrence_to_string p f) ""
;;



