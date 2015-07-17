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
(*** OPENS ***************************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

let has_p qs p = List.exists (fun q -> CP.is_prefix_of q p) qs;;

let ostring d o p i s t f =
 project W.M.Term.to_stringm (s, t) >>= fun (s,t) ->
 let map n = List.assoc (fst n) d in
 let s = 
 match fst o with
 | T.Node.Axiom -> 
  Printf.sprintf " cnf(%i, axiom, %s = %s, '%s')" i s t f
 | T.Node.Instance (n,_) ->
  Printf.sprintf
   (" cnf(%i, plain, %s = %s, inference(instance, [status(thm)],[%i]))")
   i s t (map n)
 | T.Node.Deduce (n1, _, n2, _) ->
  Printf.sprintf
  (" cnf(%i, plain, %s = %s, inference(deduce, [status(thm)],[%i, %i]))")
  i s t (map n1) (map n2)
 | T.Node.Rewrite (n1, _, n2, l1, l2) ->
  let rule = if List.mem p l1 then "compose"
   else if has_p l2 p then "collapse/simplify"
   else failwith "strange rewrite2"
  in
   Printf.sprintf
   " cnf(%i, plain, %s = %s, inference(%s, [status(thm)],[%i, %i]))"
   i s t rule (map n1) (map n2)
 in return s
;;

let orient_occurrence p i =
 N.constraints i >>= fun (c0,c1) ->
 if has_p c0 p then return (Some (i, true))
 else if has_p c1 p then return (Some (i, false))
 else return None
;;

let orient_string i occ = 
 let pt = uncurry (flip N.brule) >=> W.M.Rule.to_stringm in
 match occ with
 | None -> return ""
 | Some n' ->  
   pt n' >>= fun rs ->
   return (Printf.sprintf " cnf(%i.1, plain, %s, inference(orient, [status(thm)], [%i]))\n" i rs i)
;;

let occurrence_to_string p f (str,(c,d)) o =
 let i = snd o in
 N.data i >>= fun (s, t) ->
 ostring d o p c s t f >>= fun os ->
 orient_occurrence p i >>= orient_string c >>= fun os' ->
 return (str ^ os ^ "\n" ^ os', (c+1,(i,c)::d))
;;

let occurrence_has p o =
 match o with
  | T.Node.Axiom -> true
  | T.Node.Instance(_,ps) -> has_p ps p
  | T.Node.Deduce (_, _, _, ps) -> has_p ps p
  | T.Node.Rewrite(_, _, _, ps1, ps2) -> (has_p ps1 p) || (has_p ps2 p)
;;

let occurrence_parents = function
 | T.Node.Axiom -> []
 | T.Node.Instance(n,_) -> [fst n]
 | T.Node.Deduce (n1, _, n2, _)
 | T.Node.Rewrite(n1, _, n2, _, _) -> [fst n1; fst n2]
;;

let process_in = function
  | T.Node.Axiom -> failwith "process_in axiom?"
  | T.Node.Deduce (_, _, _, ps) -> List.hd ps
  | T.Node.Rewrite(_, _, _, ps1, ps2) -> List.hd (ps1 @ ps2)
  | T.Node.Instance(_,ps) -> List.hd ps
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
 
let rec smaller_occurrence_for p o = function
  | [] -> o
  | o' :: os ->
   if occurrence_has p o' && is_smaller o' o then
    smaller_occurrence_for p o' os
   else smaller_occurrence_for p o os
;;

let rec some_occurrence_for i p = function
  | [] -> failwith ("Searching for "^(string_of_int i)^"i"^ (CP.to_string p) ^ ": does not occur in node.")
  | o :: os ->
   if occurrence_has p o then o
   else some_occurrence_for i p os
;;

let smallest_occurrence_for p i =
  N.parents i >>= fun ps ->
 try
  let o = some_occurrence_for i p ps in
  return (smaller_occurrence_for p o ps)
 with _ ->
  let o = List.hd ps in
  Format.printf " %i has parents %s\n%!" i (List.to_string string_of_int " " (occurrence_parents o));
  let p = process_in o in
  return (smaller_occurrence_for p o ps)
;;


let rec joint_history_for p os = function
 | [] -> return os
 | n :: ns ->
  smallest_occurrence_for p n >>= fun o ->
  let parents = occurrence_parents o in
  Format.printf "%i has parents %s\n%!" n (List.to_string string_of_int " " parents);
  joint_history_for p ((o, n) :: os) (sort_in parents ns)
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
 lift fst <.> (foldl (occurrence_to_string p f) ("",(1,[])))
;;



