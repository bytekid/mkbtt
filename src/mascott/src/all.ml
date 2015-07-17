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

(** Inferences for non-ordered multicompletion with termination tools.
 @author Sarah Winkler
 @since  2011/03/01 *)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module Sub = U.Substitution;;
module Term = U.Term;;
module Rule = U.Rule;;
module W  = World;;
module Monad = W.Monad;;
module N = IndexedNode;;
module NI = NodeTermIndexx;;
module CC = CPCCache;;

(*** OPENS (2) ***********************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)
let inter = List.intersect;;
let union = List.union;;
let diff = List.diff;;
let is_empty = List.is_empty;;

(*** OPENS (2) ***********************************************************)
open Monad;;

let lookup_redundant_processes o = failwith "CCP does not support caching"

let normalize u v =
 let eqn,_ = Equation.oriented_of_terms u v in
 Equation.terms eqn
;;

let considered_for outer p inner =
 W.M.Rulex.narrow (Rule.lhs outer) inner p >>= function
  | Some (u, sigma, inner') ->
   let v = Sub.apply_term sigma (Rule.rhs outer) in
   let u,v = normalize u v in
   N.considered_for u v
  | None -> return []
;;

let output_considered c u outer p inner ps =
 W.M.Term.to_stringm u >>= fun us ->
 W.M.Rule.to_stringm outer >>= fun o ->
 W.M.Rule.to_stringm inner >>= fun i ->
 Format.printf "case (%s) reducing %s:\n CP <%s, %s, %s> already considered for %s\n%!" 
  c us i (Pos.to_string p) o (CompletionProcessx.set_to_string ps);
 return ()
;;

(* rl0 is outer, rl2 inner rule of cp at position p in lhs of rl0.
   rl1 reduces overlapped term u at position q. 
   Two equational proofs connect s=t:
   s <-^epsilon_rl0 u ->^q_rl1 w  (P1)
   w <-^q_rl1 u -> p_rl2 t        (P2)         
   3 cases:
    (a) if q is below p then the CP is not prime -> return true
    (b) if q is below the root, only P1 needs to be checked
     (b1) if P1 is just variable overlap, return true
     (b2) check P1
    (c) if P2 is just variable overlap only P1 needs to be checked
     (c1) if P1 is just variable overlap, return true
     (c2) check P1
    (d) if q is the root, check both 
   Returns process set, subset of ps for which the CP is NOT connected*)
let connected_below ps rl1 q (u,((rl0,_,_),p,(rl2,_,_)),_) =
 let (>) = Pos.(>) in
 if q > p then 
  ((*Format.printf "case (a): CP already considered";*)
  return [])  (* (a) *)
 else if (q > Pos.root) (* (b) *)
      || (not (List.mem p (Term.funs_pos (Rule.lhs rl1)))) then (* (c) *)
   if not (List.mem q (Term.funs_pos (Rule.lhs rl0))) then (* (b1), (c1) *)
    return []
   else (* (b2), (c2) *)
    considered_for rl0 q rl1 >>= fun ps' -> 
    (*output_considered "bc" u rl0 q rl1 ps' >>*)
    return (diff ps ps')
  else (* (d) *)
   considered_for rl0 q rl1 >>= fun ps' ->
   considered_for rl1 p rl2 >>= fun ps'' ->
   (*output_considered "d" u rl0 q rl1 ps' >>
   output_considered "d" u rl1 p rl2 ps'' >>*)
   return (diff ps (inter ps' ps''))
;;


let remove_rule_label overlap ps ((i, b), q) =
 N.brule b i >>= W.M.Rule.rename >>= fun rule ->
 N.brc b i >>= fun ps' -> 
 if is_empty (inter ps ps') then
  return ps
 else 
  connected_below (inter ps ps') rule q overlap >>= fun connected ->
  return (union (diff ps ps') connected)
;;


let compute_nonredundant ((u,o,_) as overlap) pset =
 let usable ((_,_,m),_,(_,_,d)) ((i,_),_) = (fst m <> i) && (fst d <> i) in
 NI.encompassment_candidates u >>= fun matches ->
 let matches = List.filter (usable o) matches in
 foldl (remove_rule_label overlap) pset matches
;;


(* looks up redundant processes in hashtable, and reduced different set
   if entry was found for this overlap. otherwise, redundnats are
   computed and stored. note that due to caching, fewer redundants might
   be returned than actually possible *)
let filter_nonredundant overlap process_set =
 CC.caching_active >>= fun cache ->
 if cache then (
   lookup_redundant_processes overlap >>= function
  | None ->
    compute_nonredundant overlap process_set >>= fun nonred ->
    (*let red = diff process_set nonred in
    if not (is_empty red) then
     CC.store_redundant_for_term (term overlap) red >> return nonred
    else*)
     return nonred
  | Some red -> return (diff process_set red))
 else
  compute_nonredundant overlap process_set
;;
