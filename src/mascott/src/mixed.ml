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
module T = U.Term;;
module R = U.Rule;;
module E = U.Elogic;;
module ACPos = ACPosition;;
module C = Completion;;
module W  = World;;
module Monad = W.Monad;;
module N = IndexedNode;;
module NI = NodeTermIndex;;
module O = Types.Overlap;;

(*** OPENS (2) ***********************************************************)
open Monad;;
open World;;
(*** FUNCTIONS ***********************************************************)
let inter = List.intersect;;
let union = List.union;;
let diff = List.diff;;
let is_empty = List.is_empty;;
let (>>=) = Monad.(>>=);;
let return = Monad.return;;




let normalize u = (lift fst) <.> W.M.Equation.oriented_of_terms u

let terms_considered ps (u,v) = 
 if List.is_empty ps then return []
 else 
  normalize u v >>= N.considered_for >>= fun ps' ->
  return (diff ps ps')
;;

let considered ps ((ro,po,(io,bo)),tso) ((ri,pi,(ii,bi)),tsi) =
 W.get_node_state >>= fun c ->
 let not_current i = not (List.mem i c.current) in
 if (not_current io) && (not_current ii) then
  return ps
 else
  foldl terms_considered ps (List.product tso tsi) >>= fun left ->
  return (diff ps left)
;;

let output_considered c u outer p inner ps =
 W.M.Term.to_stringm u >>= fun us ->
 W.M.Rule.to_stringm outer >>= fun o ->
 W.M.Rule.to_stringm inner >>= fun i ->
 Format.printf "case (%s) reducing %s:\n CP <%s, %s, %s> already considered for %s\n%!" 
  c us i (Pos.to_string p) o (CompletionProcessx.set_to_string ps);
 return ()
;;

let pos_diff (p,ps) (q,qs) =
 if List.is_empty qs then
  let pl, ql = Pair.map Pos.to_list (p,q) in
  if List.length pl >= (List.length ql) then
   Pos.of_list (List.drop (List.length ql) pl),qs
  else (
  Format.printf "diff %s - %s?\n%!" 
   (ACPos.to_string (p,ps)) (ACPos.to_string (q,qs));
  Pos.root,[] (* should not matter *))
 else
  if Pos.compare p q = 0 then
  Pos.root,List.diff ps qs
 else (
  Format.printf "diff %s - %s?\n%!" 
   (ACPos.to_string (p,ps)) (ACPos.to_string (q,qs));
  Pos.root,[] (* should not matter *))
;;
 

(* rl0 is outer, rl2 inner rule of cp at position p in lhs of rl0.
   rl1 reduces overlapped term u at position q. 
   Two equational proofs connect s=t:
   s <-^epsilon_rl0 u ->^q_rl1 w  (P1)
   w <-^q_rl1 u -> p_rl2 t        (P2)         
   3 cases:
    (a) if q is below p then the CP is not prime -> redundant
    (b) if q is below the root, only P1 needs to be checked
     (b1) if P1 is just variable overlap -> redundant
     (b2) check P1
    (c) if P2 is just variable overlap only P1 needs to be checked
     (c1) if P1 is just variable overlap, return true
     (c2) check P1
    (d) if q is the root, check both 
   Returns process set, subset of ps for which the CP is redundant*)
let connected_below o (s,t) ps (rl1,q,ts,ps',ib) =
 let rl0,rl2 = Triple.fst (O.outer_rule o),Triple.fst (O.inner_rule o) in
 let p = O.position o in
 let (>) = ACPos.(>) in
(* W.M.Rule.to_stringm rl1 >>= fun rl1s ->
 Format.printf " with %s at %s\n%!" rl1s (ACPos.to_string q);*)
(* if q > p then 
  ((*Format.printf " %s > %s\n%!"
   (ACPos.to_string p) (ACPos.to_string q);*)
  return ps')  (* (a) *)
 else*) (* q is above p or parallel to p *)
 (* project W.M.Termx.funs_pos (R.lhs rl0, R.lhs rl1) >>= fun (fpos0, fpos1) -> 
  let p' = pos_diff p q in (* p = q.p' *)
  let u = O.source o in
  if (q <> (ACPos.root u)) (* (b) *)
      || (ACPos.are_parallel p q)
      || (not (List.mem p' fpos1)) then (* (c) *)
   if not (List.mem q fpos0 ) then (* (b1), (c1) *)
    return ps
   else (* (b2), (c2) *)
    considered ps (O.outer_rule o,[t]) ((rl1,ps',ib),ts)
    (*output_considered "bc" u rl0 q rl1 ps' >>*)
  else (* (d) *)
   (*all_considered ps rl0 q rl1 >>= fun ps' ->*)
    considered ps (O.outer_rule o,[t]) ((rl1,ps',ib),ts) >>= fun qs' ->
   (*all_considered ps rl1 p rl2 >>= fun ps'' ->*)
    considered ps((rl1,ps',ib),ts) (O.inner_rule o,[s]) >>= fun qs'' ->
   (*output_considered "d" u rl0 q rl1 ps' >>
   output_considered "d" u rl1 p rl2 ps'' >>*)
   return (inter qs' qs'')*)
 return []
;;


(* If set of considered processes ps is [], return empty list.
   Otherwise try to remove some processes by considering reduct. 
   We know that the intersection between ps and ps' is not empty. *)
let remove_rule_label o (s,t) ps ((rule,q,ts,ps',ib) as reduct) =
 if List.is_empty ps then 
  return []
 else
  connected_below o (s,t) (inter ps ps') reduct >>= fun connected ->
  return (diff ps connected)
;;

(* Source term u of overlap o is not S-reducible. 
   Next we find rule nodes that allow to rewrite term u. We filter out
   trivial ones where the reduction corresponds to one of the reductions
   in the overlap, and those where no common processes for the rewrite
   step exist. 
   For the remaining ones reducts are computed*)
let filter_nonconnected o (s,t) ps =
 let u = O.source o in
(* W.M.Term.to_stringm u >>= fun us ->
 Format.printf "rewrite %s\n%!" us;*)
 let usable ((i,b),q) =
(* position matching may throw away too many *)
  N.brc b i >>= fun ps' ->
  return (not (List.is_empty (inter ps ps')))
 in
 let collect ((i,b),q) =
  N.brule b i >>= fun rl ->
  N.brc b i >>= fun ps ->
  W.M.ACRewrite.rewrite_with_at u rl q >>= function
   | [] -> return []
   | reducts -> return [(rl,q,reducts,ps,(i,b))]
 in
 W.M.Termx.funs_pos_below_root u >>= fun pos ->
 let pos = List.filter (fun p -> not (ACPos.leq p (O.position o))) pos in
 NI.encompassments (u,pos) >>= filter usable >>= flat_map collect >>=
 foldl (remove_rule_label o (s,t)) ps
;;

(* CP (s,t) was derived from overlap for nonempty set of processes ps.
   Want to return subset ps' of ps for which this CP is not
   redundant. *)
let filter_nonredundant overlap (s,t) ps =
 SReduce.filter_nonredundant overlap (s,t) ps >>= function
  | [] -> return []
  | ps -> 
 Prime.filter_nonredundant overlap (s,t) ps (*>>= function
  | [] -> return []
  | ps -> filter_nonconnected overlap (s,t) ps)*)
;;
