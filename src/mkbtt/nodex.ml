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

(** Implementation of nodes for mkbTT.
 @author Sarah Winkler
 @since  2010/08/31 *)

(** Nodes store two terms s,t and five process sets (i.e. int sets). The 
    first two sets contain processes i where s->t (t->s) is in R[i]. The
    third set contains processes i where s=t is in E[i], and the last two
    those where s->t (t->s) is in the constraint system C[i]. 
    Moreover, it contains a list of occurrences so that its origins in
    the completion process can be traced back. *)

(*** OPENS ***************************************************************)
open Util;;
open Types;;

(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module Term = U.Term;;
module Rule = U.Rule;;
module C = Completion;;
module W = World;;
module Monad = W.Monad;;
module CP = CompletionProcessx;;
module SS = SelectionStrategy;;

(*** OPENS ***************************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)
include Types.Node

let log2 s = W.log 2 s

let union = List.union

let inter = List.intersect

let diff = List.diff

let first n = fst (Equation.terms n.data);;
let second n = snd (Equation.terms n.data);;
let data n = Equation.terms n.data
let r0 n = n.r0;;
let r0o n = fst n.r0;;
let r0c n = snd n.r0;;
let r0_all n = union (r0o n) (r0c n)
let r1 n = n.r1;;
let r1o n = fst n.r1;;
let r1c n = snd n.r1;;
let r1_all n = union (r1o n) (r1c n)
let e n = n.e;;
let eo n = fst n.e;;
let ec n = snd n.e;;
let e_all n = union (eo n) (ec n);;
let reo n = union (r0o n) (union (r1o n) (eo n));;
let c0 n = n.c0;;
let c1 n = n.c1;;
let constraints n = n.c0, n.c1;;
let parents n = n.parents;;

let brule b n =
 let l, r = if b then first n, second n else second n,first n in
 Rule.of_terms l r

let brc b n = let r = if b then r0c else r1c in r n

let brcec b n = union (brc b n) (ec n)

let bcontent b n =
 let s, t = data n in 
 if b then s,t,r0 n,r1 n,e n else t,s,r1 n,r0 n,e n
;;

let equation n = Equation.of_terms (first n) (second n)

let content_to_string n =
 let s f n = CP.set_to_string (f n) in
 (Term.to_string (first n))^":"^(Term.to_string (second n))^" "^
 "("^(s r0o n)^", "^(s r0c n)^") "^
 "("^(s r1o n)^", "^(s r1c n)^") "^
 "("^(s eo n)^", "^(s ec n)^") "^
 (s c0 n)^", "^(s c1 n)
 (*(origin_to_string (parents n)) *)
;;

let to_string n = "<"^(content_to_string n)^">"

let origin_to_string os =
 let ostring = function
  | Axiom -> "axiom"
  | Deduce (n1, p, n2,ls) ->
   let ls = CP.set_to_string ls in
   Format.sprintf "Deduce(%i, %s, %i, %s)" (fst n1) (Pos.to_string p) (fst n2) ls
  | Rewrite (n1, _, n2, l1, l2) ->
   let ls1, ls2 = CP.set_to_string l1, CP.set_to_string l2 in
   Format.sprintf "Rewrite(%i:%s, %i:%s)" (fst n1) ls1 (fst n2) ls2
  | Instance (n',ps) -> 
   Format.sprintf "Instance(%i, %s)" (fst n') (CP.set_to_string ps)
 in List.to_string ostring "@,"  os
;;

let to_stringm n =
 let s f n = CP.set_to_string (f n) in
 W.M.Term.to_stringm (first n) >>= fun ss ->
 W.M.Term.to_stringm (second n) >>= fun ts ->
 return ("<"^ss^":"^ts^" "^
 "("^(s r0o n)^", "^(s r0c n)^") "^
 "("^(s r1o n)^", "^(s r1c n)^") "^
 "("^(s eo n)^", "^(s ec n)^") "^
 (s c0 n)^", "^(s c1 n)^">" ^ 
 (origin_to_string (parents n))) 
;;

let all = Util.uncurry union

let sum_of_sizes (s,t) = (Term.size s) + (Term.size t);;
let max_of_sizes (s,t) = max (Term.size s) (Term.size t);;

let sel_size st =
 W.get_options >>= fun o ->
 if C.selection_macro o = Some SS.JapanSum then return (sum_of_sizes st)
 else return (max_of_sizes st)
;;

let add_csizes st ps = sel_size st >>= fun k -> iter (CP.add_csize k) ps;;
let add_esizes st ps = sel_size st >>= fun k -> iter (CP.add_esize k) ps;;
let add_rosizes st ps = sel_size st >>= fun k -> iter (CP.add_rosize k) ps;;
let remove_esizes t p = sel_size t >>= fun k -> iter (CP.remove_esize k) p;;

let add_sizes st es cs = 
 sel_size st >>= fun k -> 
 iter (CP.add_esize k) es >>
 iter (CP.add_csize k) cs
;;

let set_sizes st eadd erem cadd =
 sel_size st >>= fun k -> 
 iter (CP.add_esize k) eadd >>
 iter (CP.remove_esize k) erem >>
 iter (CP.add_csize k) cadd
;;

let adapt_esizes st eold enew =
 sel_size st >>= fun k ->
 iter (CP.add_esize k) (diff enew eold) >>
 iter (CP.remove_esize k) (diff eold enew)
;;

let adapt_rosizes st rold rnew =
 sel_size st >>= fun k ->
 iter (CP.add_rosize k) (diff rnew rold) >>
 iter (CP.remove_rosize k) (diff rold rnew)
;;

let adapt_sizes st e e' c = adapt_esizes st e e' >> add_csizes st c

let create_constraint s t c0 = 
 let n = CP.empty in
 add_csizes (s,t) c0 >>
 return (make s t (n,n) (n,n) (n,n) c0 n [])
;;

let create s t r0 r1 e pt = 
 let n = CP.empty in
 add_esizes (s,t) e >>
 return (make s t (r0,n) (r1,n) (e,n) n n pt)
;;

let adapt_sizes st eold e cold c =
 set_sizes st (diff e eold) (diff eold e) (diff c cold)
;;

(* ADD *)
(* add additional labels as open *)
let merge r0' r1' e' c0' c1' p n =
 let (r0o,r0c),(r1o,r1c),(eo,ec) = r0 n, r1 n, e n in
 let c0, c1, ps = c0 n, c1 n, parents n in
 let r0, r1 = (union r0' r0o,r0c), (union r1' r1o,r1c) in
 let r' = union r0' r1' in
 let r = union (r0_all n) (r1_all n) in
 let e = (union (diff e' r) (diff eo r'), diff ec r') in
 let c0',c1' = union c0' c0,union c1' c1 in
 let s, t = data n in
 adapt_sizes (s,t) (union eo ec) (all e) (union c0 c1) (union c0' c1') >>
 return (make s t r0 r1 e c0' c1' (p@ps))
;;

(* ADD *)
let of_axiom e =
 let s, t = Equation.terms e in
 let n = CP.empty in
 let (s,t,_,_,_) = Equation.normalize s t n n n in
 create s t CP.empty CP.empty CP.initial_set [Axiom]
;;

let of_rule r =
 let s, t = Rule.to_terms r in
 let n = CP.empty in
 add_rosizes (s,t) CP.initial_set >>
 let (s,t,r0,r1,_) = Equation.normalize s t CP.initial_set n n in
 return (make s t (r0,n) (r1,n) (n,n) r0 r1 [Axiom])
;;

(* functions required to match module type ELEMENT *)
let compare n n' = Equation.compare n.data n'.data

let map f n =
 let s, t = data n in
 let r0, r1, e = Pair.map f n.r0, Pair.map f n.r1, Pair.map f n.e in
 make s t r0 r1 e (f (c0 n)) (f (c1 n)) (parents n)
;;

let project_rule f0 f1 p n =  
 if CP.mem p (f0 n) then [brule true n]
 else if CP.mem p (f1 n) then [brule false n]
 else []
;;

let project_rulex f0 f1 p n =
 if CP.mem p (f0 n) then [brule true n, true]
 else if CP.mem p (f1 n) then [brule false n, false]
 else []
;;

let project_eqn f p n = if CP.mem p (f n) then [equation n] else []

let project_r = project_rule r0_all r1_all

let project_rx = project_rulex r0_all r1_all

let project_e =  project_eqn e_all

let project_c = project_rule c0 c1

let project_cx = project_rulex c0 c1

let project_r_closed = project_rule r0c r1c

let project_r_open = project_rule r0o r1o

let project_e_closed =  project_eqn ec 

let project_rx = project_rulex r0_all r1_all

let project_cx = project_rulex c0 c1

let contains_process_open p n =
 CP.mem p (eo n) || CP.mem p (r0o n) || CP.mem p (r1o n)
;;

let er_contains_closed p n =
 CP.mem p (ec n) || CP.mem p (r0c n) || CP.mem p (r1c n)
;;

let update n r0' r1' e' = 
 adapt_esizes (data n) (all n.e) (all e') >> 
 adapt_rosizes (data n) (r0o n) (fst r0') >>
 adapt_rosizes (data n) (r1o n) (fst r1') >>
 return {n with r0 = r0'; r1=r1'; e=e'}
;;

let bupdate b n r0' r1' e' =
 if b then update n r0' r1' e' else update n r1' r0' e'
;;

let update_all n r0' r1' e' c0' c1' =
 update n r0' r1' e' >>= fun n' ->
 add_csizes (data n) (diff (union c0' c1') (union n.c0 n.c1)) >>
 return {n' with c0=c0'; c1=c1'}
;;

let close n =
 update n (CP.empty,r0_all n) (CP.empty,r1_all n) (CP.empty,e_all n)
;;

(* label change according to orient inference.
 - moves (eo n) without lr and rl and s to ec of n, 
 - adds lr and s.0 and r0o to r0c
 - adds rl and s.1 and r1o to r1c
 - adds lr and s.0 to c0
 - adds rl and s.1 to c1  *)
let move_to_r_and_c (lr,rl,s) n =
 let s0, s1 = CP.add_zero s, CP.add_one s in
 (* in some cases, the chosen node is also closed, so at this point 
 already split. thus also have to remove s0 and s1 *)
 let remove = List.foldl1 List.union [lr;rl;s;s0;s1] in
 let ep = [], union (CP.diff (eo n) remove) (ec n) in
 let lr',rl' = union lr s0, union rl s1 in
 let r0p = [], (union (r0_all n) lr') in
 let r1p = [], (union (r1_all n) rl') in
 (if s = [] then return () 
  else remove_esizes (data n) (diff (union s0 s1) (eo n))) >>
 update_all n r0p r1p ep (union (c0 n) lr') (union (c1 n) rl')
;;

let r0_where is_open n = if is_open then r0o n else r0c n

let r1_where is_open n = if is_open then r1o n else r1c n

let e_where is_open n = if is_open then eo n else ec n

let all_labels n = (r0_all n) @ (r1_all n) @ (e_all n) @ (c0 n) @ (c1 n)

(* Checks whether node n subsumes a possible new node with data s, t and 
   labels r0, r1, e. Due to its usage upon creation of a new node, c0 and 
   c1 are not checked. 
   Assumption: s, t is data of n in this order. *)
let subsumes n r0 r1 e =
 (CP.is_subset r0 (r0_all n)) &&
 (CP.is_subset r1 (r1_all n)) &&
 (CP.is_subset e (all_labels n))
;;

(* Auxiliary for the function below *)
let was_already_there ls os =
 let remove ls = function
  | Instance (_, ls') -> diff ls ls'
  | Axiom -> [] (* already considered by all processes *)
  | Deduce (_,_,_,ls') -> diff ls ls'
  | Rewrite(_,_,_,r,e) -> diff ls (union r e)
 in
 List.is_empty (List.fold_left remove ls os)
;;

let considered_for n =
 let add ls = function
  | Instance (_,ls') -> union ls ls' 
  | Axiom -> [] (* already considered by all processes *)
  | Deduce (_,_,_,ls') -> union ls ls'
  | Rewrite(_,_,_,r,e) -> union ls (union r e)
 in
 List.fold_left add [] (parents n)
;;

(* Tries to check whether node n having the same data as a node with the
   given labels already dealt with the processes in these label sets.
   The criterion is used when a node is to be generated; here we check 
   whether r0, r1 and e occur in the node's history (or, faster to check,
   are already contained in the node's constraints.) (In contrast to 
   earlier versions of this function the criterion should be complete now.)*) 
let is_done_with n r0 r1 e =
 let subset = CP.is_subset (r0 @ r1 @ e) ((c0 n) @ (c1 n)) in
 (* second part caused unsoundness cf SK3.24 *)
 subset (*|| (was_already_there (CP.union3 r0 r1 e) (parents n))*)
;;

let for_all p n =
 (p (r0_all n)) && (p (r1_all n)) && (p (e_all n)) &&
 (p (c0 n)) && (p (c1 n))
;;

let has_er_labels uo n = 
 let ps = if uo then reo n else CP.union3 (r0c n) (r1c n) (ec n) in
 not (CP.is_empty ps)
;;

let has_non_empty_labels n = not (for_all CP.is_empty n)

let contains_rule_labels n = 
 (not (List.is_empty (r0_all n))) || (not (List.is_empty (r1_all n)))
;;

let sum_size n = let s, t = data n in Term.size s + (Term.size t) 

let max_size n = let s, t = data n in max (Term.size s) (Term.size t)

let restrict_to_process p = map (fun ps -> inter [p] ps)

let remove_processes ps = map (fun qs -> diff qs ps)

let is_axiom n = List.mem Axiom (parents n)
