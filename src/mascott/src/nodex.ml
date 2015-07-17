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
module Tx = Termx;;
module W = World;;
module Monad = W.Monad;;
module ACPos = ACPosition;;
module CP = CompletionProcessx;;
module ACLogic = W.M.ACLogic;;
module ACR = ACRewrite;;

(*** OPENS ***************************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)


include Types.Node

let log2 s = W.log 2 s

let union = List.union

let inter = List.intersect

let diff = List.diff

let punion p = union (fst p) (snd p)

let first n = fst (Equation.terms n.data);;
let second n = snd (Equation.terms n.data);;
let data n = Equation.terms n.data

let r0o n = punion (fst n.r0);;
let r0c n = punion (snd n.r0);;
let r0 n = r0o n, r0c n;;
let r0op n = snd (fst n.r0);;
let r0ou n = fst (fst n.r0);;
let r0cp n = snd (snd n.r0);;
let r0cu n = fst (snd n.r0);;
let r0_all n = union (r0o n) (r0c n);;
let r0_allu n = union (r0ou n) (r0cu n);;
let r0_allp n = union (r0op n) (r0cp n);;

let r1o n = punion (fst n.r1);;
let r1c n = punion (snd n.r1);;
let r1 n = r1o n, r1c n;;
let r1op n = snd (fst n.r1);;
let r1ou n = fst (fst n.r1);;
let r1cp n = snd (snd n.r1);;
let r1cu n = fst (snd n.r1);;
let r1_all n = union (r1o n) (r1c n);;
let r1_allu n = union (r1ou n) (r1cu n);;
let r1_allp n = union (r1op n) (r1cp n);;

let e n = n.e;;
let eo n = fst n.e;;
let ec n = snd n.e;;
let e_all n = union (eo n) (ec n);;
let reo n = union (r0o n) (union (r1o n) (eo n));;
let rueo n = union (r0ou n) (union (r1ou n) (eo n));;
let c0 n = n.c0;;
let c1 n = n.c1;;
let constraints n = n.c0, n.c1;;
let label_union n = List.foldl1 union [r0_all n;r1_all n;e_all n;c0 n;c1 n];;
let pos n = n.pos;;
let parents n = n.parents;;

let brule b n =
 let l, r = if b then first n, second n else second n,first n in
 Rule.of_terms l r

let brc b n = let r = if b then r0c else r1c in r n

let brcec b n = union (brc b n) (ec n)

let br0cu b n = if b then r0cu n else r1cu n

let bcontent b n =
 let s, t = data n in 
 if b then s,t,n.r0,n.r1,e n else t,s,n.r1,n.r0,e n
;;

let brewpos b n = (if b then fst else snd) (all_pos n.pos);;
let bdedpos b n = (if b then fst else snd) (nonsymm_pos n.pos);;
let equation n = Equation.of_terms (first n) (second n);;

let content_to_string n =
 let s f n = CP.set_to_string (f n) in
 let sp f n = CP.pset_to_string (f n) in
 let spu fp fu n = CP.set_to_string (fu n)^(sp fp n) in
 (Term.to_string (first n))^":"^(Term.to_string (second n))^" "^
 "("^(spu r0op r0ou n)^", "^(spu r0cp r0cu n)^") "^
 "("^(spu r1op r1ou n)^", "^(spu r1cp r1cu n)^") "^
 "("^(s eo n)^", "^(s ec n)^") "^
 "("^(s c0 n)^", "^(s c1 n)^") "
 (*(origin_to_string (parents n)) *)
;;

let to_string n = "<"^(content_to_string n)^">"

let pos_to_string n =
 let a0, a1 = all_pos n in
 let ns0, ns1 = nonsymm_pos n in
 (ACPos.all_to_string a0) ^ "|" ^ (ACPos.all_to_string ns0) ^ ", " ^
 (ACPos.all_to_string a1) ^ "|" ^ (ACPos.all_to_string ns1)
;;

let to_stringm n =
 let s f n = CP.set_to_string (f n) in
 let sp f n = CP.pset_to_string (f n) in
 let spu fp fu n = CP.set_to_string (fu n)^(sp fp n) in
 W.M.Term.to_stringm (first n) >>= fun ss ->
 W.M.Term.to_stringm (second n) >>= fun ts ->
 return ("<"^ss^":"^ts^" "^
 "("^(spu r0op r0ou n)^", "^(spu r0cp r0cu n)^") "^
 "("^(spu r1op r1ou n)^", "^(spu r1cp r1cu n)^") "^
 "("^(s eo n)^", "^(s ec n)^") "^
 "("^(s c0 n)^", "^(s c1 n)^") "(*^
 "("^(pos_to_string n.pos)^")>")
 (origin_to_string (parents n))*) )
;;

let create s t r0 r1 e (p0,p1) pt = 
 let n = CP.empty in
 make s t (r0,(n,n)) (r1,(n,n)) (e,n) n n (create_pos p0 p1) pt
;;

(* add additional labels as open *)
let merge (r0u',r0p') (r1u',r1p') e' c0' c1' (pos0,pos1) p n =
 (* pick labels of n *)
 let (r0o,r0c),(r1o,r1c),(eo,ec) = n.r0, n.r1, e n in
 let c0, c1, ps = c0 n, c1 n, parents n in
 (* remove already present closed labels of ri' *)
 let diffr0 l = diff l (r0_all n) in
 let diffr1 l = diff l (r1_all n) in
 let r0' = diffr0 r0u', diffr0 r0p' in
 let r1' = diffr1 r1u', diffr1 r1p' in
 (* add to open labels *)
 let pair_union (a,b) (c,d) = union a c, union b d in
 let r0 = (pair_union r0' (fst n.r0),r0c) in
 let r1 = (pair_union r1' (fst n.r1),r1c) in
 (* determine e *)
 let r' = union (punion r0') (punion r1') in
 let r = union (r0_all n) (r1_all n) in
 let e = (union (diff e' r) (diff eo r'), diff ec r') in
 let c0,c1 = union c0' c0,union c1' c1 in
 let (a0,a1),(ns0,ns1) = all_pos n.pos, nonsymm_pos n.pos in
 let (a0,a1) = pair_union (a0,a1) (pos0,pos1) in
 let pos = make_pos (a0,ns0) (a1,ns1) in
 let s, t = data n in
 make s t r0 r1 e c0 c1 pos (p@ps)
;;


let of_axiom e =
 let s, t = Equation.terms e in
 let n = CP.empty in
 project W.M.Termx.funs_pos (s,t) >>= fun pos ->
 return (create s t (n,n) (n,n) CP.initial_set pos [Axiom])
;;

(* functions required to match module type ELEMENT *)
let compare n n' = Equation.compare n.data n'.data

let map_er f n =
 let s, t = data n in
 let r0 = Pair.map (Pair.map f) n.r0 in
 let r1 = Pair.map (Pair.map f) n.r1 in
 let e = Pair.map f n.e in
 make s t r0 r1 e (c0 n) (c1 n) (pos n) (parents n)
;;

let map f n = let n = map_er f n in {n with c0 = f (c0 n); c1 = f (c1 n)}

let project_rule f0 f1 p n =  
 if CP.mem p (f0 n) then [brule true n]
 else if CP.mem p (f1 n) then [brule false n]
 else []
;;

let project_eqn f p n = if CP.mem p (f n) then [equation n] else []

let project_r = project_rule r0_all r1_all

let project_e =  project_eqn e_all

let project_c = project_rule c0 c1

let project_r_closed = project_rule r0c r1c

let project_r_closed_unprotected = project_rule r0cu r1cu

let project_e_closed =  project_eqn ec 

let contains_process_open p n =
 CP.mem p (eo n) || CP.mem p (r0o n) || CP.mem p (r1o n)
;;

let er_contains_closed p n =
 CP.mem p (ec n) || CP.mem p (r0c n) || CP.mem p (r1c n)
;;

let set_pos p n = {n with pos = p} 

let of_s_axiom e =
 let l, r = Rule.to_terms e in
 let n = CP.empty in
 project W.M.Termx.funs_pos (l,r) >>= fun (p0,p1) ->
 let n = create l r (n,CP.initial_set) (n,n) n (p0,p1) [SAxiom] in
 let n = {n with c0 = CP.initial_set} in
 return (set_pos (make_pos (p0,p0) (p1,p1)) n)
;;

(* leave protected labels unchanged *)
let update n r0 r1 e = 
 {n with r0 = r0; r1=r1; e=e}
;;

let bupdate b n r0' r1' e' =
 if b then update n r0' r1' e' else update n r1' r0' e'
;;

let update_all n r0' r1' e' c0' c1' =
 let n' = update n r0' r1' e' in {n' with c0=c0'; c1=c1'}
;;

let close n =
 let e = CP.empty,CP.empty in
 let r0cp,r1cp = r0_allp n, r1_allp n in
 let r0cu,r1cu = r0_allu n, r1_allu n in 
 update n (e,(r0cu,r0cp)) (e,(r1cu,r1cp)) (CP.empty,e_all n)
;;

let close_protected n =
 let e = CP.empty in
 let r0cp,r1cp = r0_allp n, r1_allp n in
 update n ((r0ou n,e),(r0cu n,r0cp)) ((r1ou n,e),(r1cu n,r1cp)) n.e
;;

(* label change according to orient inference.
 - moves (eo n) without lr and rl and s to ec of n, 
 - adds lr and s.0 and r0o to r0c
 - adds rl and s.1 and r1o to r1c
 - adds lr and s.0 to c0
 - adds rl and s.1 to c1  
 - leave protected rewrite labels unchanged *)
let move_to_r_and_c (lr,rl,s) n =
 let ep = [], union (CP.diff (eo n) (CP.union3 lr rl s)) (ec n) in
 let lr' = union lr (CP.add_zero s) in
 let rl' = union rl (CP.add_one s) in
 let r0 = ([],[]), (union (r0_all n) lr', r0cp n) in
 let r1 = ([],[]), (union (r1_all n) rl', r1cp n) in
 update_all n r0 r1 ep (union (c0 n) lr') (union (c1 n) rl')
;;

let r0_where is_open n = if is_open then r0o n else r0c n

let r1_where is_open n = if is_open then r1o n else r1c n

let e_where is_open n = if is_open then eo n else ec n

let all_labels n = (r0_all n) @ (r1_all n) @ (e_all n) @ (c0 n) @ (c1 n)

(* Checks whether node n subsumes a possible new node with data s, t and 
   labels r0, r1, e. Due to its usage upon creation of a new node, c0 and 
   c1 are not checked. 
   Assumption: s, t is data of n in this order. *)
let subsumes n (r0u,r0p) (r1u,r1p) e =
 (CP.is_subset r0p (r0_allp n)) &&
 (CP.is_subset r0u (r0_all n)) &&
 (CP.is_subset r1p (r1_allp n)) &&
 (CP.is_subset r1u (r1_all n)) &&
 (CP.is_subset e (all_labels n))
;;


let sum_size n = let s, t = data n in Term.size s + (Term.size t)

let max_size n = let s, t = data n in max (Term.size s) (Term.size t)

(* Auxiliary for the function below *)
let was_already_there ls os =
 let remove ls = function
  | Axiom
  | SAxiom -> [] (* already considered by all processes *)
  | Deduce o -> diff ls (Types.Overlap.processes o)
  | Rewrite o -> diff ls (Types.Overlap.processes o)
  | Extend(_,ls') -> diff ls ls'
 in
 List.is_empty (List.fold_left remove ls os)
;;
(* Tries to check whether node n having the same data as a node with the
   given labels already dealt with the processes in these label sets.
   The criterion is used when a node is to be generated; here we check 
   whether r0, r1 and e occur in the node's history (or, faster to check,
   are already contained in the node's constraints.) (In contrast to 
   earlier versions of this function the criterion should be complete now.)*) 
let is_done_with n (r0u,r0p) (r1u,r1p) e =
 let subset = CP.is_subset (CP.union3 r0u r1u e) ((c0 n) @ (c1 n)) in
 (* to really check if the node was already treated, need to change origin
    so rather check only for unprotected labels *)
 let already_there = was_already_there (CP.union3 r0u r1u e) (parents n) in
 (List.is_empty (r0p @ r1p)) && (subset || already_there)
;;

let for_all p n =
 (p (r0_all n)) && (p (r1_all n)) && (p (e_all n)) &&
 (p (c0 n)) && (p (c1 n))
;;

let has_non_empty_labels n = not (for_all CP.is_empty n)

let has_non_empty_unprotected_open_labels n =
 not (CP.is_empty (CP.union3 (r0ou n) (r1ou n) (eo n)))
;;

let contains_rule_labels n = 
 (not (List.is_empty (r0_all n))) || (not (List.is_empty (r1_all n)))
;;

let restrict_to_process p = map (fun ps -> inter [p] ps)

let remove_processes ps = map (fun qs -> diff qs ps)

let remove_er ps = map_er (fun qs -> diff qs ps)

(* ude only unprotected labels as extended rules do not have to be extended
 once again *)
let bextend b n =
 let r = br0cu b n in
 if List.is_empty r then return None else
 W.M.Rulex.extend (brule b n) >>= function
 | None -> return None | Some rule -> return (Some(rule, r)) 
;;


let set_deduce_positions n =
 let st, ts = brule true n, brule false n in
 project W.M.Termx.funs_pos (Rule.lhs st,Rule.lhs ts) >>= fun (p0,p1) ->
 let (a0,a1) = all_pos n.pos in
 return (set_pos (make_pos (a0,p0) (a1,p1)) n)
;;

let s_normalize s =
 let t_start = Unix.gettimeofday () in
 W.get_options >>= (return <.> Completion.s_theory) >>= fun th ->
 W.M.Trsx.normal_form s th >>= fun res ->
 Statistics.add_t_srewrite ((Unix.gettimeofday ()) -. t_start) >>
 return res
;;


let order_s_normalize s u r0 r1 e =
 W.M.Equation.oriented_of_terms s u >>= fun (eqn,b) ->
 let s,u = Equation.terms eqn in
 project s_normalize (s,u) >>= fun (s,u) ->
 W.M.Equation.oriented_of_terms s u >>= fun (eqn,b') ->
 let not_flipped = (b && b') || (not b' && not b) in
 let s,u = Equation.terms eqn in
 return (if not_flipped then s,u,r0,r1,e else s,u,r1,r0,e)
;;


let order s u r0 r1 e =
 W.M.Equation.oriented_of_terms s u >>= fun (eqn,b) ->
 let s,u = Equation.terms eqn in
 return (if b then s,u,r0,r1,e else s,u,r1,r0,e)
;;

let is_axiom n = List.mem Axiom (parents n)
