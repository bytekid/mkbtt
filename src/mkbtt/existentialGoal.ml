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

(** Data type and functions related to existentially quantified goals 
    in theorem proving.
@author Sarah Winkler
@since  2010/11/13 *)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module Fun = Rewriting.Function;;
module Term = U.Term;;
module Rule = U.Rule;;
module Sub = U.Substitution;;
module Elogic = U.Elogic;;
module CP = CompletionProcessx;;
module C = Completion;;
module T = Types;;
module W = World;;
module Monad = W.Monad;;
module St = Statistics;;
module N = IndexedNode;;
module GS = GoalState;;
module NSA = NodeSet;;
module NS = NodeState;;
module NI = NodeTermIndexx;;

(*** OPENS (2) ***********************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

(* ------------------------------------------------------------------ *)
(*  CREATE goal                                                       *)
(* ------------------------------------------------------------------ *)
let dummy = []

let true_node_id = ref 0

let set = function
 | [c] ->
  (* add true node to node set*)
  GS.equals_x_x_is_true () >>= fun e ->
  NSA.of_axioms [e] >>= fun ns_true ->
  NS.add_open ns_true >>= fun _ -> 
  true_node_id := List.hd ns_true;
  (* create existential goal node from false equation*)
  let s, t = Equation.terms c in  
  GS.equals_s_t_is_false s t >>= fun e ->
  N.of_axiom e >>= fun n ->
  N.close n >>
  let l, r = Equation.terms e in
  let lpos, rpos = Term.funs_pos l, Term.funs_pos r in
  GS.set_egoal [n,(lpos,rpos)]
 | _ -> raise (C.GiveUp "Only single existential goal supported.")
;;

(* ------------------------------------------------------------------ *)
(*  CHECK whether goal is proven                                      *)
(* ------------------------------------------------------------------ *)
let proven_for =
  GS.true_is_false () >>= fun eq ->
  N.node_by_eqn eq >>= function
   | Some (_,n) -> 
    return ((Nodex.e_all n) @ (Nodex.r0_all n) @ (Nodex.r1_all n))
   | None -> return []
;;


let list_to_string f l =
 let rec to_string = function
  | [] -> "]"
  | [x] -> (f x) ^ "]"
  | x :: xs -> (f x) ^ ", " ^ (to_string xs)
 in
 "[" ^ (to_string l)
;;

 let node_to_string (i, (ps, qs)) =
  let to_s = list_to_string Pos.to_string in
  N.to_stringm i >>= fun ns ->
  return ("("^ns ^ " with " ^ (to_s ps) ^ ", " ^ (to_s qs)^")")
;;

let to_stringm = 
 GS.get_egoal >>=
 let add s n = node_to_string n >>= fun s' -> return (s^", "^s') in
 foldl add ""
;;

(********** SOME FUNCTIONS FOR GOAL SETS ******************************)

let union = List.union

let inter = List.intersect

let diff = List.diff

(* add and union for goals - merge basic positions *)
let add_goal gs (node, (p1, p2)) =
 let equal_node = fun (n, _) -> n = node in
 try
  let (node', (p1', p2')) as x = List.find equal_node gs in
  let gs' = List.remove x gs in
  (node, (union p1 p1', union p2 p2')) :: gs'
 with Not_found -> (node, (p1, p2)) :: gs
;;

let gunion = List.foldl add_goal 


(*************************** REWRITING **********************************)
let (<) = Pos.(<)

(* function computing position lists *)
(* determine basic positions *)
let basic_positions bpos p rhs =
 let above = List.filter (fun q -> q < p) bpos in
 let rpos = Term.funs_pos rhs in
 let from_rhs = List.map (fun q -> Pos.append p q) rpos in
 (*Format.printf "Basic positions from %s at %s are %s, %s \n"
  (list_to_string Pos.to_string bpos)
  (Pos.to_string p)
  (list_to_string Pos.to_string not_below)
  (list_to_string Pos.to_string from_rhs);*)
 union above from_rhs
;;

let is_right_of p q = 
 let rec split_common_prefix p q = 
  match p, q with
   | i :: is, j :: js when i = j -> split_common_prefix is js
   | _ -> p, q
 in
 match split_common_prefix (Pos.to_list p) (Pos.to_list q) with
 | [], _ -> false
 | _, [] -> false
 | i :: _, j :: _ -> i > j
;;

let left_right_basic_positions bpos p rhs =
 let pos = List.filter (fun q -> (q < p) && (is_right_of q p)) bpos in
 let rpos = Term.funs_pos rhs in
 let from_rhs = List.map (fun q -> Pos.append p q) rpos in
 (*Format.printf "Basic positions from %s at %s are %s, %s \n"
  (list_to_string Pos.to_string bpos)
  (Pos.to_string p)
  (list_to_string Pos.to_string not_below)
  (list_to_string Pos.to_string from_rhs);*)
 union pos from_rhs
;;

let create s t r0 r1 e pt =
 let create =
  let s,t,r0,r1,e = MKBtt.normalize s t r0 r1 e in 
  GS.false_term () >>= fun f ->
  if Term.compare s f = 0 then
   N.create s t r0 (union r1 e) [] pt
  else if Term.compare t f = 0 then
   N.create s t  (union r0 e) r1 [] pt
  else
   N.create s t r0 r1 e pt
 in
 create >>= fun n -> N.close n >> return n
 (* TODO: index new npde here? *)
;;

(* tries to rewrite node n in orientation bn using node rn in orientation 
   brn at position p. uo is a flag determining whether to rewrite open 
   (true) or closed (false) labels. el projects out equation labels, 
   dependent on rewrite1 or rewrite2. 
   bpos are the (basic/left-to-right basic) positions of n. *)
let rewrite_node_with el (n, bn) bpos ((rn, brn), p) =
 N.bcontent bn n >>= fun (s,t,r0,r1,e) ->
 N.bcontent brn rn >>= fun (l,r,(_,rr),_,_) ->
 (* determine labels (depend on uo) and term of new node *)
 let i (o, c) = inter o rr in
 let r0', e' = i r0, el (i r1) (i e) in
 let pt = T.Node.Rewrite((n, bn), p, (rn, brn), r0', e') in
 W.M.Rulex.rewrite t (Rule.of_terms l r) p >>= function
  | None -> return []
  | Some (u,_,_) ->
   (* if step not effective, return *)
   let rr = union r0' e' in
   if List.is_empty rr then return []
   else
    (* update old node *)
    let d (o, c) = diff o rr,c in
    N.bupdate bn n (d r0) (d r1) (d e) >>
    (* check whether new node is subsumed or to be deleted *)
    let s,u,r0',r1',e' = MKBtt.normalize s u r0' [] e' in
    N.is_not_necessary s u r0' r1' e' >>= fun b -> if b then
     return []
    else
     let (pos1, pos2) = if bn then bpos else Pair.flip bpos in
     let bpos' = (pos1, basic_positions pos2 Pos.root r) in
     create s u r0' r1' e' pt >>= fun n' -> return [n', bpos']
;;

(* Tries to rewrite node n using closed nodes. fs are flags to 
   distinguish between rewrite1 and rewrite2 inferences. The bool b
   distinguishes between the second term (true) or the first term
   (false) rewritten. Returns all newly created nodes *)
let rewrite_node (retrieve, el) (n,b) pos =
 N.brule b n >>= fun rule ->
 retrieve (Rule.rhs rule) >>= fun matches ->
 let add cn m =
  rewrite_node_with el (n,b) pos m >>= fun c -> return (union c cn)
 in
 foldl add [] matches
;;

(* Tries to use a rewrite step to change nodes in ns using closed nodes.
   fs are flags to distinguish between rewrite1 and rewrite2 inferences.
   Return node sets (in, cn) where
   - in are the irreducible nodes of ns and
   - cn are newly created nodes *)
let rewrite_nodes fs ns =
 let add_reducts (irn, cn) (n, bpos) =
  rewrite_node fs (n,true) bpos >>= fun cn_lr ->
  rewrite_node fs (n,false) bpos >>= fun cn_rl ->
  let cn' = union cn_lr cn_rl in
  if List.is_empty cn' then return (union [n,bpos] irn,cn)
  else return (irn,union cn' cn)
 in
 foldl add_reducts ([], []) ns
;;

(* rewrite1 uses generic rewrite function, with appropriate index
   retrieval and equation label filtering parameters *)
let rewrite1 idx =
 let pair_root x = return (x,Pos.root) in
 let retrieve t = NI.variant_candidates_in idx t >>= map pair_root in
 let elabels l2 l3 = l3 in
 rewrite_nodes (retrieve, elabels)
;;

(* rewrite2 uses generic rewrite function, with appropriate index
   retrieval and equation label filtering parameters *)
let rewrite2 idx =
 let retrieve = NI.encompassment_candidates_in idx in
 let elabels l2 l3 = union l2 l3 in
 rewrite_nodes (retrieve, elabels)
;;

let gc = filter (fun (n, _ ) -> N.has_non_empty_labels n)


(* return nodes created in 1 or more steps which are not further 
   reducible *)
let rewrite_fixpoint ns idx =
 let rec rewrite irreducible ns =
  rewrite1 idx ns >>= fun (irred1, created1) ->
  rewrite2 idx ns >>= fun (irred2, created2) ->
  gc (gunion created1 created2) >>= fun created -> (* TODO necessary? *)
  let irreducible = gunion (inter irred1 irred2) irreducible in
  if List.is_empty created then
   return irreducible
  else
   rewrite irreducible created
 in
 rewrite [] ns >>= fun not_further_reducible ->
 return (diff not_further_reducible ns)
;;

(* For the set of nodes ns, return nodes created by rewrite steps.
   Since goal nodes are never oriented, only open labels are reduced. *)
let rewrite_with ns idx =
 let t_start = Unix.gettimeofday () in
 rewrite_fixpoint ns idx >>= fun created_nodes ->
 St.add_t_rewrite ((Unix.gettimeofday ()) -. t_start) >>
 return created_nodes
;;

let rewrite_goal ns = NI.rewrite_index >>= rewrite_fixpoint ns

(*************************** NARROWING **********************************)

let cp_of_overlap (((rl,r0,mom),p,(rl',r0',dad)),bpos) =
 W.M.Rulex.narrow (Rule.lhs rl) rl' p >>= function
  | Some (u, sigma, rule2) ->
   let v = Sub.apply_term sigma (Rule.rhs rl) in
   let e = inter r0 r0' in
   N.is_not_necessary u v [] [] e >>= fun b ->
   if b then return [] else
    if List.is_empty e then return [] else
     let pt = T.Node.Deduce(mom, p, dad, e) in
     let u,v,_,_,_ = MKBtt.normalize u v [] [] e in
     create u v [] [] e pt >>= fun n -> return [n,bpos]
  | None -> return []
;;


let filter_basic_positions outerrule bpos innerrule =
 let l1 = Rule.lhs outerrule in
 let l2 = Rule.lhs innerrule in
 List.filter (fun p -> Elogic.are_unifiable (Term.subterm p l1) l2) bpos
;;

let find_overlaps ((orule,ols,o) as onode) (pos1,pos2) (irule,ils,i) =
 let pos2olp p =
   let pos1' = basic_positions pos1 p (Rule.rhs irule) in
   (onode,p,(irule,ils,i)),(pos1',pos2)
 in
 List.map pos2olp (filter_basic_positions orule pos1 irule)
;;

let bdeduce n b =
 N.brule b n >>= fun innerrule -> 
 GS.get_egoal >>= fun gs ->
 pair (N.brule b, N.brcec b) n >>= fun (ir,il) ->
 let innerrule = (ir,il,(n,b)) in
 let overlaps (g, (pos1, pos2)) =
  pair (N.brule true, N.brcec true) g >>= fun (r,l) ->
  pair (N.brule false, N.brcec false) g >>= fun (r',l') ->
  let o1 = find_overlaps (r,l,(g,true)) (pos1, pos2) innerrule in
  let o2 = find_overlaps (r',l',(g,false)) (pos2, pos1) innerrule in
  return (o1 @ o2)
 in
 let add ols g = overlaps g >>= fun o -> return (o @ ols) in
 foldl add [] gs >>= fun ols ->
 let add_cp cps o = cp_of_overlap o >>= fun c -> return (gunion c cps) in
 foldl add_cp [] ols
;;

let deduce n =
 let t_start = Unix.gettimeofday () in
 bdeduce n true >>= fun ns1 ->
 bdeduce n false >>= fun ns2 ->
 St.add_t_deduce (Unix.gettimeofday () -. t_start) >>
 return (union ns1 ns2)
;;


let rewrite node single_index =
 GS.get_egoal >>= fun gs ->
 rewrite_with gs single_index >>= fun new_goals ->
 gc (gunion new_goals gs) >>= GS.set_egoal >>
 deduce node >>= fun ns ->
 rewrite_goal ns >>= fun ns' ->
 gc (ns' @ ns) >>= fun new_goals ->
 GS.get_egoal >>= fun gs ->
 GS.set_egoal (gunion new_goals gs) >>
(* GS.true_node () >>= fun tn ->*)
 deduce (* tn *) !true_node_id >>= fun new_goals ->
 GS.get_egoal >>= fun gs ->
 GS.set_egoal (gunion new_goals gs)
;;

let decide p =
 let rec rewrite nc =
  proven_for >>= fun ps ->
  if not (List.is_empty ps) then return C.Uns
  else match nc with
   | [] -> raise (C.GiveUp "strange nc list")
   | n :: nc ->
    deduce n >>= rewrite_goal >>= gc >>= fun new_goals ->
    GS.get_egoal >>= fun gs ->
    GS.set_egoal (gunion new_goals gs) >>
    GS.true_node () >>= fun tn ->
    deduce tn >>= fun new_goals ->
    GS.set_egoal (gunion new_goals gs) >>
    rewrite (nc @ [n])
 in
 NS.restrict_to_process p >>
 NS.closed_nodes >>= MKBtt.gc >>= rewrite
;;


let proof_output p =
 GS.true_is_false () >>= N.by_eqn >>= function
  | Some n ->
   History.joint_history [] [n] >>= fun h ->
   W.get_options >>= fun o ->
   History.as_string p (C.filename o) h >>= fun s ->
   return (s, "Goal was proven using narrowing.")
  | None -> (* true=false does not exist *)
   raise (C.GiveUp "true=false not found")
;;

