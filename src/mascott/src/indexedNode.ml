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

(** Maintains a node set. 
 @author Sarah Winkler
 @since  2010/08/31 *)

(** Functions for nodes indexed by an integer *)
(*** OPENS ***************************************************************)
open Util;;
(*** SUBMODULES (1) ******************************************************)
module Term = U.Term;;
module Rule = U.Rule;;
module Eq = Equation;;
module W = World;;
module St = Statistics;;
module IN = W.IntNodeIndex;;
module H = Hashtbl;;
module CP = CompletionProcessx;;
module N = Nodex;;
(*module NI = Types.NodeTermIndex;;*)
module Monad = W.Monad;;
module ACL = W.M.ACLogic;;

(*** OPENS ***************************************************************)
open World;;
open Monad;;

(*** SUBMODULES (2) ******************************************************)

(*** FUNCTIONS ***********************************************************)

let log2 s = W.log 2 s

let union = List.union

let diff = List.diff

let by_id i = 
 W.get_node_pool >>= fun c -> 
 return (try Some (IN.find_elt i c.ids) with Not_found -> None)
;;

let id n = W.get_node_pool >>= fun c -> return (IN.find_key n c.ids)

let by_eqn e = 
 W.get_node_pool >>= fun c -> 
 return (try Some (H.find c.equations e) with Not_found -> None)
;;

let node_by_eqn e =
 by_eqn e >>= function
  | Some i -> 
   by_id i >>= 
   (function Some n -> return (Some (i, n)) | None -> return None)
  | None -> 
   return None
;;

let store n =
 St.inc_n_nodes >>
 W.get_node_pool >>= fun c ->
 let i, ids' = IN.fresh c.ids in
 let ids' = IN.add i n ids' in
 H.add c.equations (N.equation n) i;
 W.set_node_pool {c with ids = ids'}
;;

let restore f i =
 by_id i >>= function
 | Some  n ->
  W.get_node_pool >>= fun c ->
  let n' = f n in
  let ids' = IN.replace i n' c.ids in
  W.set_node_pool {c with ids = ids'}
 | None -> failwith "no id in indexed node restore"
;;

let restorem f i =
 by_id i >>= function
 | Some  n ->
  W.get_node_pool >>= fun c ->
  f n >>= fun n' ->
  let ids' = IN.replace i n' c.ids in
  W.set_node_pool {c with ids = ids'}
 | None -> failwith "no id in indexed node restore"
;;

(* flatten and normalize terms, already S-normalized *)
let create_axiom e =
 let s,t = Eq.terms e in 
 W.M.Equation.oriented_of_terms s t >>= fun (eq,_) ->
 N.of_axiom eq >>= fun n -> store n >> return n
;;

let create_s_axiom r = N.of_s_axiom r >>= fun n -> store n >> return n

let of_axiom = create_axiom >=> id

(* terms assumed to be already normalized *)
let check_create_with fmerge fcreate s t r0 r1 e c0 c1 pos p =
 let eqn = Eq.of_terms s t in
 (* small lemmata *)
 W.get_processes >>= fun pstate ->
 let note = List.foldl1 union [fst r0;snd r0;fst r1;snd r1;c0;c1] in
 let e = if Term.size s + (Term.size t) < 5 then diff pstate.all note else e in
 (*W.M.Equation.to_stringm eqn >>= fun se ->
 Format.printf "In check create: %s\n%!" se;*)
  node_by_eqn eqn >>= function 
   | Some (i, n) ->
    let flip = not (Term.equal (fst (N.data n)) (fst (Eq.terms eqn))) in
    (*N.to_stringm n >>= fun ns ->
    Format.printf "Node is %s, flip is %i\n%!" ns (if flip then 1 else 0);*)
    let (r0,r1,e,c0,c1) = if flip then r1,r0,e,c1,c0 else r0,r1,e,c0,c1 in
    let pos = if flip then Pair.flip pos else pos in
    let merge = fmerge r0 r1 e c0 c1 pos p in
    restore merge i >> return i
   | None ->
    let s, t = Eq.terms eqn in
    (*W.M.Term.to_stringm s >>= fun ss ->
    W.M.Term.to_stringm t >>= fun ts ->
    Format.printf "Before created %s=%s\n%!" ss ts;*)
    let n = fcreate s t r0 r1 e pos p in
    (*N.to_stringm n >>= fun ns ->
    Format.printf "Created: %s\n%!" ns;*)
    store n >> id n
;;

let check_create = check_create_with N.merge N.create

(* terms assumed to be already normalized *)
let create s t r0 r1 e p = 
 let t_start = Unix.gettimeofday () in
 project W.M.Termx.funs_pos (s,t) >>= fun pos ->
 St.add_t_one ((Unix.gettimeofday ()) -. t_start) >>
 check_create s t r0 r1 e [] [] pos [p]
;;

let create_deduce s t e p = 
 project W.M.Termx.funs_pos (s,t) >>= fun pos ->
 check_create s t ([],[]) ([],[]) e [] [] pos [p]
;;

(*
let create_constraint rule c0 c1 = 
 check_create (Rule.lhs rule) (Rule.rhs rule) ([],[]) ([],[]) [] c0 c1 []
;;*)

let split s i = restore (N.map (CP.apply_split s)) i;;

let check_node pred s t r0 r1 e =
 node_by_eqn (Eq.of_terms s t) >>= function
  | Some (_,n) -> return (pred n r0 r1 e)
  | None -> return false
;;

(* Checks whether there exists a node n which subsumes a possible new node 
  with data s, t and labels r0, r1, e. Due to its usage upon creation of a 
  new node, c0 and c1 are not checked. (s, t) need not be normalized! *)
let is_subsumed = check_node N.subsumes

(* Tries to check whether there exists a node n having data s,t such that
 the processes in the given label sets are already dealt with.
 The criterion is used when a node is to be generated; here we check 
 whether r0, r1 and e are already contained in the node's constraints.
 However, a node might (by rewriting) already be done with although
 the following function does not recoginze it as such. (Not a problem,
 as only used as an optimisation) *)
let is_done_with = check_node N.is_done_with

(* check whether node need not be created ( in rewrite), i.e.,
   either has equal terms or is subsumed or is done with *)
let is_not_necessary s t r0 r1 e =
 if Term.compare s t = 0 then return true else
 (is_subsumed s t r0 r1 e >>= fun b1 -> (
 if b1 then return true else is_done_with s t r0 r1 e))
;;

(* wrappers for node functions so that they can be called with ints *)
let id_lift f i = 
 by_id i >>= function 
  | Some n -> return (f n)
  | None -> failwith "no id in id_lift found"
;;

let id_liftm f i = 
 by_id i >>= function
  | Some x -> f x
  | None -> failwith "no id in id_liftm found"

let eo = id_lift N.eo;;
let ec = id_lift N.ec;;
let e_all = id_lift N.e_all;;
let reo = id_lift N.reo;;
let rueo = id_lift N.rueo;;
let data = id_lift N.data;;
let parents = id_lift N.parents;;
let constraints = id_lift N.constraints;;
let brule b = id_lift (N.brule b);;
let brc b = id_lift (N.brc b);;
let brcec b = id_lift (N.brcec b);;
let brewpos b = id_lift (N.brewpos b);;
let bdedpos b = id_lift (N.bdedpos b);;
let bcontent b = id_lift (N.bcontent b);;

let bmap_union f i =
 f true i >>= fun xs -> f false i >>= (return <.> (union xs))
;;

let to_string i = 
 (id_lift N.to_string i) >>= fun s ->
 return (s^" ("^(string_of_int i)^")")
;;

let to_stringm i =
 (id_liftm N.to_stringm i) >>= fun s ->
 return (s^" ("^(string_of_int i)^")")
;;

let close = restore N.close 

let close_protected = restore N.close_protected

let move_to_r_and_c ps = restore (N.move_to_r_and_c ps)

(*
let bupdate b n r0 r1 e = restore (fun n -> N.bupdate b n r0 r1 e) n
*)
let has_non_empty_labels = id_lift N.has_non_empty_labels

let has_non_empty_unprotected_open_labels = 
 id_lift N.has_non_empty_unprotected_open_labels
;;

let contains_rule_labels = id_lift N.contains_rule_labels

let er_contains_closed p = id_lift (N.er_contains_closed p)

let sum_size = id_lift (N.sum_size)

let max_size = id_lift (N.max_size)
  
let restrict_to_process p = restore (N.restrict_to_process p)

let remove_processes ps = restore (N.remove_processes ps)

let remove_er ps = restore (N.remove_er ps)

(*let s_normalize s =
 let t_start = Unix.gettimeofday () in
 W.get_options >>= (return <.> Completion.s_theory) >>= fun th ->
 W.M.Trsx.normal_form s th >>= fun res ->
 Statistics.add_t_srewrite ((Unix.gettimeofday ()) -. t_start) >>
 return res
;;

let rec s_normalize t =
  let check ((n,b),p) = function
   | Some t -> return (Some t)
   | None -> (brule b n >>= fun rl ->
    W.M.ACRewrite.rewrite_with_at t rl p >>= function
     | [] -> return None
     | t' :: _ -> return (Some t'))
  in
  W.M.Termx.funs_pos t >>= fun pos ->
  NI.s_encompassments (t,pos) >>= fun rulenodes ->
  foldr check None rulenodes >>= function
   | None -> return t
   | Some t' -> s_normalize t'
;;

let s_normalize s = mcache s_normalize s s_normal_forms

let normalize s u r0 r1 e =
 W.M.Equation.oriented_of_terms s u >>= fun (eqn,b) ->
 let s,u = Equation.terms eqn in
 project s_normalize (s,u) >>= fun (s,u) ->
 W.M.Equation.oriented_of_terms s u >>= fun (eqn,b') ->
 let not_flipped = (b && b') || (not b' && not b) in
 let s,u = Equation.terms eqn in
 return (if not_flipped then s,u,r0,r1,e else s,u,r1,r0,e)
;; *)

let create_rule rule r i =
 let s, t = Rule.to_terms rule in
 let p = Types.Node.Extend(i,r) in
 W.M.Termx.funs_pos_extended rule s >>= fun sp ->
 W.M.Termx.funs_pos t >>= fun tp ->
 N.order(*_s_normalize*) s t ([],r) ([],[]) [] >>= fun (s,t,r0,r1,_) ->
 (*W.M.Rule.to_stringm (Rule.of_terms s t) >>= fun rs ->
 Format.printf "Create norm rule %s for %s %s\n%!" rs (CP.set_to_string (snd r0)) (CP.set_to_string (snd r1));*)
 let pos = if ([],r) = r0 then (sp,tp) else (tp,sp) in (* flip? *)
(* check_create s t r0 r1 [] (snd r0) (snd r1) pos [p] >>= fun n ->*) 
 check_create s t r0 r1 [] [] [] pos [p] >>= fun n ->
 restore (N.set_pos (N.make_pos (fst pos,fst pos) (snd pos,snd pos))) n >>
 return [n]
;;

let extend i =
 let bextend b n =
  N.bextend b n >>= function
    None -> return [] 
  | Some(rule, r) -> create_rule rule r (i,b)
 in bmap_union (fun b -> id_liftm (bextend b)) i >>= fun ne ->
 iter close_protected ne >> return ne
;;

let considered_for e =
 node_by_eqn e >>= function
  | None -> return []
  | Some (_,n) -> return (N.label_union n)
;;

let set_deduce_positions = restorem N.set_deduce_positions


