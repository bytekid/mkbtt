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
 @since  2010/11/01 *)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module Sub = U.Substitution;;
module Term = U.Term;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module Elogic = U.Elogic;;
module C = Completion;;
module CP = CompletionProcessx;;
module CC = CPCCache;;
module T = Termination;;
module W  = World;;
module Monad = W.Monad;;
module St = Statistics;;
module N = IndexedNode;;
module NSA = NodeSet;;
module NS = NodeState;;
module NI = NodeTermIndexx;;
module PIso = ProcessIsomorphism;;

(*** OPENS (2) ***********************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)

let log1 = W.log 1

let log2 s = ((*Format.printf "%s\n" s;*) W.log 2 s)


(* --- Process set operations ---------------------------------------- *)
let inter = List.intersect

let union = List.union

let diff = List.diff

let is_empty = List.is_empty


(* ------------------------------------------------------------------ *)
(*  ORIENT inference                                                  *)
(* ------------------------------------------------------------------ *)

let rec tuple = function
 | [] -> [],None
 | [x] -> [], Some x
 | x :: x' :: xs -> let ps,s = tuple xs in (x,x')::ps, s
;;

let untuple = List.foldl (fun l (p,q) -> p::q::l) []

(* parallel version *)
let term_checks_one_dir rl e =
 NS.all_nodes >>= fun ns ->
 let project p = NSA.project_c p ns >>= fun c -> return (p,c) in
 let add = apply return (T.preserved_by_rule rl) in
 let add2 ((p,cs),(p',cs')) = 
  T.preserved_parallel (Trs.add rl cs) (Trs.add rl cs') >>= fun (b,b') -> 
  return ((p,b),(p',b'))
 in
 map project e >>= fun e ->
 let ps, single = tuple e in
 map add2 ps >>= (return <.> untuple) >>= fun rs ->
 let filter = List.foldl (fun l (x,b) -> if b then x::l else l) [] in
 match single with 
   None -> return (filter rs) 
 | Some p -> add p >>= fun r -> return (filter (r::rs))
;;


let do_split n lr rl st cs p =
 if lr && not rl then return (true, false, false)
 else if rl && not lr then return (false, true, false)
 else if not rl && not lr then return (false, false, false)
 else (
  PIso.orientation_symmetric st n cs p >>= fun b -> 
  return (b, false, not b))
;;

let term_checks_both_dirs n (st,ts) e =
 let check m p =
  m >>= fun (lr,rl,s) ->
  NS.all_nodes >>= fun ns ->
  NSA.project_c p ns >>= fun cs ->
  let trs0, trs1 = Pair.map (fun rl -> Trs.add rl cs) (st,ts) in
  Termination.preserved_parallel trs0 trs1 >>= fun (b1, b2) ->
  let add_if x xs b = if b then x :: xs else xs in
  do_split n b1 b2 st cs p >>= fun (only_lr, only_rl, both) ->
  return (add_if p lr only_lr, add_if p rl only_rl, add_if p s both)
 in
 List.fold_left check (return ([],[],[])) e >>= fun (lr,rl,s) ->
 NS.split_state s >>
 return (lr, rl, s)
;;

let might_terminate rl = 
 let l, r = Rule.to_terms rl in
 (Rule.is_rewrite_rule rl) && (not (Term.is_subterm l r))
;;

(* a wrapper checking whether node allows for rewrite rule(s) *)
let termination_checks_for_processes n (s,t) e =
 let st, ts = Rule.of_terms s t, Rule.of_terms t s in 
 match might_terminate st, might_terminate ts with
  | true, true  -> term_checks_both_dirs n (st,ts) e 
  | true, false -> term_checks_one_dir st e >>= fun p -> return (p,[],[])
  | false, true -> term_checks_one_dir ts e >>= fun p -> return ([],p,[])
  | _           -> return ([],[],[])
;;

let orient n =
 let t_start = Unix.gettimeofday () in
 N.eo n >>= fun e -> (* equation label *)
 if List.is_empty e then (
  N.close n >>
  (St.add_t_orient (Unix.gettimeofday () -. t_start)))
 else (
  N.data n >>= fun (s, t) ->
  termination_checks_for_processes n (s,t) e >>= fun (r_lr,r_rl,s) ->
  N.move_to_r_and_c (r_lr,r_rl,s) n >>
  (St.add_t_orient (Unix.gettimeofday () -. t_start))) 
;;

(* ------------------------------------------------------------------ *)
(*  GC inference                                                      *)
(* ------------------------------------------------------------------ *)

let gc = filter N.has_non_empty_labels

(* ------------------------------------------------------------------ *)
(*  REWRITE inferences                                                *)
(* ------------------------------------------------------------------ *)

let normalize s u r0 r1 e =
 let eqn,b = Equation.oriented_of_terms s u in
 let s,u = Equation.terms eqn in
 if b then s,u,r0,r1,e else s,u,r1,r0,e
;;

let check_index_remove b n r0 r0' r1 r1' =
 (if not (List.is_empty (snd r0)) && (List.is_empty (snd r0')) then
  NI.remove_node b n
 else return ()) >>
 if not (List.is_empty (snd r1)) && (List.is_empty (snd r1')) then
  NI.remove_node (not b) n
 else return ()
;;


(* tries to rewrite node n in orientation bn using node rn in orientation 
   brn at position p. uo is a flag determining whether to rewrite open 
   (true) or closed (false) labels. el projects out equation labels, 
   dependent on rewrite1 or rewrite2. *)
let rewrite_node_with uo (el,varts) (n, bn) ((rn, brn), p) =
(* N.to_stringm n >>= fun s1 ->
 N.to_stringm rn >>= fun s2 ->
 let bs b = if b then "true" else "false" in
 let str = "rewrite "^s1^"("^(bs bn)^") with "^s2^"("^(bs brn)^") at "^(Pos.to_string p) in*)
 N.bcontent bn n >>= fun (s,t,r0,r1,e) ->
 N.bcontent brn rn >>= fun (l,r,(_,rr),(_,rr'),_) ->
 (* determine labels (depend on uo) and term of new node *)
 let i (o, c) = if uo then inter o rr else inter c rr in
 let r0', e' = i r0, el (i r1) (i e) in
 (* if step not effective, return *)
 let rr = union r0' e' in
 if List.is_empty rr || (n = rn) || (varts t l) then 
  (*log2 "aborted: empty labels or same nodes or variants" >>*)
  return ([], false) 
 else
  let pt = Types.Node.Rewrite((n, bn), p, (rn, brn), r0', e') in
  W.M.Rulex.rewrite t (Rule.of_terms l r) p >>= function
  | None -> return ([], false) (* not matchable *)
  | Some (u,_,_) ->
   (*log2 str >>*)
   let d (o, c) = if uo then (diff o rr,c) else (o, diff c rr) in
   N.bupdate bn n (d r0) (d r1) (d e) >>
   (if not uo then check_index_remove bn n r0 (d r0) r1 (d r1) 
    else return ()) >>
   (*N.to_stringm n >>= fun str -> 
   log2 ("Updated old node is"^str) >>*)
   let s,u,r0',r1',e' = normalize s u r0' [] e' in
   (* check whether new node is subsumed or to be deleted *)
   N.is_not_necessary s u r0' r1' e' >>= fun b -> 
   if b then (*log2 "node not necessary" >>*) return ([], true)
   else
    N.create s u r0' r1' e' pt >>= fun n' -> 
    (*N.to_stringm n' >>= fun str -> log2 ("Node created: "^str) >>*)
    return ([n'], true)
;;


(* Tries to rewrite node n using closed nodes. fs are flags to 
   distinguish between rewrite1 and rewrite2 inferences. The bool b
   distinguishes between the second term (true) or the first term
   (false) rewritten. Returns all newly created nodes *)
let rewrite_node uo (retrieve,el,p) (n,b) =
 N.brule b n >>= fun rule ->
 if Term.is_var (Rule.rhs rule) then
  return ([], false)
 else
  retrieve (Rule.rhs rule) >>= fun matches ->
  let matches = List.sort compare matches in
  let add (cn,change) m = 
   N.has_er_labels uo n >>= fun not_superfluous ->
   if uo && (not not_superfluous) then
    return (cn, change)
   else
    rewrite_node_with uo (el,p) (n,b) m >>= fun (c, change') -> 
    return (union c cn, change || change') 
  in
  foldl add ([], false) matches
;;

(* Tries to use a rewrite step to change nodes in ns using closed nodes.
   fs are flags to distinguish between rewrite1 and rewrite2 inferences.
   Return node sets (in, cn) where
   - in are the irreducible nodes of ns and
   - cn are newly created nodes *)
let rewrite_nodes fs uo ns =
 let add_reducts (irn, cn) n =
  (* can in rewrite2 already have been emptied by rewrite1 *)
  N.has_er_labels uo n >>= fun not_superfluous ->
  if uo && (not not_superfluous) then (
   return (irn, cn))
  else
   rewrite_node uo fs (n,true) >>= fun (cn_lr, change_lr) ->
   N.has_er_labels uo n >>= fun not_superfluous ->
   if uo && (not not_superfluous) then 
    return (irn,List.union cn_lr cn)
   else
    rewrite_node uo fs (n,false) >>= fun (cn_rl, change_rl) ->
    let cn' = List.union cn_lr cn_rl in
    let change = change_lr || change_rl in
    if not change then return (union [n] irn,cn) 
    else 
     N.has_er_labels uo n >>= fun not_superfluous ->
     if not_superfluous then (* for some labels irreducible *)
      return (union [n] irn,List.union cn' cn)
     else
      return (irn,List.union cn' cn)
 in
 foldl add_reducts ([], []) ns >>= fun res ->
 return res
;;

(* rewrite1 uses generic rewrite function, with appropriate index
   retrieval and equation label filtering parameters *)
let rewrite1 uo idx =
 let pair_root x = return (x,Pos.root) in
 let retrieve t = NI.variant_candidates_in idx t >>= map pair_root in
 let elabels l2 l3 = l3 in
 rewrite_nodes (retrieve, elabels,fun _ _ -> false) uo
;;

(* rewrite2 uses generic rewrite function, with appropriate index
   retrieval and equation label filtering parameters *)
let rewrite2 uo idx =
 let retrieve = NI.encompassment_candidates_in idx in
 let elabels l2 l3 = union l2 l3 in
 let variants t t' = Elogic.is_variant t' t in
 rewrite_nodes (retrieve, elabels,variants) uo
;;

(* return nodes created in 1 or more steps which are not further 
   reducible *)
let rewrite_fixpoint uo idx ns =
 let rec rewrite irreducible ns =
  (*NSA.to_stringm ns >>= fun  nss ->
  log2 ("Starting rewrite iteration with \n"^nss) >>*)
  rewrite1 uo idx ns >>= fun (irred1, created1) ->
  rewrite2 uo idx ns >>= fun (irred2, created2) ->
  let created = List.union created1 created2 in
  let irreducible = List.union (inter irred1 irred2) irreducible in
  if is_empty created then return irreducible
  else rewrite irreducible created
 in
 rewrite [] ns >>= fun not_further_reducible ->
 let result = diff not_further_reducible ns in
 (*NSA.to_stringm not_further_reducible >>= fun nr ->
 log2 ("Not further reducible: \n"^nr) >>
 NSA.to_stringm result >>= fun r ->
 log2 ("Result of rewrite: \n"^r) >>*)
 return result
;;   

(* for the set of nodes ns, return nodes created by rewrite steps *)
let rewrite uo idx ns =
 (*log2 "Starting rewrite" >>*)
 let t_start = Unix.gettimeofday () in
 rewrite_fixpoint uo idx ns >>= fun created_nodes -> 
 St.add_t_rewrite ((Unix.gettimeofday ()) -. t_start) >>
 return created_nodes
;;

let rewrite_open ns = NI.rewrite_index >>= fun idx -> rewrite true idx ns

let rewrite_closed_once_with ns idx =
 let t_start = Unix.gettimeofday () in
 rewrite1 false idx ns >>= fun (_, created1) ->
 rewrite2 false idx ns >>= fun (_, created2) ->
 let created = List.union created1 created2 in
 St.add_t_rewrite ((Unix.gettimeofday ()) -. t_start) >>
 return created
;;

let rewrite_open_once_with ns idx = 
 let t_start = Unix.gettimeofday () in
 rewrite1 true idx ns >>= fun (_, created1) ->
 rewrite2 true idx ns >>= fun (_, created2) ->
 let created = List.union created1 created2 in
 St.add_t_rewrite ((Unix.gettimeofday ()) -. t_start) >>
 return created
;;

(* ------------------------------------------------------------------ *)
(*  DEDUCE inference                                                  *)
(* ------------------------------------------------------------------ *)

let importance u v e =
 W.get_options >>=  fun o ->
 St.n_processes >>= fun n ->
 let th = if n > 50 then 7 else C.propagate_small_lemmata o in
 if Term.size u + (Term.size v) <= th then
  CP.all_processes
 else 
  return e
;;

let cp_of_overlap (((rl,r0,mom),p,(rl',r0',dad)) as o) =
 N.to_stringm (fst mom) >>= fun s1 ->
 N.to_stringm (fst dad) >>= fun s2 ->
 let i1 = if (snd mom) then "1" else "0" in
 let i2 = if (snd dad) then "1" else "0" in
 log2 ("Overlap "^s1^" ("^i1^") with "^s2^" ("^i2^") at "^(Pos.to_string p)) >>
 W.M.Rulex.narrow (Rule.lhs rl) rl' p >>= function
  | Some (u, sigma, rule2) ->
   let v = Sub.apply_term sigma (Rule.rhs rl) in
   let e = inter r0 r0' in
   let u,v,_,_,_ = normalize u v [] [] e in
   N.is_not_necessary u v [] [] e >>= fun b -> 
   (*log2 ("Node is not necessary:"^(if b then "1" else "0")) >>*)
   if b then return [] else
    let w = Sub.apply_term sigma (Rule.lhs rl) in
    CPC.filter_nonredundant (w,o,sigma) e >>= fun e ->
    (*log2 ("deduce for labels "^(CP.set_to_string e)) >>*)
    if List.is_empty e then return [] else
     let pt = Types.Node.Deduce(mom, p, dad, e) in
     importance u v e >>= fun e ->
     N.create u v [] [] e pt >>= fun n -> 
     (*N.to_stringm n >>= fun s -> log2 ("Node created: "^s) >>*)
     return [n]
  | None -> return []
;;

let filter_cp o =
 let valid_overlap ((rl,r,mom),p,(rl',r',dad)) =
  not (Term.is_var (Term.subterm p (Rule.lhs rl))) &&
  (not (Term.is_var (Rule.lhs rl'))) &&
  (not ((Rule.compare rl rl' = 0) && (p = Pos.root))) &&
  (not (List.is_empty (inter r r')))
 in 
 if not (valid_overlap o) then return [] else cp_of_overlap o
;;

let ovl1 labels n b ((n', b'),p) =
 (*N.to_stringm n >>= fun s1 ->
 N.to_stringm n' >>= fun s2 ->
 log2 ("overlap "^s1^" with "^s2^" at "^(Pos.to_string p)) >>*)
 pair (N.brule b, labels b) n >>= fun (rule, r) ->
 pair (N.brule b', labels b') n' >>= fun (rule', r') ->
 return ((rule,r,(n,b)),p,(rule',r',(n',b')))
;;

let ovl2 ls n b t = ovl1 ls n b t >>= fun t -> return (Triple.flip t)

let bdeduce ls n b =
 N.brule b n >>= fun rule -> let t = Rule.lhs rule in
 if Term.is_var t then
  return []
 else
  (*W.M.Term.to_stringm t >>= fun ts ->
  log2 ("Retrieve overlaps for "^ts) >>*)
  NI.overlap1_candidates_below_root t >>= fun oc1 ->
  NI.overlap2_candidates t >>= fun oc2 ->
  (*log2 ((string_of_int (List.length oc1))^" overlaps 1") >>
  log2 ((string_of_int (List.length oc2))^" overlaps 2") >>*)
  map (ovl1 ls n b) oc1 >>= fun o1 ->
  map (ovl2 ls n b) oc2 >>= fun o2 ->
  let add_cp cps o = filter_cp o >>= fun c -> return (union c cps) in
  let cmp x y = (compare x y) * (-1) in
  foldl add_cp [] (List.sort cmp (o1 @ o2))
;;

let deduce n =
 (*log2 "DEDUCE" >>*)
 let t_start = Unix.gettimeofday () in
 let ls = N.brc in
 bdeduce ls n true >>= fun ns1 ->
 bdeduce ls n false >>= fun ns2 ->
 St.add_t_deduce (Unix.gettimeofday () -. t_start) >>
 return (union ns1 ns2)
;;


(* ------------------------------------------------------------------ *)
(*  check for success                                                 *)
(* ------------------------------------------------------------------ *)

let unfinished_processes =
 let add f l n = f n >>= fun l' -> return (union l l') in
 NS.open_nodes >>= fun ns ->
 foldl (add N.reo) [] ns >>= fun are_open ->
 NS.closed_nodes >>= fun ns ->
 foldl (add N.ec) [] ns >>= fun closed_eq ->
 return (union are_open closed_eq)
;;


