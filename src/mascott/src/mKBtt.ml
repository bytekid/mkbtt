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
module CP = CompletionProcessx;;
module ACPos = ACPosition;;
module Tx = World.M.Termx;;
module Context = U.Context;;
module ACL = World.M.ACLogic;;
module T = Termination;;
module W  = World;;
module Monad = W.Monad;;
module St = Statistics;;
module N = IndexedNode;;
module NSA = NodeSetAux;;
module NS = NodeState;;
module ACRewrite = W.M.ACRewrite;;
module NI = NodeTermIndex;;
module O = Types.Overlap;;

(*** OPENS (2) ***********************************************************)
open Monad;;
open World;;

(*** FUNCTIONS ***********************************************************)

let log1 = W.log 1

let log2 s = ((*Format.printf "%s\n" s;*) W.log 2 s)


(* --- Process set operations ---------------------------------------- *)
let inter = List.intersect

let union = List.union

let diff = List.diff

let is_empty = List.is_empty

(* ------------------------------------------------------------------ *)
(*  NORMALIZE inference                                               *)
(* ------------------------------------------------------------------ *)
(* some caching stuff *)
let s_normal_forms : (Term.t, Term.t) Hashtbl.t = Hashtbl.create 300

let s_reducts : (Term.t, Term.t option) Hashtbl.t = Hashtbl.create 300

let mcache f k table =
 let x = try Some (Hashtbl.find table k) with Not_found -> None in
 match x with
  | None -> (
   f k >>= fun v ->
   Hashtbl.add table k v;
   return v)
  | Some v -> return v
;;

(* some not so dirty functions *)
let map_none f x = function Some r -> return (Some r) | None -> f x

let rec s_normalize_term t =
 CP.all_processes >>= fun ps ->
 NS.s_nodes >>= NSA.project_r (List.hd ps) >>= fun ss ->
 let step t =
  let rec step = function
   | Term.Var x -> return None
   | (Term.Fun (f,ts) as t) ->
    let rew ((n,b),p) =
     N.brule b n >>= fun rl ->
     W.M.ACRewrite.rewrite_with_at t rl p >>= function
      [] -> return None | (t',_) :: _ -> return (Some t') in
    NI.s_encompassments (t,[ACPos.root t]) >>= foldr (map_none rew) None
    >>= function
     | Some u -> return (Some u)
     | None -> (
      let rfold (us,b) ti =
       step ti >>= function
        None -> return (us@[ti], b) | Some ui -> return (us@[ui], true)
      in
      foldl rfold ([], false) ts >>= fun (us, b) ->
      if b then return (Some (Term.Fun (f,us))) else return None)
  in mcache step t s_reducts
 in step t >>= function
   None -> return t
 | Some u -> W.M.Termx.flatten u >>= fun u' -> s_normalize_term u'
;;

let s_normalize_term t = mcache s_normalize_term t s_normal_forms

let s_normalize s u r0 r1 e =
 W.M.Equation.oriented_of_terms s u >>= fun (eqn,b) ->
 let s,u = Equation.terms eqn in
 (*project W.M.Term.to_stringm (s,u) >>= fun (ss,us) ->
 Format.printf "S-Normalize %s=%s\n" ss us;*)
 project s_normalize_term (s,u) >>= fun (s,u) ->
 W.M.Equation.oriented_of_terms s u >>= fun (eqn,b') ->
 (*project W.M.Term.to_stringm (s,u) >>= fun (ss',us') ->
 Format.printf "S-Normalize %s=%s to %s=%s\n%!" ss us ss' us';*)
 let not_flipped = (b && b') || (not b' && not b) in
 let s,u = Equation.terms eqn in
 return (if not_flipped then s,u,r0,r1,e else s,u,r1,r0,e)
;;

let fixed_rule_normalize s u r0 r1 e =
 W.M.Equation.oriented_of_terms s u >>= fun (eqn,b) ->
 let s,u = Equation.terms eqn in
(* project W.M.Term.to_stringm (s,u) >>= fun (ss,us) ->
 Format.printf "FR-Normalize %s=%s\n%!" ss us;*)
 NS.fixed_rules >>= fun trs ->
 NS.s_rules >>= fun strs ->
 let trs = Trs.union strs trs in
 let fr_normalize = ACRewrite.normalize trs in
 project fr_normalize (s,u) >>= fun (s,u) ->
 W.M.Equation.oriented_of_terms s u >>= fun (eqn,b') ->
(* project W.M.Term.to_stringm (s,u) >>= fun (ss',us') ->
 Format.printf "FR-Normalize %s=%s to %s=%s\n%!" ss us ss' us';*)
 let not_flipped = (b && b') || (not b' && not b) in
 let s,u = Equation.terms eqn in
 return (if not_flipped then s,u,r0,r1,e else s,u,r1,r0,e) 
 >>= fun (s,u,r0,t1,e) -> s_normalize s u r0 r1 e
;;

let clean_terms s u r0 r1 e =
 W.M.Equation.oriented_of_terms s u >>= fun (eqn,b') ->
 let s,u = Equation.terms eqn in
 return (if not b' then s,u,r0,r1,e else s,u,r1,r0,e)
;;

(* ------------------------------------------------------------------ *)
(*  GC inference                                                      *)
(* ------------------------------------------------------------------ *)

let gc = filter N.has_non_empty_labels

(* ------------------------------------------------------------------ *)
(*  REWRITE inferences                                                *)
(* ------------------------------------------------------------------ *)

(*
let check_index_remove b n r0 r0' r1 r1' =
 (if not (List.is_empty (snd r0)) && (List.is_empty (snd r0')) then
  NI.remove_node b n
 else return ()) >>
 if not (List.is_empty (snd r1)) && (List.is_empty (snd r1')) then
  NI.remove_node (not b) n
 else return ()
;;*)

let create_node (s,r0u,r0p,e,(rl1, rl2, rr)) ((mom,bm),p,(dad,bd)) (u,sigma) =
 let t = Rule.rhs rl1 in
 let cp = Rule.of_terms s u in
 let o = O.make t (rl1,rr,(mom,bm)) p (rl2,rr,(dad,bd)) sigma cp in
 (*let pt = Types.Node.Rewrite((mom,bm), p, (dad,bd),t, union r0u r0p, e) in*)
 let pt = Types.Node.Rewrite o in
 W.get_options >>= fun o ->
 (if not (Completion.certification_output o) then
   fixed_rule_normalize s u (r0u,r0p) ([],[]) e (* includes S-normalization*)
  else clean_terms s u (r0u,r0p) ([],[]) e) >>= fun (s,u,r0,r1,e) ->
 N.is_not_necessary s u r0 r1 e >>= fun b ->
 if b then ((*log2 "Not necessary" >>*) return [])
 else N.create s u r0 r1 e pt >>= fun n -> 
 N.to_stringm n >>= fun str -> log2 ("Rewrite creates node "^str) >>
 return [n]
;;

let rewrite_node_with_some (rewrite, labels,retrieve_fun) uo b n =
 let rewrite_with ((rn,rb),p) = 
  N.bcontent b n >>= fun (s,t,r0,r1,e) ->
  (* decide for open or closed labels *)
  let project = if uo then fst else snd in
  let r0,r1,e = (project r0,project r1,project e) in
  (* use only unprotected labels *)
  let r1 = fst r1 in
  (* find rule labels *)
  N.brc rb rn >>= fun rr ->
  (* determine labels of new node (depend on uo) and term of new node *)
  let i = inter rr in
  let r0u',r0p',e' = i (fst r0), i (snd r0), labels (i r1) (i e) in
  let rr = union r0u' (union r0p' e') in
  if List.is_empty rr || (n=rn) then 
   return [] 
  else (
   W.M.Term.to_stringm t >>= fun ts -> 
(*   N.brule rb rn >>= W.M.Rule.to_stringm >>= fun ns -> log2 ("Rewrite? "^ts^" with "^ns) >>*)
   N.brule rb rn >>= fun rule -> rewrite t p rule >>= fun reducts ->
   (*log2 "reducts found" >>*)
   if List.is_empty reducts then
    return []
   else
    let rulelab = Rule.of_terms s t, rule,rr in
    (*N.to_stringm n >>= fun ns ->
    N.brule rb rn >>= W.M.Rule.to_stringm >>= fun rs ->
    log2 ("Rewrite "^ns^" with "^rs^" ("^(string_of_int (List.length reducts))^")") >> *)
    N.remove_er rr n >>
    flat_map (create_node (s,r0u',r0p',e',rulelab) ((n,b),p,(rn,rb))) reducts)
 in
 N.brule b n >>= fun rl ->
 if Term.is_var (Rule.rhs rl) then
  return []
 else (
  N.brewpos (not b) n >>= fun pos ->
  retrieve_fun (Rule.rhs rl,pos) >>= fun rulenodes ->
  flat_map rewrite_with rulenodes)
;;

(* Tries to use a rewrite step to change nodes in ns using closed nodes.
   fs are flags to distinguish between rewrite1 and rewrite2 inferences.
   Return node sets (in, cn) where
   - in are the irreducible nodes of ns and
   - cn are newly created nodes *)
let rewrite_nodes fs uo ns =
 let add_reducts (irn, cn) n =
  N.bmap_union (rewrite_node_with_some fs uo) n >>= fun cn' ->
  if List.is_empty cn' then return (union [n] irn,cn) 
  else return (irn,union cn' cn)
 in
 foldl add_reducts ([], []) ns 
;;

(* rewrite1 uses generic rewrite function, with appropriate index
   retrieval and equation label filtering parameters *)
(*let rewrite1 idx b ns =
 let rt = ACPos.root in
 NS.fixed_rules >>= fun trs ->
 let rewrite t _ rl = (*ACRewrite.rewrite1*) ACRewrite.nf_with trs t (rt t) (rl, idx) in
 let elabels l2 l3 = l3 in
 let variants (t,_) = 
  NI.variants_in idx t >>= fun vs ->
  return (List.map (flip Pair.make (rt t)) vs)
 in
 rewrite_nodes (rewrite, elabels, variants) b ns 
;;

(* rewrite2 uses generic rewrite function, with appropriate index
   retrieval and equation label filtering parameters *)
let rewrite2 idx b ns = 
 NS.fixed_rules >>= fun trs ->
 let rewrite t p rl = (*ACRewrite.rewrite2*) (ACRewrite.nf_with trs t p (rl,idx)) in
 let elabels l2 l3 = union l2 l3 in
 rewrite_nodes (rewrite, elabels,NI.encompassments_in idx) b ns
;; *)

let rewrite12 idx b ns = 
 W.get_options >>= fun o ->
 if Completion.certification_output o then
  let rewrite t p rl = ACRewrite.rewrite_with_at t rl p in
  let elabels l2 l3 = union l2 l3 in
  rewrite_nodes (rewrite, elabels, NI.encompassments_in idx) b ns
 else
  NS.fixed_rules >>= fun trs ->
  let rewrite t p rl = 
   ACRewrite.nf_with trs t p (rl,idx) >>= fun rs ->
   return (List.map (fun t -> t,Sub.empty) rs)
  in
  let elabels l2 l3 = union l2 l3 in
  rewrite_nodes (rewrite, elabels,NI.encompassments_in idx) b ns
;;

(* return nodes created in 1 or more steps which are not further 
   reducible *)
let rewrite_fixpoint uo idx ns =
 let rec rewrite irreducible ns =
  (*IndexedSet.to_stringm ns >>= fun  nss ->
  log2 ("rewrite iteration: ("^(string_of_int (List.length ns))^")" ^ nss) >>*)
(*  rewrite1 idx uo ns >>= fun (irred1, created1) ->*)
  rewrite12 idx uo ns >>= fun (irreducible, created) ->
(*  let created = List.union created1 created2 in
  let irreducible = union (inter irred1 irred2) irreducible in*)
  if is_empty created then return irreducible
  else rewrite irreducible created
 in
 rewrite [] ns >>= fun not_further_reducible ->
 let new_ones = diff not_further_reducible ns in
 (*IndexedSet.to_stringm not_further_reducible >>= fun nss ->
  log2 ("Fixpoint reached.\n Not further reducible: "^(string_of_int (List.length ns))^"\n"^nss) >>
  IndexedSet.to_stringm new_ones >>= fun nss ->
  log2 ("Created: "^(string_of_int (List.length ns))^"\n"^nss) >>*)
 return new_ones
;;   

(* for the set of nodes ns, return nodes created by rewrite steps *)
let rewrite uo ns idx =
 let t_start = Unix.gettimeofday () in
 rewrite_fixpoint uo idx ns >>= fun created_nodes -> 
 St.add_t_rewrite ((Unix.gettimeofday ()) -. t_start) >>
 return created_nodes
;;

let rewrite_open ns = NI.get_index >>= rewrite true ns

let rewrite_closed ns = NI.get_index >>= rewrite false ns

let rewrite_closed_with ns n = NI.single_index n >>= rewrite false ns

let rewrite_open_with ns n = NI.single_index n >>= rewrite true ns


(* ------------------------------------------------------------------ *)
(*  DEDUCE inference                                                  *)
(* ------------------------------------------------------------------ *)

(* Creates a node corresponding to the critical pair u=v derived for 
 process set e from the overlap (dad,p,mom) on the term w.
 Note that u is l1\sigma[r2\sigma]_p while v is r1\sigma. *)
let create_node istheta (u,v,e,o) =
 (*N.to_stringm (fst (O.mom o)) >>= fun s1 ->
 N.to_stringm (fst (O.dad o)) >>= fun s2 ->
 let i1 = if (snd (O.mom o)) then "1" else "0" in
 let i2 = if (snd (O.dad o)) then "1" else "0" in
 let p = ACPosition.to_string (O.position o) in
 W.M.Term.to_stringm (O.source o) >>= fun us ->
 log2 ("overlap: "^s1^" <"^i1^"> with "^s2^" <"^i2^"> at "^p^" from"^us) >>*)
(* N.brule (snd (O.mom o)) (fst (O.mom o)) >>= fun mr ->
 N.brule (snd (O.dad o)) (fst (O.dad o)) >>= fun dr ->
 let moms = Rule.map (Sub.apply_term (O.sub o)) mr in
 let dads = Rule.map (Sub.apply_term (O.sub o)) dr in
 W.M.Rule.to_stringm mr >>= fun s1 ->
 W.M.Rule.to_stringm dr >>= fun s2 ->
 log2 ("substituted: " ^ s1 ^ " and " ^ s2) >>*)
 W.get_options >>= fun opt ->
 (if not (Completion.certification_output opt) then
  fixed_rule_normalize u v ([],[]) ([],[]) e 
  else clean_terms u v ([],[]) ([],[]) e) >>= fun (u,v,_,_,_) -> (* also S *)
 (*map W.M.Term.to_stringm [u;v] >>= fun [us;vs] ->
 log2 ("CP "^us^" = "^vs^"?") >>*) 
 N.is_not_necessary u v ([],[]) ([],[]) e >>= fun b ->
(* log2 ("Node is not necessary:"^(if b then "1" else "0")) >>*)
 (*log2 ("deduce for labels "^(CP.set_to_string e)) >>*)
 if b then return [] else
   (if istheta then return e else CPC.filter_nonredundant o (u,v) e) >>= fun e -> 
   if List.is_empty e then (*log2 "Redundant" >>*) return [] else 
    (*let pt = Types.Node.Deduce(O.mom o, O.position o, O.dad o, o, e) in*)
    let pt = Types.Node.Deduce o in
    N.create_deduce u v e pt >>= fun n -> 
    N.to_stringm n >>= fun s ->
    log2 ("Node created: "^s) >>
    return [n]
;;

let is_nontrivial ((_,_,mom),(_,_,_,dad)) (u,p,_) =
(* checked in narrow anyway *)
(*  not (Term.is_var (Term.subterm p (Rule.lhs orl))) &&*)
(* checked in bdeduce/overlap2/overlap1 anyway*)
(*  (not (Term.is_var (Rule.lhs rl'))) && *)
  not ((dad = mom) && (p = (ACPos.root u)))
;;

let overlap fnarrow (((irule,rd,dad),(orule,rm,op,mum)) as o) =
 let e = inter rd rm in
 if List.is_empty e then 
  return []
 else
(*  W.M.Rule.to_stringm irule >>= fun irules ->
  W.M.Rule.to_stringm orule >>= fun orules ->
  log2 ("Check overlaps between inner "^irules^", outer "^orules) >>*)
  let l,r = Rule.to_terms orule in 
  fnarrow l op irule >>= fun os ->
  (*log2 ((string_of_int (List.length os))^" overlaps") >>*)
  let to_cp (u,p,(sigma,lpsigma,lsigma)) =
   let mom = (orule,rm,mum) in
   (Tx.flatten <.> Sub.apply_term sigma) r >>= fun rsigma ->
   let cp = Rule.of_terms u rsigma in
   let o = O.make lsigma mom p (fst o) sigma cp in
   return (u,rsigma,e,o) 
  in
  map to_cp (List.filter (is_nontrivial o) os)
;;

let overlap1' ac_narrow ((orule,rm,opos,mum) as outer) ns = 
 let overlap1_with n =
  let narrow b n = 
   N.brule b n >>= fun rl -> N.brc b n >>= fun r ->
   if Rule.is_rewrite_rule rl then 
    overlap ac_narrow ((rl,r,(n,b)),outer) 
   else return []
  in
  N.bmap_union narrow n 
 in flat_map overlap1_with ns
;;

let overlap1 = overlap1' ACRewrite.narrow

(* overlap 2: node n is inner rule *)
let overlap2_below_root ((irule,rd,dad) as inner) ns =
 let overlap2_with n =
 let narrow b n = 
  N.brule b n >>= fun rl -> N.brc b n >>= fun r ->
  N.bdedpos b n >>= fun opos ->
  if Rule.is_rewrite_rule rl then 
   overlap ACRewrite.narrow_below_root (inner, (rl,r,opos,(n,b))) 
  else return []
 in
 N.bmap_union narrow n 
 in flat_map overlap2_with ns
;;

let bdeduce is_theta b n =
 N.brule b n >>= fun rule -> N.brc b n >>= fun r ->
 N.bdedpos b n >>= fun pos ->
 if not (Rule.is_rewrite_rule rule) then
  return []
 else
  W.M.Rule.rename rule >>= fun rule ->
  NS.closed_nodes >>= fun ns ->
  overlap1 (rule,r,pos,(n,b)) ns >>= fun oc1 ->
  overlap2_below_root (rule,r,(n,b)) ns >>= fun oc2 ->
  flat_map (create_node is_theta) (List.rev_append oc1 oc2)
;;

let deduce n =
 let t_start = Unix.gettimeofday () in
 N.bmap_union (bdeduce false) n >>= fun ns ->
 St.add_t_deduce (Unix.gettimeofday () -. t_start) >>= fun _ ->
 return ns
;;

let set_current ns =
 W.get_node_state >>= fun c ->
 W.set_node_state {c with current = ns} 
;;

let deduce_rewrite n =
 deduce n >>= fun ns ->
 N.extend n >>= fun ne ->
 set_current (n::ne) >>
(* IndexedSet.to_stringm ne >>= fun nes ->
 log1 ("Created by Extend: \n"^nes) >>*)
 flat_map deduce ne >>= fun ns' ->
 let ns = union ns ns' in
 IndexedSet.to_stringm ns >>= fun nss ->
 log1 ("Created by Deduce: \n"^nss) >>
 rewrite_open ns >>= fun ns' ->
 gc ns >>= fun ns ->
 let nss = union ns ns' in
 IndexedSet.to_stringm nss >>= fun nss ->
 log1 ("Returned by deduce: \n"^nss) >>
 return (union ns ns')
;;


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
 NS.closed_nodes >>= fun ns ->
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

let term_checks_both_dirs (st,ts) e =
 let check m p =
  m >>= fun (lr,rl,s) ->
  NS.all_nodes >>= fun ns ->
  (*NS.closed_nodes >>= fun ns ->
  NS.s_nodes >>= fun s -> *)
  NSA.project_c p ns >>= fun cs ->
  let trs, trs' = Trs.add st cs, Trs.add ts cs in
  Termination.preserved_parallel trs trs' >>= fun (b1, b2) ->
  let add_if x xs b = if b then x :: xs else xs in
  let only_lr, only_rl = b1 && not b2, b2 && not b1 in
  return (add_if p lr only_lr, add_if p rl only_rl, add_if p s (b1 && b2))
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
let termination_checks_for_processes (s,t) e =
 let st, ts = Rule.of_terms s t, Rule.of_terms t s in 
 match might_terminate st, might_terminate ts with
  | true, true  -> term_checks_both_dirs (st,ts) e 
  | true, false -> term_checks_one_dir st e >>= fun p -> return (p,[],[])
  | false, true -> term_checks_one_dir ts e >>= fun p -> return ([],p,[])
  | _           -> return ([],[],[])
;;

(* check if ALL processes orient this rule in certain way, if yes,
   add it to set of simplification rules *)
let add_fixed_rule (s,t) r_lr r_rl =
 W.get_processes >>= fun c ->
 if List.equal r_lr c.all then NS.add_fixed_rule (Rule.of_terms s t)
 else if List.equal r_rl c.all then NS.add_fixed_rule (Rule.of_terms t s)
 else return ()
;;

let psi_general n =
 N.eo n >>= fun e -> (* equation label *)
 if List.is_empty e then 
  N.close n 
 else
  N.data n >>= fun (s, t) ->
  termination_checks_for_processes (s,t) e >>= fun (r_lr,r_rl,ss) ->
  N.move_to_r_and_c (r_lr,r_rl,ss) n >>
  add_fixed_rule (s,t) r_lr r_rl
;;

(* beforehand n needs to be oriented, otherwise no CPs computed *)
let btheta_general b n =
 NS.s_nodes >>= fun sn ->
(* IndexedSet.to_stringm sn >>= fun nss ->
 log1 ("S: \n"^nss) >>*)
 N.brule b n >>= fun rule ->
 W.M.Rule.rename rule >>= fun rule ->
 N.brc b n >>= fun r ->
 N.bdedpos b n >>= fun upos ->
 overlap1 (rule,r,upos,(n,b)) sn >>= fun oc1 ->
 overlap2_below_root (rule,r,(n,b)) sn >>= fun oc2 ->
 flat_map (create_node true) (List.rev_append oc1 oc2)
;;

let theta_general n =
(*log1 "Starting Theta computation" >> *)
 N.bmap_union btheta_general n >>= fun ns ->
 N.extend n >>= fun ne ->
 set_current (n::ne) >>
 flat_map (N.bmap_union btheta_general) ne >>= fun ns' ->
 let ns = union ns ns' in
(* IndexedSet.to_stringm ns >>= fun nss ->
 log1 ("Created by Theta: \n"^nss) >>*)
 rewrite_open ns >>= fun ns' ->
 gc (union ns ns') >>= NS.add_open >>
(* IndexedSet.to_stringm ns >>= fun nss ->
 log1 ("After rewrite: \n"^nss) >>*)
 return ns
;;

let orient n = 
 psi_general n >>
 N.set_deduce_positions n >>
 theta_general n >>= NS.add_open >>= fun _ ->
 return ()
;;

(* ------------------------------------------------------------------ *)
(*  check for success                                                 *)
(* ------------------------------------------------------------------ *)

let unfinished_processes =
 let add f l n = f n >>= fun l' -> return (union l l') in
 NS.open_nodes >>= fun ns ->
 foldl (add N.rueo) [] ns >>= fun are_open ->
(* Format.printf "Open processes: %s\n" (CP.set_to_string are_open);*)
 NS.closed_nodes >>= fun ns ->
 foldl (add N.ec) [] ns >>= fun closed_eq ->
 (*Format.printf "Closed equation for: %s\n" (CP.set_to_string closed_eq);*)
 return (union are_open closed_eq)
;;


