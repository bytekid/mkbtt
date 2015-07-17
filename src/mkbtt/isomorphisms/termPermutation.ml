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

(** Argument permutations
 @author Sarah Winkler
 @since  2009/08/11
*)

(*** OPENS ********************************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Eq = Equation;;
module Var = Rewriting.Variable;;
module Fun = Rewriting.Function;;
module Term = U.Term;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module Sub = Termx.VSub;;
module TRen = TrsRenaming;;

(*** EXCEPTIONS **********************************************************)
exception Not_permutable;;
exception No_variant_found;;

(*** TYPES **************************************************************)
type permutation = (Fun.t * int list) list

(*** FUNCTIONS ***********************************************************)

(* interleave n l produces a list of lists obtained if n is inserted into
   l at all possible positions *)
let rec interleave' n front back result =
 match back with
  | [] -> (front @ [n]) :: result
  | a :: back' -> 
   let result' = (front @ (n :: back)) :: result in
   interleave' n (front @ [a]) back' result'
;;

let interleave n l = interleave' n [] l []


(* permutations n gives a list of all possible permutations S_n *)
let rec permutations n = 
 if n == 0 then
  [[]]
 else
  List.concat (List.map (interleave n) (permutations (n-1)))
;;

(* permutes list l according to permutations *)
let rec permute l permutation =
 match permutation with
  | [] -> []
  | i :: p -> (List.nth l (i - 1)) :: (permute l p)
;;

let rec identity n =
 if n == 0 then [] else (identity (n - 1)) @ [n]
;;

(* lookup permutation for function symbol in dictionary *)
(* currently dictionary is association list *)
let plookup f d = List.assoc f d

let plookup_save f n d = 
 try List.assoc f d
 with Not_found -> identity n
;;

(* permutes term t where dictionary dict contains maps from
function symbols to permutations *)
let rec permute_term d = function
  | Term.Var v -> Term.Var v
  | Term.Fun(f, args) -> 
   let args' = List.map (permute_term d) args in
   let a = List.length args in
   Term.Fun(f, (permute args' (plookup_save f a d)))
;;


let permute_rule d rule =
 let lhs, rhs = Rule.lhs rule, Rule.rhs rule in
 Rule.of_terms (permute_term d lhs) (permute_term d rhs)
;;

let permute_trs d = Trs.of_list <.> (Trs.map (permute_rule d))

let permute_equation d = Pair.map (permute_term d)

let permute_eqs d = List.map (permute_equation d)

(* check whether list of termpairs is permutable with respect
   to variable renaming mgu and permutation pi. Assume that pi
   is completely specified! *)
let rec permutable mgu d = function
  | [] -> (mgu, d)
  | ((Term.Var y), (Term.Var x)) :: eqs ->
    begin
    try
     let z = Sub.find y mgu in
     if (Var.compare z x) == 0 then
      permutable mgu d eqs
     else raise Not_permutable
    with Not_found ->
     (
     let s = Sub.singleton y x in
     permutable (Sub.union mgu s) d eqs
     )
     end
  | (Term.Fun (c0, []), Term.Fun (c1, [])) :: eqs when
     (Fun.compare c0 c1) == 0 ->
     permutable mgu d eqs
  | (Term.Fun (f0, [t0]), Term.Fun (f1, [t1])) :: eqs when
     (Fun.compare f0 f1) == 0 ->
     permutable mgu d ((t0, t1) :: eqs)
  | (Term.Fun (f0, ts0), Term.Fun (f1, ts1)) :: eqs when 
     (Fun.compare f0 f1) == 0 ->
    let p = plookup f0 d in
    let ts0' = permute ts0 p in
    permutable mgu d ((List.combine ts0' ts1) @ eqs)
  | _ -> raise Not_permutable   
;;

 let is_bijective ?(c = Pervasives.compare) s =
  let (dom, img) = Sub.fold (fun x t (d, i) -> (x::d, t::i)) s ([], []) in
  List.equal dom img
 ;;

let permutation_equivalence' rule d = 
 try
  let sigma = Sub.empty in
  let sigma', _ = permutable sigma d [(Rule.lhs rule, Rule.rhs rule)] in
  is_bijective sigma'
 with Not_permutable -> false
;;

(* no constants in fs *)
let rec get_dictionaries_for = function
 | [] -> [[]]
 | (f, a) :: fs ->
  let ps = permutations a in
  let ds = get_dictionaries_for fs in
  List.concat (List.map (fun p -> List.map (fun d -> (f, p) :: d) ds) ps)
;;

(* auxiliary for next function to fold *)
let fold_check_dic rule result d =
 match result with
 | Some d -> Some d
 | None -> if permutation_equivalence' rule d then Some d else None
;; 

let at_least_binary_funs rule =
 let rec find = function
  | Term.Var _ -> []
  | Term.Fun (f, ts) -> 
   let a = List.length ts in
   if a > 1 then (f, a) :: (List.concat (List.map find ts))
   else (List.concat (List.map find ts))
 in
 let l, r = Rule.to_terms rule in List.unique (find l @ (find r))
;;

(* gets a rule and tries to find a permutation that makes the two terms equal *)
let permutation_equivalence rule = 
 let fs = at_least_binary_funs rule in
 let dics = get_dictionaries_for fs in
 List.fold_left (fold_check_dic rule) None dics 
;;

let rule_symmetric rule =
 try
  match permutation_equivalence rule with
   | None -> false
   | Some _ -> true
 with
  | No_variant_found -> false
;;

let rec remove_variant' r trs trs' =
 if Trs.is_empty trs then 
  raise No_variant_found
 else
  let r' = Trs.choose trs in
  let trsred = Trs.remove r' trs in
  if Rule.is_variant r r' then
   Trs.union trsred trs'
  else
   remove_variant' r trsred (Trs.add r' trs')
;;

let remove_variant r trs = remove_variant' r trs Trs.empty

let equal_up_to_variants trs1 trs2 =
 try
  let trs2rest = Trs.fold remove_variant trs1 trs2
  in Trs.is_empty trs2rest
 with No_variant_found -> false
;;

let orientation_symmetric rule trs = 
  let deq = permutation_equivalence rule in
  match deq with
   | Some d -> 
    let trs' = permute_trs d trs in 
    if equal_up_to_variants trs trs' then d else raise Not_permutable
   | None -> raise Not_permutable
;;

let equal_as_equations eqs1 eqs2 =
 let is_variant e e' = Eq.is_variant (Eq.of_pair e) (Eq.of_pair e') in
 let has_variant eqs e = List.exists (is_variant e) eqs in
 List.for_all (has_variant eqs1) eqs2
;;

(* eqns is actually trs *)
let eqns_invariant eqns d =
 let eqns = List.map Equation.terms eqns in
 let eqns' = permute_eqs d eqns in
 equal_as_equations eqns eqns'
;;


let trs_invariant trs d =
 let trs' = permute_trs d trs in
 Trs.is_variant trs trs'
;;


(* check whether list of termpairs is permutable with respect
   to variable renaming mgu and permutation pi. Assume that pi
   is completely specified! *)
let rec permutable_inc mgu d = function
  | [] -> [mgu, d]
  | ((Term.Var y), (Term.Var x)) :: eqs ->
    begin
    try
     let z = Sub.find y mgu in
     if (Var.compare z x) == 0 then
      permutable_inc mgu d eqs
     else raise Not_permutable
    with Not_found ->
     (
     let s = Sub.singleton y x in
     permutable_inc (Sub.union mgu s) d eqs
     )
     end
  | (Term.Fun (c0, []), Term.Fun (c1, [])) :: eqs when
     (Fun.compare c0 c1) == 0 ->
     permutable_inc mgu d eqs
  | (Term.Fun (f0, [t0]), Term.Fun (f1, [t1])) :: eqs when
     (Fun.compare f0 f1) == 0 ->
     permutable_inc mgu d ((t0, t1) :: eqs)
  | (Term.Fun (f0, ts0), Term.Fun (f1, ts1)) :: eqs when
     (Fun.compare f0 f1) == 0 ->
    let n = List.length ts0 in
    let ps = try [plookup f0 d] with Not_found -> permutations n in
    let apply p =
     let ts0' = permute ts0 p in
     permutable_inc mgu ((f0, p) :: d) ((List.combine ts0' ts1) @ eqs)
    in
    List.concat (List.map apply ps)
  | _ -> raise Not_permutable
;;

let permutable_rules d rl rl' =
 let l, r = Rule.to_terms rl in
 let l', r' = Rule.to_terms rl' in
 permutable_inc (Sub.empty) d [l,l'; r,r'] 
;;


(********* functions for checking arbitrary processes equal *************)

(* theta is argument permutation *)
let possible_theta (rule,(i1,j1)) (rule',(i2,j2)) d =
 if (i1 <> i2) || (j1 <> j2) then []
 else
 try
  let sigma_d_set = permutable_rules d rule rule' in
  List.map snd (List.filter (is_bijective <.> fst) sigma_d_set)
 with Not_permutable -> []
;;



(********** match equation sets *****************************************)
let theta_set_for_eqn_pair (eqn,s) (eqn',s') theta =
 let rl = Equation.to_rule eqn in
 let rl' = Equation.to_rule eqn' in
 let rlr = possible_theta (rl,s) (rl',s') theta in
 let rrl = possible_theta (Rule.invert rl, Pair.flip s) (rl',s') theta in
 ((eqn',s'), rlr @ rrl)
;;

let rec match_equation_sets_for e e' theta =
 let eqn = List.hd e in
 let add tset eqn' = (theta_set_for_eqn_pair eqn eqn' theta) :: tset in
 let theta_e_set = List.fold_left add [] e' in
 let match_eqn (eqn', thetaset') =
  let e_red, e_red' = List.remove eqn e, List.remove eqn' e' in
  match_equation_sets e_red e_red' thetaset'
 in
 List.concat (List.map match_eqn theta_e_set)

and match_equation_sets e e' theta_set =
 if e = [] then theta_set
 else
  List.concat (List.map (match_equation_sets_for e e') theta_set)
;;

(********** match rule sets *********************************************)
let theta_set_for_rule_pair rule rule' theta =
 let thetaset =  possible_theta rule rule' theta in
  (rule', thetaset)
;;

let rec match_rule_sets_for e e' theta =
 let eqn = List.hd e in
 let add tset eqn' = (theta_set_for_rule_pair eqn eqn' theta) :: tset in
 let theta_e_set = List.foldl add [] e' in
 let match_eqn (eqn', thetaset') =
  let e_red, e_red' = List.remove eqn e, List.remove eqn' e' in
  match_rule_sets e_red e_red' thetaset'
 in
 List.concat (List.map match_eqn theta_e_set)
and match_rule_sets e e' theta_set =
 if List.is_empty e then
  theta_set
 else
  List.concat (List.map (match_rule_sets_for e e') theta_set)
;;


(********** match all ***************************************************)

(* the main function: gets two process states and checks whether their
   - current equation sets e, e'
   - current rewrite systems r, r'
   - current constraint systems c, c'
  are equal under some isomorphism theta (all type Trs.t).
  Boolean return value.                                                  *)
let processes_match (p, (e, r, c)) (p', (e', r', c')) =
 if not (TRen.cardinalities_match (e, r, c) (e', r', c')) then
  false
 else
  let theta_set_0 = [[]] in
  let theta_set = match_equation_sets e e' theta_set_0 in
  let theta_set' = match_rule_sets r r' theta_set in
  let theta_set'' = match_rule_sets c c' theta_set' in
  not (List.is_empty theta_set'')
;;

(* to check whether a set of equations can be non-trivially renamed such
 that it remains equivalent - checked at the beginning of a run; not a
 necessary condition (e.g. {f(a)=b, f(c)=f(a)}) but useful for current
 set of examples *)
let permutation_possible eqs =
 let theta_set = match_equation_sets eqs eqs [[]] in
 List.length theta_set > 0
;;

(*** TESTS ***************************************************************)

let test () =
 Format.printf "testing module TermPermutation ... \n";
 let (x,y,z), (c,f,g,h,k), _ = Termx.test_signature in
 let g_x_g_y_z = Term.Fun (g, [x; Term.Fun (g, [y; z])]) in
 let g_g_x_y_z = Term.Fun (g, [Term.Fun (g, [x; y]); z]) in
 let rule = Rule.of_terms g_x_g_y_z g_g_x_y_z in
 let trs = Trs.empty in
 let b = 
  try
   let _ = orientation_symmetric rule trs in true
  with Not_permutable -> false
 in
 assert b;
 let rule' = Rule.of_terms g_x_g_y_z x in
 assert (not (rule_symmetric rule'));
;;

(*test ()*)
