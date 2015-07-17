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

(** Check for renaming isomorphisms
 @author Sarah Winkler
 @since  2009/07/07 *)

(*** OPENS ********************************************************************)
open Util;;
(*** SUBMODULES **********************************************************)
module Var = Rewriting.Variable;;
module Fun = Rewriting.Function;;
module Term = U.Term;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module Sub = U.Substitution;;
module E = Equation;;
module CP = CompletionProcessx;;
module Tx = Termx;;
module W = World;;
module Monad = W.Monad;;

(*** OPENS ***************************************************************)
open Monad;;

(*** SUBMODULES **********************************************************)
module SymbolSubstitution = struct
 include Replacement.Make (Fun) (Fun);;

 exception Not_injective;;
 let uncurry = Util.uncurry;;
 let replace x t sub = add x t sub;;
 let union sub sub' = fold (fun q t sub -> add q t sub) sub' sub;;
 
 (* singleton substitution mapping f to g *)
 let singleton f g = add f g empty;;

 (* apply a substitution to some other substitution *)
 let compose substitute symsub symsub' =
  fold
   (fun f g s -> replace f (substitute symsub g) s)
   symsub'
   empty
 ;;

 let is_injective t =
  let check _ f = function 
    None -> None 
   | Some d -> if List.mem f d then None else Some (f :: d)
  in
  match fold check t (Some []) with None -> false | _ -> true
 ;;

 let to_string s =
  fold
  (fun key data str -> 
    str ^ (Fun.to_string key) ^ ":" ^ (Fun.to_string data) ^ " ")
  s
  ""
 ;;

 let rec rename_term theta = function
  | Term.Var x as t -> t
  | Term.Fun (f, ts) -> 
   begin
    let f' = try find f theta with Not_found -> f in
    let ts' = List.map (rename_term theta) ts in
    Term.Fun (f', ts')
   end
 ;;

 let rename_rule theta = 
  (uncurry Rule.of_terms) <.> (Rule.map (rename_term theta))
 ;;

 let rename_trs trs theta = Trs.of_list (Trs.map (rename_rule theta) trs)

 let rename_equation theta e = E.map (rename_term theta) 

 let rename_eqs eqs theta = List.map (rename_equation theta) eqs
 end

module SymSub = SymbolSubstitution;;

(*** EXCEPTIONS **********************************************************)
exception Substitution_inconsistent;;
exception Not_renamable;;

(*** FUNCTIONS ***********************************************************)

let uncurry = Util.uncurry;;

let var = function
 | Term.Var x -> x
 | _ -> failwith "trsRenaming: Not a variable"
;;

(* check whether two terms are variants with respect to a variable
   renaming mgu and a symbol renaming theta. mgu and theta are extended
   if consistent and returned as a pair *)
 let rec variants mgu theta = function
  | [] -> (mgu, theta)
  | ((Term.Var y), (Term.Var x)) :: eqs ->
    begin
    try
     let z = var (Sub.find y mgu) in
     if (Var.compare z x) == 0 then
      variants mgu theta eqs
     else raise Not_renamable
    with Not_found ->
     (
     let s = Sub.singleton y (Term.make_var x) in
     variants (Sub.union mgu s) theta eqs
     )
     end
  | (Term.Fun (f0, ts0), Term.Fun (f1, ts1)) :: eqs  ->
   if (List.length ts0) == (List.length ts1) then
    begin
    try
     let g = SymSub.find f0 theta in 
     if (Fun.compare g f1) == 0 then
      let eqs' = (List.combine ts0 ts1) @ eqs in
      variants mgu theta eqs'
     else raise Not_renamable
    with Not_found ->
     (
     let s = SymSub.singleton f0 f1 in
     let eqs' = (List.combine ts0 ts1) @ eqs in
     variants mgu (SymSub.union s theta) eqs')
    end
   else
    raise Not_renamable
  | _ -> raise Not_renamable
;;

let term_variants s t mgu theta = 
 let (sigma, theta) = variants mgu theta [(s, t)] in
 if (SymSub.is_injective theta) && (Sub.is_bijective sigma) then
  (sigma, theta)
 else
  raise Not_renamable
;;

let terms_are_variants s t = 
 try
  let sub = Sub.empty in
  let symsub = SymSub.empty in
  let _ = term_variants s t sub symsub in
  true
 with
  | Not_renamable -> false
;;

let rule_variants rl1 rl2 theta =
 let sigma = Sub.empty in (* variable renaming *)
 let l1, r1 = Rule.lhs rl1, Rule.rhs rl1 in (* check lhs *)
 let l2, r2 = Rule.lhs rl2, Rule.rhs rl2 in (* check rhs *)
 let sigma', theta' = term_variants l1 l2 sigma theta in
 snd (term_variants r1 r2 sigma' theta') (* return theta'' *)
;;

let rule_pattern_variants (rl1,(i1,j1)) (rl2,(i2,j2)) =
 if (i1 <> i2) || (j1 <> j2) then raise Not_renamable
 else rule_variants rl1 rl2
;;

let rules_are_variants r1 r2 =
 try let _ = rule_variants r1 r2 SymSub.empty in true
 with Not_renamable -> false
;;

(* returns a symbol renaming if a rule is isomorphic to its invert,
   otherwise raises Not_renamable *)
let rule_symmetry rule theta = 
 rule_variants rule (Rule.invert rule) theta

let rule_symmetric rule = 
 try
  let theta = SymSub.empty in 
  let t = rule_symmetry rule theta in ignore t; true
 with
  | Not_renamable -> false
;;

let orientation_symmetric rule trs =
  let theta = SymSub.empty in
  (* try to find renaming matching the rule with its invert *)
  let theta = rule_variants rule (Rule.invert rule) theta in
  (* apply renaming to trs *)
  let trs' = SymSub.rename_trs trs theta in
  (* check if resulting trs is equal to original one - in this case
     the two rule orientations are redendant *)
  if Trs.equivalent trs trs' then theta else 
   raise Not_renamable
;;

(*********** check equality of equation sets ****************************)

let remove_eqn rule trs =
  if Trs.mem rule trs then
   Trs.remove rule trs
  else if Trs.mem (Rule.invert rule) trs then
   Trs.remove (Rule.invert rule) trs
  else raise Not_found
;;

(*
(* eqn is actually trs *)
let equations_invariant eqn theta =
 let eqnren = SymSub.rename_eqs eqn theta in
 equal_as_equations eqn eqnren
;;

let exists_rule_variant trs rule =
 Trs.exists (Rule.is_variant rule) trs
;;

let equal_as_trs trs1 trs2 =
  Trs.for_all (exists_rule_variant trs1) trs2
;;
*)

let trs_invariant trs theta =
 let trsren = SymSub.rename_trs trs theta in
 Trs.equivalent trs trsren
;;

let eqns_invariant eqns =
 let rs = List.map ((uncurry Rule.of_terms) <.> E.terms) eqns in
 let trs = Trs.of_list rs in
 trs_invariant trs
;;

(********* functions for checking arbitrary processes equal *************)

(* return option on possible extension of symbol substitution theta that
   makes eqn isomorphic to eqn' (checks only one direction)             *)
let possible_theta rule rule' theta =
 try let theta' = rule_pattern_variants rule rule' theta in [theta']
 with Not_renamable -> []
;;


(* checks if two rules are isomorphic under theta, basically achieved by
   the function f. For TrsRenaming, this is possible_theta *)
let theta_set_for_rule_pair rule rule' (theta : SymSub.t) =
 let thetaset =  possible_theta rule rule' theta in
  (rule', thetaset)
;;

(* returns set of possible extensions of theta' of theta such that e is
   isomorphic to e' under theta'. Assumes |e|=|e'| > 0                  *)
let rec match_rule_sets_for e e' (theta : SymSub.t) =
 let eqn = List.hd e in
 let add tset eqn' = (theta_set_for_rule_pair eqn eqn' theta) :: tset in
 let theta_e_set = List.foldl add [] e' in
 let match_eqn (eqn', thetaset') =
  let e_red, e_red' = List.remove eqn e, List.remove eqn' e' in
  match_rule_sets e_red e_red' thetaset'
 in
 List.concat (List.map match_eqn theta_e_set)
(* theta_set is a set of function symbol renamings. This function filters
   all theta \in theta_set which can be extended to theta' such that e and
   e' are isomorphic under theta'.
   Assumes |e|=|e'|.
   Returns set of all possibilities for theta'.                         *)
and match_rule_sets e e' theta_set =
 if List.is_empty e then
  theta_set
 else
  List.concat (List.map (match_rule_sets_for e e') theta_set)
;;

(* and the same again for equations *)

(* checks if two equations are isomorphic under theta, basically 
   achieved by the function f. 
   For TrsRenaming, this is possible_theta                              *)
let theta_set_for_eqn_pair (eqn,s) (eqn',s') theta =
 let rl = E.to_rule eqn in
 let rl' = E.to_rule eqn' in
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
 if e = [] then (if e' = [] then theta_set else [])
 else
  List.concat (List.map (match_equation_sets_for e e') theta_set)
;;

let equation_sets_renamable es es' =
 let es = List.map (fun e -> e,(0,0)) es in
 let es' = List.map (fun e -> e,(0,0)) es' in
 if (List.length es) != (List.length es') then []
 else match_equation_sets es es' [SymSub.empty]
;;


(********** match all ***************************************************)

(* check whether |e|=|e'| && |r|=|r'| && |c|=|c'|                       *)
let cardinalities_match (e, r, c) (e', r', c') =
 ((List.length e) == (List.length e')) &&
 ((List.length r) == (List.length r')) &&
 ((List.length c) == (List.length c'))
;;
(* Claim: if two processes (under the same selection startegy) have
   different lengths (i.e., oriented a different number of equations
   with non-unique orientation, they are not isomorphic 
   WRONG !!! see CGE *)
let lengths_match p p' = CP.length p = CP.length p'


(* the main function: gets two process states and checks whether their
   - current equation sets e, e' 
   - current rewrite systems r, r'
   - current constraint systems c, c'
  are equal under some isomorphism theta (all type Trs.t). 
  Boolean return value.                                                  *)
let processes_match (p, (e, r, c)) (p', (e', r', c')) =
 if 
  (* lengths match is not true !!! *)
  not ((cardinalities_match (e, r, c) (e', r', c')))
 then return false
 else (
   let theta_set = match_rule_sets r r' [SymSub.empty] in
   let theta_set' = match_rule_sets c c' theta_set in
   let theta_set'' = match_equation_sets e e' theta_set' in
   let b = not (List.is_empty theta_set'') in
   return b)
;;

(* to check whether a set of equations can be non-trivially renamed such
 that it remains equivalent - checked at the beginning of a run; not a
 necessary condition (e.g. {f(a)=b, f(c)=f(a)}) but useful for current
 set of examples *)
let renaming_possible eqs =
 let theta_set = match_equation_sets eqs eqs [SymSub.empty] in
 (*Format.printf "There are %i renamings\n%!" (List.length theta_set);*)
 List.length theta_set > 1(*
 else 
  let symm e = 
   try let _ = orientation_symmetric e Trs.empty in true 
   with Not_renamable -> false
  in
  List.exists symm (List.map E.to_rule eqs)*)
;;

(*** TESTS ***************************************************************)

let test () =
 Format.printf "testing module TrsRenaming ... \n";
 let (x,y,z), (c,f,g,h,k), sigma = Tx.test_signature in
 let const = Term.Fun (c, []) in
 let f_x = Term.Fun (f, [x]) in
 let f_f_x = Term.Fun (f, [f_x]) in
 let h_h_c = Term.Fun (h, [Term.Fun (h,[const])]) in
 let h_h_x = Term.Fun (h, [Term.Fun (h, [x])]) in
 let h_x = Term.Fun (h, [x]) in
 let h_h_y = Term.Fun (h, [Term.Fun (h, [y])]) in
 let f_h_y = Term.Fun (f, [Term.Fun (h, [y])]) in
 let h_f_y = Term.Fun (h, [Term.Fun (f, [y])]) in
 let g_x_x = Term.Fun (g, [x; x]) in
 let g_x_y = Term.Fun (g, [x; y]) in
 let k_z_z = Term.Fun (k, [z; z]) in
 let g_f_x_h_h_x = Term.Fun (g, [f_x; h_h_x]) in
 let k_h_x_h_h_x = Term.Fun (k, [h_x; h_h_x]) in
 let r1 = Rule.of_terms f_f_x f_x in
 let r2 = Rule.of_terms h_h_x h_x in
 let r2' = Rule.of_terms h_h_y h_x in
 let r3 = Rule.of_terms f_h_y h_f_y in
 let r4 = Rule.of_terms g_x_x const in
 let trs = Trs.of_list [r1; r2; r4] in 
(*** Some asssertions ****************************************************)
 assert (terms_are_variants h_h_x h_h_y);
 assert (terms_are_variants h_h_x f_f_x);
 assert (terms_are_variants h_f_y f_h_y);
 assert (not (terms_are_variants h_h_c f_f_x));
 assert (not (terms_are_variants g_x_x f_x));
 assert (terms_are_variants g_x_x k_z_z);
 assert (not (terms_are_variants g_x_y k_z_z));
 assert (not (terms_are_variants g_f_x_h_h_x k_h_x_h_h_x));
 assert (not (rules_are_variants r1 r2'));
 assert (rules_are_variants r3 (Rule.invert r3));
 let b = 
 try 
  let _ = orientation_symmetric r3 trs in true 
 with Not_renamable -> false
 in
 assert b;
 let eqns1 = [E.of_terms g_x_x x, (0,1)] in
 let eqns2 = [E.of_terms k_z_z z, (0,1)] in
(* test if processes_match checks equations_match, symmetric *)
 let s = W.initial_context in
 let processes_match' p p' = 
  Either.right (U.Monad.run sigma (Monad.run s 
   (processes_match ([], p) ([], p'))) )
 in
 assert (processes_match' (eqns1, [], []) (eqns2, [], []));
 let eqns1' = [E.of_terms g_x_y x, (0,1)] in
(* nonlinearity substitution *)
 assert (not (processes_match' (eqns1', [], []) (eqns2, [], [])));
 let rs1 = [Rule.of_terms g_x_x x, (0,1)] in
 let rs2 = [Rule.of_terms k_z_z z, (0,1)] in
(* check if theta from eqns is verified in rules *)
 assert (processes_match' (eqns1, rs1, []) (eqns2, rs2, []));
 let cs1 = (Rule.of_terms f_x x, (2,1)) :: rs1 in
 let cs2 = (Rule.of_terms h_x x, (2,1)) :: rs2 in
(* check if theta is extended in rules *)
 assert (processes_match' (eqns1, rs1, cs1) (eqns2, rs2, cs2));
 let g_f_x_f_f_x = Term.Fun (g, [f_x; Term.Fun(f, [f_x])]) in
 let cs1' = (Rule.of_terms k_h_x_h_h_x f_x, (3,2)) :: cs1 in
 let cs2' = (Rule.of_terms g_f_x_f_f_x h_x, (3,2)) :: cs2 in
(* check if theta is extended in rules *)
 assert (processes_match' (eqns1, rs1, cs1') (eqns2, rs2, cs2'));
 assert (processes_match' (eqns1, cs1', rs1) (eqns2, cs2', rs2))
;;

(*test ();;*)
