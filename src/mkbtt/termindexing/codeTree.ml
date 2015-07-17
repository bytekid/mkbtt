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

(**
@author Sarah Winkler
@since  2009/07/21 *)

(** Term indexing using code trees *)

(*** OPENS ********************************************************************)
open Util;;
(*** EXCEPTIONS **********************************************************)
exception No_back_pointer

exception Malformed_tree of string

exception Not_in_index

exception Empty_branch

(*** MODULES *************************************************************)
module Fun = Rewriting.Function;;
module Pos = Rewriting.Position;;
module Var = Rewriting.Variable;;
module T = U.Term;;
module M = U.Monad;;

open M;;

module Make (*: TermIndex.T with type entry = Entry.t*)
    = functor (Entry: TermIndex.ENTRY_TYPE) ->
   struct

(*** SUBMODULES **********************************************************)
module EL = TermIndex.EntryList(Entry);;

(*** TYPES ***************************************************************)
type entry = Entry.t

 type instruction =
  | Check of Fun.t * instruction * instruction
  | Put of int * instruction * instruction
  | Compare of int * int * instruction * instruction
  | Success of Entry.t list
  | Fail
 ;;

 type t = instruction

(* convenient for generalization retrievals *)
type flatterm =
 | Fun of Fun.t * flatterm * flatterm * T.t (* next, after, subterm here *)
 | Var of Var.t * flatterm (* next = after *)
 | End
;;


(*** GLOBALS *************************************************************)

(*** FUNCTIONS ***********************************************************)

let is_empty t = return (t == Fail)

let cont n = function
 | Check (_, c, _ )
 | Put (_, c, _ )
 | Compare (_, _, c, _ ) -> c
 | _ -> raise (Malformed_tree "cont does not exist")
;;

let back n = function
 | Check (_, _, b)
 | Put (_, _, b)
 | Compare (_, _, _, b) -> b
 | _ -> raise (Malformed_tree "cont does not exist")
;;

let set_back instruction b' =
 match instruction with
  | Check (f, c, b) -> Check (f, c, b')
  | Put (n, c, b) -> Put (n, c, b')
  | Compare (m, k, c, b) -> Compare (m, k, c, b')
  | _ -> raise (Malformed_tree "back does not exist")
;;

(* output code *)
let rec code_to_string c =
 match c with
  | Check(f, c, b) ->
   let cs, bs = code_to_string c, code_to_string b in
   "Check(" ^ (Fun.to_string f) ^ ", " ^ cs ^ ", " ^ bs ^ ")"
  | Put(k, c, b) ->
   let cs, bs = code_to_string c, code_to_string b in
   "Put(" ^ (string_of_int k) ^ ", " ^ cs ^ ", " ^ bs ^ ")"
  | Compare(m, k, c, b) ->
   let cs, bs = code_to_string c, code_to_string b in
   let sk, sm = string_of_int k, string_of_int m in
   "Compare(" ^ sm ^ ", " ^ sk ^ ", " ^ cs ^ ", " ^ bs ^ ")"
  | Success values -> "Success" ^ (List.join Entry.to_string " " values) 
  | Fail -> "Fail"
;;

let lookup table x i code =
 try
  let j = List.assoc x table in
  (Compare(j, i, Fail, Fail)) :: code, table
 with Not_found ->
  code, (x, i) :: table
;;

let rec code_list tcodes ccodes i table = function
 | T.Var x -> 
  let ccodes', table' = lookup table x i ccodes in
  (Put (i, Fail, Fail)) :: tcodes, ccodes', table', i + 1
 | T.Fun(f, ts) ->
  let tcodes' = (Check(f, Fail, Fail)) :: tcodes in
  List.fold_left app_tcode (tcodes', ccodes, table, i) ts
and app_tcode (tcodes, ccodes, table, i) t =
 code_list tcodes ccodes i table t
;;

let rec combine_code instruction = function
 | [] -> instruction
 | Check(f, _, _) :: l ->
  combine_code (Check(f, instruction, Fail)) l
 | Put(k, _, _) :: l ->
  combine_code (Put(k, instruction, Fail)) l
 | Compare(k, m,_,  _) :: l ->
  combine_code (Compare(k, m, instruction, Fail)) l
 | _ -> raise (Malformed_tree "Compare/Fail/Success not expected")
;;

let code_for_term t =
 let success = Success [] in 
 let tcode, ccode, _, _ = code_list [] [] 0 [] t in
 combine_code (combine_code success ccode) tcode
;;

let code_for_value (t, v) = 
 let success = Success [v] in
 let tcode, ccode, _, _ = code_list [] [] 0 [] t in
 combine_code (combine_code success ccode) tcode
;;

(* ****************** CONSTRUCTION OF CODE TREES ********************** *)
let make () = Fail

(* assume code is just code, not tree (otherwise, change case for 
   Success in tree *)
let rec insert' code tree =
  match code, tree with
   | _, Fail -> code
   | Check(f, c, _), Check(g, c', b') when (Fun.compare f g) == 0 ->
    Check(g, insert' c c', b')
   | Compare(m, k, c, _), Compare(m', k', c', b') when (k == k') && (m == m') ->
     Compare(m', k', insert' c c', b')
   | Put(k, c, _), Put(k', c', b') when k = k' ->
    Put(k, insert' c c', b')
   | _, Check(_, _, b)
   | _, Compare (_, _, _, b) 
   | _, Put (_, _, b) ->
    set_back tree (insert' code b)
   | Check(_, _, b), Success vs (* cases relevant? *)
   | Compare(_, _, _, b), Success vs
   | Put(_, _, b), Success vs ->
    set_back code (Success vs)
   | Success v, Success values ->
    Success (EL.union v values) (* variant *)
   | Fail, Success _ -> raise (Malformed_tree "Fail, Success not expected")
;;

(* add entry element into code tree *)
let insert tree (term, value) =
 (*T.to_stringm term >>= fun s ->
 Format.printf "Insert into index term %s\n%!" s;
 Format.printf "Tree before is %s\n" (code_to_string tree);*)
 let code = code_for_value (term, value) in
 let tree' = insert' code tree in
(* Format.printf "Code is %s\n" (code_to_string code);
 Format.printf "Tree is %s\n" (code_to_string tree');*)
 return tree'
;;

let rec remove_code code tree v =
  match code, tree with
   | Fail, _ -> raise (Malformed_tree "Fail in code not expected")
   | Check(f,c,_), Check(g,c',b') when (Fun.compare f g) == 0 ->
    (try Check(g, remove_code c c' v, b')
     with Empty_branch -> if b' != Fail then b' else raise Empty_branch)
   | Compare(m,k,c,_), Compare(m',k',c',b') when (k==k') && (m==m') ->
    (try Compare(m', k', remove_code c c' v, b')
     with Empty_branch -> if b' != Fail then b' else raise Empty_branch)
   | Put(k, c, b), Put(k', c', b') when k = k' ->
    (try Put(k', remove_code c c' v, b')
     with Empty_branch -> if b' != Fail then b' else raise Empty_branch)
   | _, Check(_, _, b)
   | _, Compare(_, _, _, b)
   | _, Put(_, _, b) ->
    (try set_back tree (remove_code code b v)
     with Empty_branch -> set_back tree Fail)
   | Success v, Success values -> 
    if (List.length values) == 1 then raise Empty_branch
    else Success (EL.diff values v) (* variant *)
   | _, Success _ -> raise (Malformed_tree "Success in tree not expected")
   | _ -> raise Not_in_index
;;

(* removes the value from the index. if not found, Not_in_index is raised *)
let delete tree value =
 (*T.to_stringm (fst value) >>= fun s ->
 Format.printf "Remove term %s\n%!" s;*)
 let code = code_for_value value in
 let tree' = try remove_code code tree value with Empty_branch -> Fail in
 return tree'
;;

(********* RETRIEVAL OPERATIONS ******************************************)

(***** VARIANTS *****)

let rec retrieve_variants tree code =
 match tree, code with
  | Check(f, c, b), Check(g, c', _) when (Fun.compare f g) == 0 ->
   retrieve_variants c c'
  | Compare(m, k, c, b), Compare(m', k', c', _) when (k == k') && (m == m') ->
   retrieve_variants c c'
  | Put(k, c, b), Put(k', c', _) when k = k' ->
   retrieve_variants c c'
  | Check(_, _, b), _
  | Compare(_, _, _, b), _
  | Put(_, _, b), _ ->
   retrieve_variants b code
  | Success variants, Success _ -> variants
  | Fail, _ 
  | Success _, _ -> []
;;

let variant_candidates tree term =
 let code = code_for_term term in
 let vars = retrieve_variants tree code in
(* U.Term.to_stringm term >>= fun s ->
 Format.printf "CT: vars 4 %s: %i:\n%s\n" s (List.length vars)
 (List.foldl (fun s x -> (Entry.to_string x)^s) "" vars);*)
 return vars
;;

(***** GENERALIZATIONS *****)

let rec flatten' after t = 
 match t with
  | T.Var x -> Var (x, after)
  | T.Fun(f, ts) ->
   let flat_ts = List.fold_right (fun t l -> flatten' l t) ts after in
   Fun(f, flat_ts, after, t) (* add t here, required in gen retrieve *)
;;

let flatten = flatten' End

let subst table i =
 try
  List.assoc i table
 with
  Not_found -> raise (Malformed_tree "compare without put")
;;

let rec retrieve_generalizations tree t_flat sub =
 match tree, t_flat with
  | Check(f, c, b), Fun(g, next, after, _) when (Fun.compare f g) == 0 ->
   let gens = retrieve_generalizations c next sub in
   EL.union (retrieve_generalizations b t_flat sub) gens
  | Compare(m, k, c, b), End ->
   let gens = retrieve_generalizations b End sub in
   if (compare (subst sub m) (subst sub k)) == 0 then
    EL.union (retrieve_generalizations c End sub) gens
   else
    gens
  | Put(k, c, b), Var (x, after) ->
   let subterm = T.Var x in
   let gens = retrieve_generalizations c after ((k, subterm) :: sub) in
   EL.union (retrieve_generalizations b t_flat sub) gens
  | Put(k, c, b), Fun (_, _, after, subterm) ->
   let gens = retrieve_generalizations c after ((k, subterm) :: sub) in
   EL.union (retrieve_generalizations b t_flat sub) gens
  | Check(_, _, b), _ ->
   retrieve_generalizations b t_flat sub
  | Success entries, End -> entries
  | Fail, _
  | Compare _, _
  | Success _, _ -> []
  | Put _, End -> raise (Malformed_tree "not malformed?")
;;

(* find generalizations for a given term in dtree *)
let generalization_candidates tree term =
 let t_flat = flatten term in
 let gens = retrieve_generalizations tree t_flat [] in
 return gens
;;

(***** ENCOMPASSMENTS *****)
(* given a term, non-var generalization of subterms are returned,
   paired with the subterm's position. Not strict! Also not possible
   as indexing destroys nonlinearity. *)
(*
let encompassment_candidates tree term =
 let pos_st = Termx.nonvar_pos_proper_subterms term in
 let ecs =
 List.fold_left
  (fun r (t, p) ->
   let gs = retrieve_generalizations tree (flatten t) [] in
   (List.map (fun n -> (n, p)) gs) @ r)
  [] ((term,Pos.root) :: pos_st)
 in
 return ecs
;;*)


(* given a term, non-var generalization of subterms are returned,
   paired with the subterm's position. Not strict! *)
let encompassment_candidates_below_root tree term =
 let pos_st = Termx.nonvar_pos_proper_subterms term in
 let ecs =
 List.fold_left
  (fun r (t, p) ->
   let gs = retrieve_generalizations tree (flatten t) [] in
   (List.map (fun n -> (n, p)) gs) @ r)
  [] pos_st
 in
 return ecs
;;

let encompassment_candidates tree term =
 let at_root = retrieve_generalizations tree (flatten term) [] in
 encompassment_candidates_below_root tree term >>= fun below ->
 let root = flip Pair.make Pos.root in
 return (List.rev_append (List.map root at_root) below)
;;

let size t = is_empty t >>= fun b -> return (if b then 0 else 1)

let overlap1_candidates t = failwith "CodeTree: overlaps not implemented"

let overlap1_candidates_below_root t = 
 failwith "CodeTree: overlaps not implemented"
;;

let overlap2_candidates t = failwith "CodeTree: overlaps not implemented"

let unification_candidates t = 
 failwith "CodeTree: unification not implemented"
;;



end (* Make *)

module TermCodeTree = Make(TermIndex.TermEntry)
(*
let test () =
 Format.printf "testing module CodeTree\n";
 let c = Fun.of_string "c" 0 in
 let f = Fun.of_string "f" 1 in
 let g = Fun.of_string "g" 2 in
 let x = Term.Var (Var.of_string "x") in
 let y = Term.Var (Var.of_string "y") in
 let f_x = Term.Fun (f, [x]) in
 let f_f_x = Term.Fun (f, [f_x]) in
 let c_ = Term.Fun (c, []) in
 let g_x_x = Term.Fun(g, [x; x]) in
 Format.printf "Code for %s: \n %s\n"
  (Term.to_string f_f_x)
  (TermCodeTree.code_to_string (TermCodeTree.code_for_value f_f_x));
 Format.printf "Code for %s: \n %s\n"
  (Term.to_string g_x_x)
  (TermCodeTree.code_to_string (TermCodeTree.code_for_value g_x_x));
 let g_f_f_x_c = Term.Fun (g, [f_f_x; c_]) in
 Format.printf "Code for %s: \n %s\n\n" 
  (Term.to_string g_f_f_x_c) 
  (TermCodeTree.code_to_string (TermCodeTree.code_for_value g_f_f_x_c));
 let g_f_f_x_f_x = Term.Fun (g, [f_f_x; f_x]) in
 let g_f_f_x_y = Term.Fun (g, [f_f_x; y]) in
 Format.printf "Code for %s: \n %s\n\n"
  (Term.to_string g_f_f_x_f_x)
  (TermCodeTree.code_to_string (TermCodeTree.code_for_value g_f_f_x_f_x));
 let t = Term.Fun (g, [g_f_f_x_f_x; y]) in
 let t' = Term.Fun (g, [g_f_f_x_f_x; g_x_x]) in
 Format.printf "Code for %s: \n %s\n\n"
  (Term.to_string t)
  (TermCodeTree.code_to_string (TermCodeTree.code_for_value t));
(* INSERT *)
 let tree =
  TermCodeTree.insert (TermCodeTree.code_for_value g_f_f_x_c) g_f_f_x_y 
 in
 Format.printf "Code for insert: \n %s\n\n"
  (TermCodeTree.code_to_string tree);
 let tree' = TermCodeTree.insert tree t in
 Format.printf "Code for insert: \n %s\n\n"
  (TermCodeTree.code_to_string tree');
 let g_f_f_y_c = Term.Fun (g, [Term.Fun (f, [Term.Fun (f, [y])]); c_]) in
 let tree' = TermCodeTree.insert tree' g_f_f_y_c in
 Format.printf "Code for insert g_f_f_y_c: \n %s\n\n"
  (TermCodeTree.code_to_string tree');
(* DELETE *)
 let tree'' = TermCodeTree.delete tree' g_f_f_y_c in
 Format.printf "Code for delete g_f_f_y_c again: \n %s\n\n"
  (TermCodeTree.code_to_string tree'');
(* Format.printf "Code for delete g_x_x: \n %s\n\n"
  (TermCodeTree.code_to_string (TermCodeTree.delete tree' g_x_x));*)
(* VARIANTS *)
 let variants = TermCodeTree.variant_candidates tree' g_f_f_x_f_x in
 let variants' = TermCodeTree.variant_candidates tree' g_f_f_x_y in
 Format.printf "variants for %s: %s,   %s: %s\n"
  (Term.to_string g_f_f_x_f_x)
  (List.to_string Term.to_string "" variants)
  (Term.to_string g_f_f_x_y)
  (List.to_string Term.to_string "" variants');
 let tree' = TermCodeTree.insert tree' t' in
(* GENERALIZATIONS *)
 let u = Term.Fun (g, [f_x; y]) in
 let tree' = TermCodeTree.insert tree' u in
 let gens = TermCodeTree.generalization_candidates tree' g_f_f_y_c in
 Format.printf "generalizations for %s: %s\n"
  (Term.to_string g_f_f_y_c)
  (List.to_string Term.to_string "" gens); (* ok *)
 let gens = TermCodeTree.generalization_candidates tree' u in
 Format.printf "generalizations for %s: %s\n"
  (Term.to_string u)
  (List.to_string Term.to_string "" gens); (* ok *)
 let s = Term.Fun (g, [f_x; x]) in
 let tree' = TermCodeTree.insert tree' s in
 let gens = TermCodeTree.generalization_candidates tree' g_f_f_x_f_x in
 Format.printf "generalizations for %s: %s\n"
  (Term.to_string g_f_f_x_f_x)
  (List.to_string Term.to_string "" gens);
(***** ENCOMPASSMENTS *****)
 let gens = TermCodeTree.encompassment_candidates_not_strict tree' t in
 let f (t, p) = (Term.to_string t) ^ "@" ^ (Position.to_string p) ^ "\n" in 
 Format.printf "encompassments for %s: %s\n"
  (Term.to_string t)
  (List.to_string f "" gens);
;;
*)
(* test ()*)
