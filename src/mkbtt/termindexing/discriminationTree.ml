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

(** Implementation of Indexing with Discrimination Trees
@author Sarah Winkler
@since  2008/11/26 *)

(** Provides data structure and functions for term indexing *)

(*** SUBMODULES **********************************************************)
module Fun = Rewriting.Function;;
module Var = Rewriting.Variable;;
module Pos = Rewriting.Position;;
module Term = U.Term;;
module M = U.Monad;;
module PStringLabel = PString.Label;;
module SymbolMap = Map.Make(PStringLabel);;
(*** OPENS ********************************************************************)
open Util;;
open M;;

(*** MODULES *************************************************************)
module Make (*: TermIndex.T with type entry = Entry.t*)
    = functor (Entry: TermIndex.ENTRY_TYPE) ->
   struct
(*** SUBMODULES **********************************************************)
module EL = TermIndex.EntryList(Entry);;

(*** TYPES ***************************************************************)
type entry = Entry.t

(* the type definition *)
type children = {
  next: node;
  jumps: node list
}
and node =
  | Leaf of int * entry list
  | Node of int * (children SymbolMap.t)
 ;;

type t = node * int

(*** EXCEPTIONS **********************************************************)
exception Malformed_pstring

exception Empty_subtree

exception Not_in_index

(*** FUNCTIONS ***********************************************************)
let next_of n = n.next

let jumps_of n = n.jumps 

let id_of = function
 | Leaf (i, _)
 | Node (i, _) -> i
;;

let rec jump_string = function
 | [] -> ""
 | [j] -> string_of_int (id_of j)
 | j :: js -> (string_of_int (id_of j)) ^ ", " ^ (jump_string js)
;;

let rec to_string = function
 | Leaf (i, s) -> "(" ^ (string_of_int i) ^"): " ^ (List.to_string Entry.to_string "" s)
 | Node(i, n) ->
    "<" ^ (string_of_int i) ^ ">: \n" ^
    (SymbolMap.fold
     (fun k v s ->
      let fs = PStringLabel.to_string k in
      let vs = to_string (next_of v) in
      fs ^ " -> ([" ^ vs ^ "], " ^ (jump_string (jumps_of v)) ^ "), " ^ s
     )
     n
     "</" ^ (string_of_int i) ^ ">\n")
;;

(* functions for tikz output *)

let rec to_tikz' = function
 | Leaf (i, s) ->
  let content = List.to_string Entry.to_string "" s in
  " node[label={[leaf]below:$" ^ content ^ "$}](" ^ (string_of_int i) ^"){" ^ (string_of_int i) ^"}"
 | Node(i, n) ->
   "node[small] (" ^ (string_of_int i) ^") {" ^ (string_of_int i) ^"}\n" ^
    SymbolMap.fold
     (fun k v s ->
      let fs = PStringLabel.to_string k in
      let vs = to_tikz' (next_of v) in
      "child {\n" ^ vs ^ " edge from parent node[symbol] {" ^ fs ^ "}}\n" ^ s
     )
     n
    ""
;;

let tikz_jump_from i = function
 | Leaf _ -> "" (* jump links to leaves are not interesting *)
 | Node(j, _) -> 
 "\\draw[jump] (" ^ (string_of_int i) ^ ") -- (" ^ (string_of_int j) ^ ");\n"
;;

let rec tikz_jumps = function
 | Leaf _ -> ""
 | Node(i, n) ->
  SymbolMap.fold
     (fun k v s ->
      let n, j = next_of v, jumps_of v in
      let h = List.fold_left (fun r j -> (tikz_jump_from i j) ^ r) "" j in
      let d = tikz_jumps n in
      h ^ d ^ s
     )
     n
     ""
;;

let to_tikz dtree =
 let treestring =
 match dtree with 
  | Leaf (i, s) -> "\node(" ^ (string_of_int i) ^") {};"(* ^ (Setx.to_string Entry.to_string s)*)
  | Node(i, n) ->
   "\\node[scale=0.5] (" ^ (string_of_int i) ^") {}\n" ^
    SymbolMap.fold
     (fun k v s ->
      let fs = PStringLabel.to_string k in
      let vs = to_tikz' (next_of v) in
      "child {" ^ vs ^ " edge from parent node[symbol] {" ^ fs ^ "} }\n" ^ s
     )
     n
    ";\n"
 in
 let style = 
  "\\tikzstyle{symbol}=[scale=0.6]\n\\tikzstyle{leaf}=[scale=0.6]\n" ^
  "\\tikzstyle{jump}=[->, dotted]\n\\tikzstyle{small}=[scale=0.5]"
 in
 let jumpstring = tikz_jumps dtree in
 style ^ treestring ^ jumpstring
;;

(********* CONSTRUCT DISCRIMINATION TREE *********************************)

let make_children child jumpchildren = {
 next = child;
 jumps = jumpchildren
 }

let node i = Node (i, SymbolMap.empty)

let empty = (node 0, 1)

let make () = empty

(* returns function to construct next tree element depending on next label
   in pstring to insert *)
let get_next_element i p_next table f =
 try 
  let children = SymbolMap.find f table in
  next_of children, jumps_of children, i
 with Not_found -> ( 
  match p_next with
   | PString.Nil -> Leaf (i, []), [], i+1
   | _ -> node i, [], i+1
 )
;;


(* extended arity function to determine jump link target below *)
let arity s = 
 match s with 
 | PStringLabel.Star -> return 0
 | PStringLabel.Symbol f -> find_ari f
;;

let rec drop n xs =
 if n = 0 then xs else 
 match xs with
  | [] -> failwith "empty list in drop"
  | z :: zs -> drop (n-1) zs
;;

(* adds a jumplink j to a list of jump links js. if a link to
   a node with this id is already present, replace *)
let add_nonredundant j js =
 let rec add_nonredundant j acc = function
  [] -> j :: acc
  | j' :: js when (id_of j) = (id_of j') -> j :: js @ acc
  | j' :: js -> add_nonredundant j (j' :: acc) js
 in
 add_nonredundant j [] js
;;

(* insert pstring into discrimination tree *) 
(* returns (top, after) where top is the topmost node of the extended tree
   and after is a list of possible jump link targets (treated like a 
   stack) *)
let rec insert_pstring k dtree pstring value =
 match dtree, pstring with
(* if leaf reached, simply add value to set *)
  | Leaf(i, set), PString.Nil -> 
    let leaf = Leaf (i, EL.add value set) in return (leaf, [leaf], k)
  | Node(i, n), PString.Element(f, p_next, _) ->
    (* pick appropriate table entry or create new node *)
    let child, jumps, k = get_next_element k p_next n f in
    (* recursively insert remaining pstring *) 
    insert_pstring k child p_next value >>= fun (child', jumpchildren, k) ->
    (* pick appropriate jump target *)
    arity f >>= fun a ->
    let jumpchildren = drop a jumpchildren in
    let jump = List.hd jumpchildren in
    (* create new children for f *)
    let jumps' = add_nonredundant jump jumps in
    let cs = make_children child' jumps' in
    (* update table of current node *)
    let n = SymbolMap.add f cs n in 
    (* return new node n and add n to jump targets *)
    let node = Node(i, n) in
    return (node, node :: jumpchildren, k)
  | _ -> raise Malformed_pstring
;;

(* add entry element into discrimination tree *)
let insert (dtree, k) (term, value) =
 let pstring = PString.of_term term in
 insert_pstring k dtree pstring value >>= fun (dtree',_, k) ->
 return (dtree', k)
;;

let size (i:t) = return (snd i)

let is_empty i = size i >>= fun s -> return (s=1)

(********* DESTRUCT DISCRIMINATION TREE *********************************)

let find_children table f =
 try
  let children = SymbolMap.find f table in
  next_of children, jumps_of children
 with
  | Not_found -> raise Not_in_index
;;

(* auxiliary functions for debugging
let contains haystack needle =
 let ln = String.length needle in
 let rec contains i =
  if i > ((String.length haystack) - ln) then
   false
  else
   (String.compare needle (String.sub haystack i ln) = 0) ||
   contains (i + 1)
 in
 contains 0
;;

 let contains208 = function
  | Leaf (_,set) ->
   let s = Setx.to_string Entry.to_string set in
   contains s "208"
  | Node _ -> false
;;

let agent208 table =
 let agent f _ =
  let _, js = find_children table f in
  List.exists contains208 js
 in
 SymbolMap.fold (fun k v b -> (agent k v) || b) table false
;;
*)

let update_jumps (j, exists) js =
 if exists then 
  add_nonredundant j js (* just replace old with updated target *)
 else (* have to remove j from js *)
  let rec remove x acc = function
   | [] -> Format.printf "should not happen\n"; acc
   | y :: xs when (id_of x) = (id_of y) -> List.rev_append xs acc 
   | y :: xs -> remove x (y :: acc) xs
  in
  remove j [] js
;;


(* remove pstring from discrimination tree;
   returns tuple of updated tree and jumpstack, where elements
   on jumpstack are of form (node, b) where the second component
   indicates whether this node still exists *) 
let rec remove_pstring dtree pstring value =
 match dtree, pstring with
  | Leaf(i, set), PString.Nil ->
    if EL.mem value set then
     let set' = EL.remove value set in
     if [] = set' then 
      return (None, [(dtree, false)])
     else 
      let leaf = Leaf(i, set') in
      return (Some leaf, [(leaf, true)])
    else raise Not_in_index
  | Node(i, table), PString.Element(f, pstring', _) -> (
    (* determine successors *)
    let child, jumps = find_children table f in
    (* recursively remove *)
    remove_pstring child pstring' value >>= fun (treeoption, jumpstack) ->
    (* update jumpstack *)
    arity f >>= fun a ->
    let jumpstack = drop a jumpstack in
    match treeoption with
     | Some child' ->
      let jump = List.hd jumpstack in
      (* update jump list of this node *)
      let jumps' = update_jumps jump jumps in
      (* create new children record *)
      let children' = make_children child' jumps' in
      (* update table of current node *)
      let table = SymbolMap.add f children' table in
      let node = Node(i, table) in
      return (Some node, (node, true) :: jumpstack)
     | None -> (* binding in node for f gets removed *)
      let table = SymbolMap.remove f table in
      if SymbolMap.is_empty table then 
       return (None, (dtree, false) :: jumpstack) (* this node is removed *)
      else
       let node = Node(i, table) in
       return (Some node, (node, true) :: jumpstack) (* return updated node *)
   )
  | _ -> raise Malformed_pstring
;; 

(* removes the value from the index. if not found, Not_in_index is raised *)
let delete (dtree, k) (term, value) =
 let pstring = PString.of_term term in
 remove_pstring dtree pstring value >>= fun (dtreeoption, _) ->
 match dtreeoption with
  | None -> return empty (* DTree got empty *)
  | Some tree -> return (tree, k)
;;

(********* RETRIEVAL OPERATIONS ******************************************)

(***** VARIANTS *****)

let rec retrieve_variants dtree pstring = 
 match dtree, pstring with
  | Leaf(_, set), PString.Nil -> set
  | Node(_,table), PString.Element(f, pstring', _) -> 
   (try
    let dtree, _ = find_children table f in
    retrieve_variants dtree pstring'
   with Not_in_index -> [])
  | _ -> raise Malformed_pstring
;; 

let variant_candidates (dtree, _) term =
 let pstring = PString.of_term term in
 let v = retrieve_variants dtree pstring in
 return v
;;


(***** GENERALIZATIONS *****)

let rec retrieve_generalizations dtree pstring =
 match dtree, pstring with
  | Leaf(_, set), PString.Nil -> set
  | Node(_, table), PString.Element(f, next, after) ->
   EL.union 
    (gen_from_node table f next) 
    (gen_from_node table PStringLabel.Star after)
  | _ -> raise Malformed_pstring
and gen_from_node table f pstring =
 try
  let dtree, _ = find_children table f in
  retrieve_generalizations dtree pstring
 with Not_in_index -> []
;;

(* find generalizations for a given term in dtree *)
let generalization_candidates (dtree, _) term =
 let pstring = PString.of_term term in
 let v = retrieve_generalizations dtree pstring in
 return v
;;

(***** ENCOMPASSMENTS *****)

(* given a term, non-var generalization of subterms are returned,
   paired with the subterm's position. Not strict! Also not possible
   as indexing destroys nonlinearity. *)
let encompassment_candidates (dtree, _) term =
 let pos_st = Termx.nonvar_pos_proper_subterms term in
 let ecs =
 List.fold_left (fun r (t, p) ->
  let cs = retrieve_generalizations dtree (PString.of_term t) in
  EL.pair_union r (List.map (fun n -> (n, p)) cs))
   []
   ((term,Pos.root) :: pos_st)
 in
 return ecs
;;

(*****)


(* given a term, non-var generalization of subterms are returned,
   paired with the subterm's position. Not strict! Also not possible
   as indexing destroys nonlinearity. *)
let encompassment_candidates_below_root (dtree, _) term =
 let pt = Termx.nonvar_pos_proper_subterms term in
 let ecs = List.fold_left
  (fun r (t, p) ->
   let cs = retrieve_generalizations dtree (PString.of_term t) in
   EL.pair_union r (List.map (fun n -> (n, p)) cs))
   [] pt
 in
 return ecs
;;

(***** UNIFIABLE TERMS *****)

let union f xs =
 let rec union f s = function
  | [] -> s
  | x :: xs -> union f (EL.union (f x) s) xs
 in 
 union f [] xs
;;

let rec retrieve_unifiable_terms dtree pstring =
 match dtree, pstring with
  | Leaf(_, set), PString.Nil -> set
  | Node(_, table), PString.Element(f, next, after) ->
   let fmatches = unif_from_node table f next in
   let starmatches = unif_from_node table PStringLabel.Star after in
   if f = PStringLabel.Star then
    let jumps = jumps_from_node table f after in
    EL.union fmatches (EL.union starmatches jumps)
   else
    EL.union fmatches starmatches
  | _, PString.Nil -> failwith "nil" 
  | _ -> raise Malformed_pstring
and unif_from_node table f pstring =
 try
  let dtree, _ = find_children table f in
  retrieve_unifiable_terms dtree pstring
 with Not_in_index -> []
and jumps_from_node table f pstring =
 let acc k v s =
   let _, js = find_children table k in
   let junifs = union (fun n -> retrieve_unifiable_terms n pstring) js in
   EL.union s junifs
 in
 SymbolMap.fold acc table [] 
;;

(* find unifiable terms for a given term in dtree *)
let unification_candidates (dtree, _) term =
 let pstring = PString.of_term term in
 let v = retrieve_unifiable_terms dtree pstring in
 return v
;;

(* given a term, all items unifiable with proper subterms are
   returned, paired with the subterm's position *)
let overlap1_candidates (dtree, _) term =
 let pos = Termx.nonvar_pos_proper_subterms term in
 let res = List.fold_left
  (fun r (t, p) ->
   let uc = retrieve_unifiable_terms dtree (PString.of_term t) in
   EL.pair_union r (List.map (fun n -> (n, p)) uc))
   [] ((term,Pos.root) :: pos)
 in
 return res
;;

(* given a term, all items unifiable with proper subterms are
   returned, paired with the subterm's position *)
let overlap1_candidates_below_root (dtree, _) term =
 let pos = Termx.nonvar_pos_proper_subterms term in
 let res = List.fold_left
  (fun r (t, p) -> 
   let uc = retrieve_unifiable_terms dtree (PString.of_term t) in
   EL.pair_union r (List.map (fun n -> (n, p)) uc))
   [] pos
 in
 return res
;;

(* OVERLAPS 2 *)

let map_union f t =
 SymbolMap.fold (fun k v s -> EL.union (f k v) s) t []
;; 

let rec collect_leaves = function
 | Leaf(_, set) -> set
 | Node(_, table) ->
  map_union (fun _ v -> collect_leaves (next_of v)) table
;;

(* the following function is a copy of retrieve_unifiable, except that if 
   a pstring is exhausted and non-leaf node n is reached, all terms 
   attached to leaves in the subtree starting at n are returned. *)
let rec retrieve_unif_subterms dtree pstring =
 let res = 
 match dtree, pstring with
  | Leaf(i, set), PString.Nil -> set
  | Node(i, table), PString.Element(f, next, after) ->
   let fmatches = unif_subterms_from_node table f next in
   let stars = unif_subterms_from_node table PStringLabel.Star after in
   if f = PStringLabel.Star then
    let jumps = jump_sub_from_node table f after in
    EL.union fmatches (EL.union stars jumps)
   else
    EL.union fmatches stars
  | n, PString.Nil -> 
   collect_leaves dtree
  | _ -> (*raise Malformed_pstring*) []
 in
 res
and unif_subterms_from_node table f pstring =
 try
  let dtree, _ = find_children table f in
  retrieve_unif_subterms dtree pstring
 with Not_in_index -> []
and jump_sub_from_node table f pstring =
 let acc k v s =
   let _, js = find_children table k in
   let junifs = List.fold_left 
   (fun r n -> EL.union r (retrieve_unif_subterms n pstring)) [] js in
   EL.union s junifs
 in
 SymbolMap.fold acc table []
;;

(* copy of above function avoiding variable retrievals *)
let rec retrieve_unif_nonvar_subterms dtree pstring =
 let res =
 match dtree, pstring with
  | Leaf(_, set), PString.Nil -> set
  | Node(i, table), PString.Element(f, next, after) ->
   unif_subterms_from_node table f next
  | n, PString.Nil ->
   collect_leaves dtree
  | _ -> []
 in
 res
;;


(* second argument is never empty *)
let adapt_arities p = function
   [] -> failwith "empty list in adapt arities"
 | 0 :: ars -> 
  let rec collapse p ars = 
   match ars with
    [] -> failwith "empty list in collapse"
    | a :: [] -> p, [] (* dont care whats returned here, i think *)
    | 0 :: 1 :: ars -> 
     let p', _ = Pos.split_last p in
     collapse p' (0 :: ars)
    | 0 :: a :: ars -> (* no more collapses *)
     let p', i = Pos.split_last p in
     Pos.add_last (i+1) p', (a - 1) :: ars
    | _ -> failwith "_ :: _ :: _ in collapse"
   in  collapse p (0 :: ars)
 | ars -> Pos.add_last 0 p, ars
;;

let rec retrieve_context_unifiable tree pstring p ars =
 let collect k v m =
  m >>= fun s -> 
  collect_unifs_from pstring p ars k v >>= fun cs ->
  return (EL.pair_union cs s)
 in
 match tree with
  | Leaf _ -> return []
  | Node(i, n) ->
   let matched_here = retrieve_unif_nonvar_subterms tree pstring in 
   let matched_here' = List.map (fun t -> (t, p)) matched_here in
   SymbolMap.fold collect n (return []) >>= fun below ->
   return (EL.pair_union below matched_here')
and collect_unifs_from pstring p ars f fchildren =
 (* adapt ars stack *)
 arity f >>= fun a ->
 let p', ars' = adapt_arities p (a :: ars) in
 let next, js = next_of fchildren, jumps_of fchildren in
 retrieve_context_unifiable next pstring p' ars'
;;

let overlap2_candidates (dtree, _) term =
 let pstring = PString.of_term term in
 let result = 
  retrieve_context_unifiable dtree pstring Pos.root []
 in
 result
;;

end (* Make *)

(*** TESTS ***************************************************************)

module TermDTree = Make(TermIndex.TermEntry)
(*
let test () =
 Format.printf "testing module DiscriminationTree\n";
 let one = Fun.of_string "one" 0 in
 let i = Fun.of_string "i" 1 in
 let m = Fun.of_string "m" 2 in
 let b = Fun.of_string "b" 1 in
 let c = Fun.of_string "c" 1 in
 let x = Term.Var (Var.of_string "x") in
 let y = Term.Var (Var.of_string "y") in
 let c_b_y = Term.Fun (c, [Term.Fun(b, [y])]) in
 let b_c_c_b_y = Term.Fun (b, [Term.Fun(c, [c_b_y])]) in
 let c_c_x = Term.Fun (c, [Term.Fun(c, [x])]) in
 let c5_x = Term.Fun (c, [Term.Fun(c, [Term.Fun(c, [c_c_x])])]) in
 let c2_b_c5_x = Term.Fun (c, [Term.Fun(c, [Term.Fun(b, [c5_x])])]) in
 let i_x = Term.Fun (i, [x]) in
 let i_i_x = Term.Fun (i, [i_x]) in
 let one_ = Term.Fun (one, []) in
 let m_x_o = Term.Fun (m, [x; one_]) in
 let m_y_o = Term.Fun (m, [y; one_]) in
 let m_x_y = Term.Fun (m, [x; y]) in
 let m_o_o = Term.Fun (m, [one_; one_]) in
 let m_o_x = Term.Fun (m, [one_; x]) in
 let m_i_x_o = Term.Fun (m, [i_x; one_]) in
 let m_i_i_x_o = Term.Fun (m, [i_i_x; one_]) in
 let m_i_m_x_o_o = Term.Fun (m, [Term.Fun (i, [m_x_o]); one_]) in
 let t = TermDTree.make () in
 let t' = TermDTree.insert t x in
 let t' = TermDTree.insert t' m_x_o in
 let t' = TermDTree.insert t' m_i_x_o in
 let t' = TermDTree.insert t' y in
 let t' = TermDTree.insert t' m_x_y in
 let t' = TermDTree.insert t' m_o_o in
 let t' = TermDTree.insert t' m_i_i_x_o in
 let t' = TermDTree.insert t' m_i_m_x_o_o in
 Format.printf "DTree: %s\n" (TermDTree.to_string t');
 let variants = TermDTree.variant_candidates t' m_y_o in
 Format.printf "variants for %s: %s\n"
  (Term.to_string m_y_o)
  (Setx.to_string Term.to_string variants);
 let gens = TermDTree.generalization_candidates t' m_i_x_o in
 Format.printf "generalizations for %s: %s\n"
  (Term.to_string m_i_x_o)
  (Setx.to_string Term.to_string gens);
 let unifs = TermDTree.unification_candidates t' m_i_x_o in
 Format.printf "unifiable terms for %s: %s\n"
  (Term.to_string m_i_x_o)
  (Setx.to_string Term.to_string unifs);
 let unifs = TermDTree.unification_candidates t' m_x_y in
 Format.printf "unifiable terms for %s: %s\n"
  (Term.to_string m_x_y)
  (Setx.to_string Term.to_string unifs);
 let ol1 = TermDTree.overlap1_candidates_below_root t' m_o_x in
  let s =
  Setx.fold
  (fun (trm, p) s -> (Term.to_string trm^"@"^(Pos.to_string p)^", "^s))
  ol1
  ""
 in
 Format.printf "Overlap1 candidates: %s\n" s;
 let ol2 = TermDTree.overlap2_candidates t' one_ in
  let s =
  Setx.fold
  (fun (trm, p) s -> (Term.to_string trm^"@"^(Pos.to_string p)^", "^s))
  ol2
  ""
 in
 Format.printf "Overlap2 candidates: %s\n" s;
 let ts = TermDTree.insert t' c2_b_c5_x in
 let ol2 = TermDTree.overlap2_candidates ts b_c_c_b_y in
  let s =
  Setx.fold
  (fun (trm, p) s -> (Term.to_string trm^"@"^(Pos.to_string p)^", "^s))
  ol2
  ""
 in
 Format.printf "Overlap2 candidatesi for %s: %s\n" (Term.to_string b_c_c_b_y) s;
 let t'' = TermDTree.delete t' m_x_o in
 let t'' = TermDTree.delete t'' x in
 try
  let _ = TermDTree.delete t' i_x in (* not there *)
  assert false
 with
  TermDTree.Not_in_index -> assert true;
 Format.printf "DTree after delete: %s\n" (TermDTree.to_string t'');
;;*)

 (*test () *)
