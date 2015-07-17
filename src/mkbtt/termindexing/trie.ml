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

(** Term indexing with Path Indexing
@author Sarah Winkler
@since  2007/04/30 *)

(** Provides data structure and functions of a trie for term indexing *)

(*** OPENS ********************************************************************)
open Util;;

(*** MODULES *************************************************************)
module Fun = Rewriting.Function;;
module Pos = Rewriting.Position;;
module Term = U.Term;;
module Var = Rewriting.Variable;;
module M = U.Monad;;

open M;;

module Make (*: TermIndex.T with type entry = Entry.t*)
    = functor (Entry: TermIndex.ENTRY_TYPE) ->
   struct
(*** SUBMODULES **********************************************************)
module EL = TermIndex.EntryList(Entry);;

(*** TYPES ***************************************************************)
type entry = Entry.t

type flabel =
 | F of Fun.t
 | Var
;;

type t = (flabel, pnode) Hashtbl.t
and pnode =
  | PNode of t array (* position mapped to argument *)
  | Leaf of Entry.t list (* Term set attached to leaf *)
;;

(*** EXCEPTIONS **********************************************************)
(* Transformation_error: indicates failure in pattern matching *)
exception Unexpected_leaf

exception Unexpected_inner_node

exception Empty_node

exception There_is_none

exception Not_in_trie of string


(*** FUNCTIONS ***********************************************************)

let new_fnode () = Hashtbl.create 200

let new_trie = new_fnode

let make () = new_fnode ()

let is_empty t = return (Hashtbl.length t = 0)

let new_leaf term = Leaf [term]

let add_to_leaf tset term = EL.union [term] tset

let remove_from_leaf tset value = EL.diff tset [value]

let find_ari f = get >>= fun s -> M.find_ari f

let is_constant f = 
 find_ari f >>= fun a -> return (a == 0)
;;

(* Creates a new pnode, depending on the flabel flab *)
let new_pnode flab = 
 match flab with
  | F f -> 
   find_ari f >>= fun n ->
   if n = 0 then
    return (Leaf [])
   else
    (let a = (Array.make n (new_fnode ())) in
     for i = 0 to n - 1 do Array.set a i (new_fnode ()) done;
     return (PNode a)
    )
  | Var -> return (Leaf [])
;;

(* Given a fnode and a function symbol/variable, the attached pnode
   is selected if it exists, otherwise a new pnode created.
   This pnode is returned together with the flabel and the remaining
   pstring *)
let rec get_pnode fnode pstring =
 match pstring with
  | PathString.Fct (f, p, pstring') ->
   let flab = F f in (
   try return (Hashtbl.find fnode flab, flab, p, Some pstring')  
   with Not_found -> 
    new_pnode flab >>= fun n -> return (n, flab, p, Some pstring'))
 | PathString.Var -> (
  try return (Hashtbl.find fnode Var, Var, 0, None)
  with Not_found -> 
   new_pnode Var >>= fun n -> return (n, Var, 0, None))
 | PathString.Const c ->
  let clab = F c in (
  try return (Hashtbl.find fnode clab, clab, 0, None)
  with Not_found -> 
   new_pnode clab >>= fun n -> return (n, clab, 0, None))
;;

let strip = function Some r -> r | None -> (failwith "No stripping")

(* Given a (sub)trie and a pstring together with its term, the pstring is
   inserted in the trie, term added to the corresponding leaf *)
let rec add_to_fnode fnode value pstring =
 get_pnode fnode pstring >>= fun (pnode, flab, p, opt_pstring) ->
 add_to_pnode pnode p opt_pstring value >>= fun pnode' ->
 Hashtbl.replace fnode flab pnode';
 return fnode
and add_to_pnode pnode p pstring value =
 match pnode with
  | PNode a -> 
   let i = p - 1 in (* Position starts at 1, index at 0 *)
   let current = Array.get a i in
   add_to_fnode current value (strip pstring) >>= fun n ->
   Array.set a i n; 
   return (PNode a)
  | Leaf termset -> return (Leaf (add_to_leaf termset value))
;;

(* generate multiple p-strings from t, each one corresponding to a 
   traversal of one root-to-leave-path in a term *)
let insert trie (term, value) = 
 let pstrings = PathString.get term in
 let fadd = fun trie p -> add_to_fnode trie value p in
 M.foldl fadd trie pstrings >>
 return trie
;;

let hashtbl_empty h = (Hashtbl.length h) == 0

let rec array_is_empty' a i =
 if i == Array.length a then
  true
 else
  if hashtbl_empty (Array.get a i) then
   array_is_empty' a (i+1)
  else
   false
;;

let array_is_empty a = array_is_empty' a 0


let replace_leaf trie flab v =
 match Hashtbl.find trie flab with
  | Leaf tset ->
   let tset' = remove_from_leaf tset v in
   if tset' = [] then 
    raise Empty_node
   else 
    Hashtbl.replace trie flab (Leaf tset'); trie
  | _ -> raise Unexpected_inner_node
;;

let fnode_remove trie lab = 
 Hashtbl.remove trie lab;
 if Hashtbl.length trie == 0 then raise Empty_node;
 trie
;;

let rec delete_in_fnode trie s value =
 (match s with
  | PathString.Fct (f, p, s') -> 
   (try 
      let pnode = Hashtbl.find trie (F f) in 
      let pnode' = delete_in_pnode pnode p s' value in
      Hashtbl.replace trie (F f) pnode'; trie 
     with
      | Empty_node -> fnode_remove trie (F f)
      | Not_found -> raise (Not_in_trie (Fun.to_string f))
   )
  | PathString.Var -> 
    ( try replace_leaf trie Var value with
      | Empty_node -> fnode_remove trie Var
    )
  | PathString.Const c ->
    try replace_leaf trie (F c) value with
     | Empty_node -> fnode_remove trie (F c)
  )


and delete_in_pnode pnode p s value =
 match pnode with
 | PNode a ->
  (try
    let i = p - 1 in (* Position starts at 1, index at 0 *)
    let fnode = (Array.get a i) in
    (Array.set a i (delete_in_fnode fnode s value); (PNode a))
   with
    | Empty_node -> 
     if array_is_empty a then raise Empty_node else PNode a
  )
 | Leaf _ -> raise Unexpected_leaf
;;

(* exception may arrive if last term was deleted *)
let delete_pstring trie s value = 
 try 
  delete_in_fnode trie s value 
 with | Empty_node -> trie
;;

let delete trie (term, value) =
 let pstrings = PathString.get term in
  let res = 
   List.fold_left (fun t ps -> delete_pstring t ps value) trie pstrings
  in
 return res
;;


let rec retrieve_variants' trie term =
 match term with
  | Term.Var _ ->
   (try 
    (match Hashtbl.find trie Var with
     | Leaf ts -> ts
     | PNode _ -> raise Unexpected_inner_node
    )
   with
    | Not_found -> []
   )
  | Term.Fun (rt, ts) -> 
   (try
    (match Hashtbl.find trie (F rt)  with
     | Leaf ts -> ts (* constants *)
     | PNode a -> (* function symbol *)
      let f = 
       fun (tset, i) term -> 
        let ai = Array.get a i in
        (EL.inter tset (retrieve_variants' ai term), i+1)
      in
      let a0 = Array.get a 0 in
      let (res, _) =
       List.fold_left f (retrieve_variants' a0 (List.hd ts), 1) (List.tl ts)
      in
       res
    )
   with
    | Not_found -> []
   )
;;

let variant_candidates trie term =
 let v = retrieve_variants' trie term in
 return v
;;

let rec descendant_union trie = 
 let get_all pnode =
  match pnode with
   | Leaf tset -> 
    tset
   | PNode a -> 
    let add_fnode = 
     fun s fnode -> EL.union s (descendant_union fnode) 
    in
    Array.fold_left add_fnode [] a
 in
 let add_pnode = fun _ pnode tset -> EL.union tset (get_all pnode) in
 Hashtbl.fold add_pnode trie []
;;

let rec retrieve_instances trie term =
 match term with
  | Term.Var _ ->
   descendant_union trie
  | Term.Fun (rt, ts) ->
   (try
    (match Hashtbl.find trie (F rt)  with
     | Leaf ts ->  ts (* constants *)
     | PNode a -> (* function symbol *)
      let f =
       fun (tset, i) term ->
        let ai = Array.get a i in
        (EL.inter tset (retrieve_instances ai term), i+1)
      in
      let a0 = Array.get a 0 in
      let (res, _) =
       List.fold_left f (retrieve_instances a0 (List.hd ts), 1) (List.tl ts)
      in
       res
    )
   with
    | Not_found -> []
   )
;;

(* for an array a of length k, array_intersection f a computes
   \bigcap_{i=0}^k f(i, a[i]) *)
let array_intersection f a =
 let res = 
  Array.fold_left 
   (fun (s, i) ai -> (EL.inter (f i ai) s, i+1)) 
   (f 0 (Array.get a 0), 0) (* a must have at least one element *)
   a
 in fst res
;;

(* for an array a of length k, array_union f a computes
   \bigcup_{i=0}^k f(i, a[i]) *)
let array_union f a =
 let res =
  Array.fold_left
   (fun (s, i) ai -> (EL.pair_union (f i ai) s, i+1))
   ([], 0)
   a
 in fst res
;;


let get_tset_at_var trie =
  try
   (match Hashtbl.find trie Var with
     | Leaf tset -> tset
     | _ -> raise Unexpected_inner_node
   )
  with
   | Not_found -> []
;;

(* retrieval of generalizations *)
let rec generalization_candidates' trie term =
 let m =
  match Term.root term with 
   | None -> []
   | Some f ->
   let lab = F f in
   if not (Hashtbl.mem trie lab) then []
   else (
   match Hashtbl.find trie lab with
    | Leaf tset -> tset
    | PNode a -> 
      array_intersection 
      (fun i f -> 
        generalization_candidates' f (Term.subterm (Pos.make i) term)
      ) 
      a
   )
 in 
  EL.union (get_tset_at_var trie) m
;;

let generalization_candidates trie term =
 return (generalization_candidates' trie term)
;;
(* retrieval of non-variable generalizations *)
let nonvar_generalization_candidates trie term =
  match Term.root term with
   | None -> []
   | Some f ->
   let lab = F f in
   if not (Hashtbl.mem trie lab) then []
   else
   (
   match Hashtbl.find trie lab with
    | Leaf tset -> tset
    | PNode a ->
      array_intersection
      (fun i f ->
        generalization_candidates' f (Term.subterm (Pos.make i) term)
      )
      a
   )
;;

(* retrieval of unifiable terms *)
let rec unification_candidates' trie term =
 let m =
  match Term.root term with
   | None ->
    (* descendant union collects possible instanciations of term*)
    descendant_union trie
   | Some f ->
   let lab = F f in (* label in hastable *)
    if not (Hashtbl.mem trie lab) then [] (* no transition with root *)
    else
    (
    match Hashtbl.find trie lab with
     | Leaf tset -> tset (* constant *)
     | PNode a ->
      array_intersection
       (fun i f -> 
        unification_candidates' f (Term.subterm (Pos.make i) term)
       )
       a
    )
   in (* form union with terms attached to variable * transitions *)
  EL.union (get_tset_at_var trie) m 
;;

let unification_candidates trie term =
 return (unification_candidates' trie term)
;;



(* for a term, pairs (p, s) for p a position and s a subterm 
   are returned, where s is not a variable *)
(* given a term, non-vargeneralization of proper subterms are returned, 
   paired with the subterm's position. 
   I.e., lhs of applicable rules (without
   further substitution to the query term) are given, where lhs is not
   a variant of he query term *)
let encompassment_candidates trie term =
 let pos_st = Termx.nonvar_pos_proper_subterms term in
 let res = 
  EL.pair_map_union (fun (t, p) ->
   List.map (fun n -> (n, p)) (generalization_candidates' trie t))
   ((term, Pos.root) :: pos_st)
  in
  let inst = generalization_candidates' trie term in 
   (* this contains variants - not avoidable *)
  let vars = retrieve_variants' trie term in
  let inst' = EL.diff inst vars in
  let ecs = EL.pair_union res (List.map (fun n -> (n, Pos.root)) inst') in
  return ecs
;;

let encompassment_candidates_below_root trie term =
 let pos_st = Termx.nonvar_pos_proper_subterms term in
 let res = EL.pair_map_union
  (fun (t, p) ->
   List.map (fun n -> (n, p)) (generalization_candidates' trie t))
   pos_st
 in
 return res
;;

(* including variants: useful for CPC, connected and blocked *)
let encompassment_candidates_not_strict trie term = 
 variant_candidates trie term >>= fun vars ->
 let vars' = List.map (fun n -> (n, Pos.root)) vars in
 encompassment_candidates trie term >>= fun encs ->
 return (vars' @ encs)
;;

(* given a term, all items matching instances of proper subterms are
   returned, paired with the subterm's position *)
let overlap1_candidates_below_root trie term =
 let pos = Term.funs_pos term in
 (* no epsilon here to avoid duplicate computation of overlays *)
 let pos = List.remove Pos.root pos in
 (*TODO: more efficient position/subterm retrieval *)
 let res = List.fold_left
  (fun res p -> let t = Term.subterm p term in
   let cs = List.map (fun n -> (n, p)) (unification_candidates' trie t) in
   cs @ res)
  []
  pos
 in
 return res
;;

(* given a term, all items matching instances of subterms are
   returned, paired with the subterm's position *)
let overlap1_candidates trie term =
 let pos = Term.funs_pos term in
 (*TODO: more efficient position/subterm retrieval *)
 let res = List.fold_left
  (fun res p -> let t = Term.subterm p term in
   let cs = List.map (fun n -> (n, p)) (unification_candidates' trie t) in
   cs @ res)
  []
  pos
  in
  return res
;;


(* retrieval of unifiable non-variable terms *)
let retrieve_unif_nonvar trie term =
  match Term.root term with
   (* descendant union collects possible instanciations of term*)
     None -> descendant_union trie
   | Some f ->
   let lab = F f in (* label in hastable *)
    if not (Hashtbl.mem trie lab) then [] (* no transition with root *)
    else
    (
    match Hashtbl.find trie lab with
     | Leaf tset -> tset (* constant *)
     | PNode a -> (* not a constant *)
      array_intersection
       (fun i f -> 
         unification_candidates' f (Term.subterm (Pos.make i) term)
       )
       a (* a has at least one element *)
    )
;;

(* return (u, pos) pairs such that u|pos and term are
   unifiable. TODO: avoid u|pos a variable *)
let rec retrieve_context_unif' trie term p =
 let add_pos n = (n, p) in
 let pos_e_unif = List.map add_pos (retrieve_unif_nonvar trie term) in 
 Hashtbl.fold 
  (fun lab pn s -> EL.pair_union s (pnode_ret pn term p)) 
  trie
  pos_e_unif 
and pnode_ret pnode term p =
 match pnode with
  | Leaf tset -> []
  | PNode a -> 
   array_union
   (fun i f -> retrieve_context_unif' f term (Pos.add_last i p))
   a
;;

(* return overlap candidates below the root *)
let overlap2_candidates trie term = 
 let res = retrieve_context_unif' trie term Pos.root in
 return res
;;

let size t = is_empty t >>= fun b -> return (if b then 0 else 1)

end (* Make *)

(*** TESTS ***************************************************************)

(*
module TermTrie = Make(TermIndex.TermEntry)

let test () =
 Format.printf "testing module Trie\n";
 let one = Fun.of_string "one" 0 in
 let i = Fun.of_string "i" 1 in
 let m = Fun.of_string "m" 2 in
 let x = Term.Var (Var.of_string "x") in
 let y = Term.Var (Var.of_string "y") in
 let i_x = Term.Fun (i, [x]) in
 let one_ = Term.Fun (one, []) in
 let m_x_o = Term.Fun (m, [x; one_]) in
 let m_i_x_o = Term.Fun (m, [i_x; one_]) in
 let trie = TermTrie.new_trie () in
 let trie' = TermTrie.insert trie x in
 let trie' = TermTrie.insert trie' m_x_o in
 let gen = TermTrie.generalization_candidates trie' m_i_x_o in
 let s = List.fold_right (fun trm s -> (Term.to_string trm^", "^s)) gen "" in
 Format.printf "Generalizations: %s\n" s;
 let var = TermTrie.variant_candidates trie' m_i_x_o in
 let s = List.fold_right (fun trm s -> (Term.to_string trm^", "^s)) var "" in
 Format.printf "Vsariants: %s\n" s;
 let encs = TermTrie.encompassment_candidates trie' m_i_x_o in
 let s = 
  List.fold_right 
  (fun (trm, p) s -> (Term.to_string trm^"@"^(Position.to_string p)^", "^s)) 
  encs 
  "" 
 in
 Format.printf "Encompassments: %s\n" s;
 let z = Term.Var (Variable.of_string "z") in
 let u = Term.Fun(m, [Term.Fun(m, [x; y]); z]) in
 let s = Term.Fun(m, [i_x; Term.Fun(m, [x; y])]) in
 let t = Term.Fun(m, [x; Term.Fun(i, [Term.Fun(m, [y; x])])]) in
 let trie' = TermTrie.insert trie' u in
 let ol1 = TermTrie.overlap1_candidates_below_root trie' s in
  let s =
  List.fold_right
  (fun (trm, p) s -> (Term.to_string trm^"@"^(Position.to_string p)^", "^s))
  ol1
  ""
 in
 Format.printf "Overlap1 candidates: %s\n" s;
 let s = Term.Fun(m, [i_x; Term.Fun(m, [x; y])]) in
 let trie' = TermTrie.insert trie' s in
 let ol2 = TermTrie.overlap2_candidates trie' t in
  let s =
  List.fold_right
  (fun (trm, p) s -> (Term.to_string trm^"@"^(Position.to_string p)^", "^s))
  ol2
  ""
 in
 Format.printf "Overlap2 candidates: %s\n" s;
(*** Some asssertions ****************************************************)
;;
*)

