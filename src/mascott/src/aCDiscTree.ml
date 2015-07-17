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
 @since  2011/01/13 *)

(* AC discrimination trees as in 
 Bachmair et al. 93: Associative-commutative discrimination nets
*)

(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module BMA = BipartiteMatchingAutomata;;
module PS = ACPString;;
module SymbolMap = Map.Make(Types.Label);;
module T = U.Term;;
module M = U.Monad;;
module Sig = U.Signature;;
module Label = Types.Label;;
module TLabel = U.Label;;
(*** OPENS ***************************************************************)
open Util;;
open M;;

(*** INCLUDES ************************************************************)
include Types.ACDiscTree;;

(*** EXCEPTIONS **********************************************************)
exception Is_leaf

exception Is_inner_node

(*** FUNCTIONS ***********************************************************)

let values =  function
  | Leaf vs
  | ACLeaf (vs,_,_,_) -> vs
  | _ -> raise Is_inner_node
;;

let add_value (t,v) vs =
 let rec add ts = function
  | [] -> (t,[v])::ts
  | (t',vs) ::ts' when (T.compare t t') = 0 ->
   List.rev_append ((t',v::vs)::ts') ts
  | (t',vs) ::ts' -> add ((t',vs) ::ts) ts'
 in add [] vs
;;

let edges =  function
  | Node (edges) 
  | ACNode (edges,_,_,_) -> edges
  | _ -> raise Is_leaf
;;

let succ l u = try Some (SymbolMap.find l (edges u)) with Not_found -> None

let union = List.union

let munion m1 m2 = m1 >>= fun s1 -> m2 >>= fun s2 -> return (s1 @ s2)

let update edges k v = SymbolMap.add k v edges

let pair = Pair.make

let all_fst = List.map fst

(* --------------------------------------------------------------------- *)
(*                                INSERTION                              *)
(* --------------------------------------------------------------------- *)

let get_successor edges l ps =
  try edges, SymbolMap.find l edges 
  with Not_found -> 
  let succ = 
   match PS.top_and_next ps with
    | None -> failwith "Oops, empty pstring"
    | Some(Label.AC _,n) -> 
     if PS.is_empty n then ACLeaf([],[],empty,PS.pos ps) 
     else ACNode(SymbolMap.empty,[],empty,PS.pos ps)
    | Some(_,n) -> 
     if PS.is_empty n then Leaf [] 
     else Node(SymbolMap.empty)
  in SymbolMap.add l succ edges, succ
;;

let ac_pairs ps v = List.map (flip pair v) (PS.ac_subterms ps)

let rec insert (t,v) u =
 PS.of_term t >>= fun ps ->
 let l,ps' = PS.top_label ps, PS.next ps in
 let edges',succ = get_successor (edges u) l ps in
 insert_pstring succ ((t,v),ps,ps') >>= fun succ' ->
 return (Node (update edges' l succ'))

and insert_pstring node ((t,v) as tv,ops,ps) =
 match node with
  | Leaf values -> return (Leaf (add_value tv values))
  | Node(edges) ->
   let l,ps' = PS.top_label ps, PS.next ps in
   let edges',succ = get_successor edges l ps in
   insert_pstring succ (tv,ps,ps') >>= fun succ' -> 
   return (Node (update edges' l succ'))
  | ACLeaf (values,terms,subtree,p) ->
   foldl (flip insert) subtree (ac_pairs ops v) >>= fun subtree' ->
   return (ACLeaf (add_value tv values,PS.ac_term ops::terms,subtree',p))
  | ACNode (edges,terms,subtree,p) ->
   let l,ps' = PS.top_label ps, PS.next ps in
   foldl (flip insert) subtree (ac_pairs ops v) >>= fun subtree' ->
   let edges',succ = get_successor edges l ps in
   insert_pstring succ (tv,ps,ps') >>= fun succ' ->
   return (ACNode (update edges' l succ',PS.ac_term ops::terms,subtree',p))
;;


(* --------------------------------------------------------------------- *)
(*                                RETRIEVAL                              *)
(* --------------------------------------------------------------------- *)
let get_automaton k =
 if k=2 then Some BMA.s2  
 else if k=3 then Some BMA.s3
 else if k=4 then Some BMA.s4
 else None
;;

let has_matching matrix k =
 match get_automaton k with
  | None -> true (* k too large *)
  | Some sk -> BMA.check sk matrix
;;

let rec print_line = function
 | [] -> ""
 | x :: xs -> (string_of_int x)^" "^(print_line xs)
;;

let rec print_matrix = function
 | [] ->"\n"
 | l :: ls -> (print_line l)^" \n "^(print_matrix ls)
;;

let make_matrix ts smatches =
 let mem t si = if List.mem t si then 1 else 0 in
 let line si = List.map (fun t -> mem t si) ts in
 List.map line smatches
;;

let match_graph ts smatches =
 let m = List.length ts in
 let n = List.length smatches in
 let xs,ts = List.partition T.is_var ts in
 let k = List.length ts in
 if not ((n = m) || ((n>m) && (m>k))) then false
 else if k=0 then true
 else
  (*Format.printf "The matrix \n %s %!" (print_matrix matrix);*)
  let b = has_matching (make_matrix ts smatches) k in
  (*Format.printf "has a matching: %i\n%!" (if b then 1 else 0);*)
  b
;;

let all_to_stringm ts =
  map T.to_stringm ts >>= fun s -> return (List.foldl (fun x y -> x^" "^y) "" s);;

(* Obtain pstring of term t, list cs of candidate terms (cf. set M_u' in 
 [Ba93]) and current node u. *)
let rec match_term t u f =
 match PS.top_fun_label t with
  | None -> match_for_var t u f
  | Some l -> munion (match_for_label t u f l) (match_for_var t u f)

and match_for_var t u f =
 match succ Label.Star u with
  | None -> return [] 
  | Some Leaf vs -> return (List.filter (f <.> fst) vs)
  | Some ((Node _) as v) -> match_term (PS.after t) v f
  | _ -> failwith "Variable edge leads to AC node"

and match_for_label t u f l =
 match succ l u with
  | None -> return []
  | Some (Leaf vs) -> return (List.filter (f <.> fst) vs)
  | Some ((Node _) as v) -> match_term (PS.next t) v f
  | Some (ACLeaf(vs,lv,root,p)) ->
   (*PS.to_stringm t >>= fun s ->
   Format.printf "Match %s in leaf\n" s;*)
   match_all (PS.ac_subterms t) root lv >>= fun lv' ->
   (*all_to_stringm lv' >>= fun ts ->
   Format.printf "At position %s terms %s match\n%!" (Pos.to_string p) ts;*)
   let covered t = (T.is_var t) || (List.mem t lv') in
   let f' t = (f t) && (covered (T.subterm p t)) in
   return (List.filter (f' <.> fst) vs)
  | Some ((ACNode(_,lv,root,p)) as v) ->
   match_all (PS.ac_subterms t) root lv >>= fun lv' ->
   (*all_to_stringm lv' >>= fun ts ->
   Format.printf "At position %s terms %s match\n%!" (Pos.to_string p) ts;*)
   let covered t = (T.is_var t) || (List.mem t lv') in
   let f' t = (f t) && (covered (T.subterm p t)) in
   match_term (PS.next t) v f'

and match_all ss v lv =
 let retrieve_terms si = retrieve' si v >>= (return <.> all_fst) in
 map retrieve_terms ss >>= fun smatches ->
 filter (fun t -> return (match_graph (T.args t) smatches)) lv

and retrieve' t dt = PS.of_term t >>= fun p -> match_term p dt (fun _ -> true)
;;

let retrieve t dt = retrieve' t dt >>= (return <.> List.flat_map snd)

(* --------------------------------------------------------------------- *)
(*                                VARIANTS                               *)
(* --------------------------------------------------------------------- *)

let single_one_per_column m = 
 let sum = List.zip_with (+) in
 List.for_all (fun x -> x=1) (List.foldl1 sum m)
;;

let match_variant_graph ts smatches =
 let m = List.length ts in
 let n = List.length smatches in
 let xs,ts = List.partition T.is_var ts in
 let ys,ss = List.partition (T.is_var <.> fst) smatches in
 let k = List.length ts in
 let j = List.length ss in
 if not ((n = m) && (k=j)) then false
 else if k=0 then true
 else has_matching (make_matrix ts (List.map snd ss)) k
;;

(* repetition of above code which allows to apply a special graph matching
 function at the root *)
let rec variants_for_all ss v lv =
 let rterms si = variant_retrieve' si v >>= 
  (return <.> (pair si) <.> all_fst) in
 map rterms ss >>= fun smatches ->
 filter (fun t -> return (match_variant_graph (T.args t) smatches)) lv

and variants_for_label t u f l =
 match succ l u with
  | None -> return []
  | Some (Leaf vs) -> return (List.filter (f <.> fst) vs)
  | Some ((Node _) as v) -> variants_for_term (PS.next t) v f
  | Some (ACLeaf(vs,lv,root,p)) ->
   variants_for_all (PS.ac_subterms t) root lv >>= fun lv' ->
   let covered t = (T.is_var t) || (List.mem t lv') in
   let f' t = (f t) && (covered (T.subterm p t)) in
   return (List.filter (f' <.> fst) vs)
  | Some ((ACNode(_,lv,root,p)) as v) ->
   variants_for_all (PS.ac_subterms t) root lv >>= fun lv' ->
   let covered t = (T.is_var t) || (List.mem t lv') in
   let f' t = (f t) && (covered (T.subterm p t)) in
   variants_for_term (PS.next t) v f'

and variants_for_var t u f =
 match succ Label.Star u with
  | None -> return []
  | Some Leaf vs -> return (List.filter (f <.> fst) vs)
  | Some ((Node _) as v) -> variants_for_term (PS.after t) v f
  | _ -> failwith "Variable edge leads to AC node"

and variants_for_term t u f =
 match PS.top_fun_label t with
  | None -> variants_for_var t u f
  | Some l -> variants_for_label t u f l

and variant_retrieve' t dt = 
 PS.of_term t >>= fun p -> variants_for_term p dt (fun _ -> true)

let variants t dt = 
 variant_retrieve' t dt >>= (return <.> List.flat_map snd)
;;

(* --------------------------------------------------------------------- *)
(*                              ENCOMPASSMENTS                           *)
(* --------------------------------------------------------------------- *)

(* unaviodable overapproximation - nonlinearity! *)
let encompassments_at_root t dt =
 retrieve t dt (*>>= fun matches ->
 variants t dt >>= fun variants ->
 return (List.diff matches variants)*)
;; 

let encompassments_below_root (t,pos) dt =
(* T.to_stringm t >>= fun st ->
 let poss = ACPosition.all_to_string pos in*)
(* Format.printf "%s, pos %s\n%!" st poss;*)
 if T.is_var t then failwith "variable in encompassments below root";
 let pos = List.remove (ACPosition.root t) pos in
 let pos = List.remove (Pos.root,[]) pos in
 (*Format.printf "enc below root\n%!";*)
 let tpos = List.map (fun p -> Termx.subterm t p,p) pos in
(* Format.printf "still enc below root\n%!";*)
 let pair p = return <.> (flip pair) p in
 flat_map (fun (t, p) -> retrieve t dt >>= map (pair p)) tpos 
;;

(* overapproximation, includes variants - must be filtered afterwards! *)
let encompassments (t,pos) dt =
 let pair_root = return <.> (List.map (flip pair (ACPosition.root t))) in
 encompassments_at_root t dt >>= pair_root >>= fun ecs ->
 encompassments_below_root (t,pos) dt >>= fun ecs' ->
 return (union ecs ecs')
;;

(* --------------------------------------------------------------------- *)
(*                                  TESTS                                *)
(* --------------------------------------------------------------------- *)

(*
let test () =
 let sigma = Sig.empty 20 in
 let x,sigma = Sig.create_var "x" sigma in
 let y,sigma = Sig.create_var "y" sigma in
 let z,sigma = Sig.create_var "z" sigma in
 let x,y,z = T.Var x, T.Var y, T.Var z in
 let f,sigma = Sig.create_fun 2 "f" sigma in
 let f,sigma = Sig.set_theory f TLabel.AC sigma in
 let a,sigma = Sig.create_fun 0 "a" sigma in
 let b,sigma = Sig.create_fun 0 "b" sigma in
 let g,sigma = Sig.create_fun 1 "g" sigma in
 let h,sigma = Sig.create_fun 2 "h" sigma in
 let a_,b_ = T.Fun(a, []), T.Fun(b, []) in
 let ga = T.Fun(g,[a_]) in
 let gx = T.Fun(g,[x]) in
 let faa = T.Fun(f, [a_;a_]) in
 let faaa = T.Fun(f, [a_;faa]) in
 let faba = T.Fun(f, [a_;T.Fun(f, [b_;a_])]) in
 let faab = T.Fun(f, [a_;T.Fun(f, [a_;b_])]) in
 let faby = T.Fun(f, [a_;T.Fun(f, [b_;y])]) in
 let fxy = T.Fun(f, [x;y]) in
 let fxx = T.Fun(f, [x;x]) in
 let fax = T.Fun(f, [a_;x]) in
 let hfaxgb = T.Fun(h,[fax;T.Fun(g,[b_])]) in
 let hfxygz = T.Fun(h,[fxy;T.Fun(g,[z])]) in
 let hhxfaab = T.Fun(h,[T.Fun(h,[x;faa]); b_]) in
 let hfaxfxy = T.Fun(h,[fax;fxy]) in
 let hfaxfaba = T.Fun(h,[fax;faba]) in
 let hfaxfaaa = T.Fun(h,[fax;faaa]) in
 let hfabyfaaa = T.Fun(h,[faby;faaa]) in
 let hfabya = T.Fun(h,[faby;a_]) in
 let hfabyz = T.Fun(h,[faby;z]) in
 let hxa = T.Fun(h,[x;a_]) in
 let hax = T.Fun(h,[a_;x]) in
 let hxy = T.Fun(h,[x;y]) in
 let fhfabyax = T.Fun(f,[hfabya; x]) in
 let fhfxyzz = T.Fun(f,[T.Fun(h,[fxy; z]); z]) in
 let fhfabyzx = T.Fun(f,[hfabyz; x]) in 
 let fhxyz = T.Fun(f,[hxy;z]) in
 let fhxyzz = T.Fun(f,[hxy;z; z]) in
 let fhfabyab = T.Fun(f,[hfabya; b_]) in
 let all_to_stringm ts = 
  map T.to_stringm ts >>= fun s -> return (List.foldl (fun x y -> x^" "^y) "" s)
 in
 let t = empty in
 let tree = foldr (fun t u -> Termx.flatten t >>= fun s -> insert (s,s) u) t in
 let run_retrieve t ts =
  Either.right (M.run sigma (
   Termx.flatten t >>= fun t ->
   tree ts >>= retrieve t >>= fun tt -> all_to_stringm tt >>= fun s ->
   T.to_stringm t >>= fun ts ->
   Format.printf "Retrieved for %s: %s\n" ts s; return tt
 )) in
 let run_variants t ts =
  Either.right (M.run sigma (
   Termx.flatten t >>= fun t ->
   tree ts >>= variants t >>= fun tt -> all_to_stringm tt >>= fun s ->
   T.to_stringm t >>= fun ts ->
   Format.printf "Variants for %s: %s\n" ts s; return tt
 )) in
 let run_flatten ts = Either.right (M.run sigma (map Termx.flatten ts)) in
 let assert_retrieve x ys ys' =  
  assert (List.equal (run_retrieve x ys) (run_flatten ys')) 
 in
 let assert_variants x ys ys' =
  assert (List.equal (run_variants x ys) (run_flatten ys'))
 in
 assert_retrieve a_ [a_; x; b_] [a_; x];
 assert_retrieve y [a_; x] [x];
 assert_retrieve b_ [a_; ga] [];
 assert_retrieve ga [ga; gx; x; a_] [ga; gx; x];
 assert_retrieve hxa [hxa; x; hxy; hax] [hxa; x; hxy];
 assert_retrieve faa [a_] [];
 assert_retrieve faa [faa] [faa];
 assert_retrieve fax [a_; faa; x; fxy; fax; faba; b_] [x;fxy;fax];
 assert_retrieve faab [a_; faa; x; fxy; fax; faba; b_] [x;fxy;fax;faba];
 assert_retrieve faaa [a_; faa; x; fxy; fax; faba; b_; faby; faaa]
  [x; fxy; fax; faaa];
 assert_retrieve faby [faby; faa; fxy; fax; faba] [faby;fxy;fax];
 assert_retrieve hfaxgb [hfaxgb; x; hfxygz; hhxfaab] [hfaxgb;x;hfxygz];
 assert_retrieve hfabyfaaa [hfabyfaaa;hxy;hfabyz;hfaxfaba;hfaxfaaa;hfaxfxy]
  [hfabyfaaa; hxy; hfabyz; hfaxfaaa;hfaxfxy];
 assert_retrieve fhfabyax 
  [fhfabyax;fhfxyzz;fhfabyzx;fhfabyab;fhxyz;fhxyzz;fxx]
  [fhfabyax; fhfxyzz; fhfabyzx; fhxyz;fxx];
 assert_variants a_ [a_; x; b_] [a_];
 assert_variants y [a_; x] [x];
 assert_variants b_ [a_; ga] [];
 assert_variants ga [ga; gx; x; a_] [ga];
 assert_variants hxa [hxa; x; hxy; hax] [hxa];
 assert_variants faa [a_] [];
 assert_variants faa [faa] [faa];
 assert_variants faab [a_; faa; x; fxy; fax; faba; b_;faab] [faab;faba];
 assert_variants fax [a_; faa; x; fxy; fax; faba; b_] [fax];
 assert_variants fxx [a_; faa; x; fxy; fax; faba; fxx] [fxx;fxy];
 assert_variants hfabyfaaa [hfabyfaaa;hxy;hfabyz;hfaxfaba;hfaxfaaa;hfaxfxy]
  [hfabyfaaa];
;;*)

(*test ();;*)
