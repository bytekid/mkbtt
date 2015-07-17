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

(** Define strategy to select nodes
 @author Sarah Winkler
 @since  2009/12/16 *)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module Term = U.Term;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module Elogic = U.Elogic;;
module Sub = U.Substitution;;
module Str = SelectionStrategy;;
module C = Completion;;
module W = World;;
module Monad = W.Monad;;
module N = IndexedNode;;
module NS = NodeState;;
module NSA = NodeSetAux;;
module Trsx = W.M.Trsx;;
module CP = CompletionProcessx;;

(*** OPENS ***************************************************************)
open Monad;;

(*** GLOBALS *************************************************************)

let processcosts : 
 (CP.t * SelectionStrategy.processproperty, int) Hashtbl.t 
 = Hashtbl.create 20;;

(*** FUNCTIONS ***********************************************************)

(* auxiliary *)
let list_sum f s =
 List.foldr (fun x sum -> (f x) + sum) 0 s
;;

let trs_sum f trs =
 let sum x m =
  m >>= fun s -> 
  f x >>= fun fx ->
  return (s + fx)
 in
 Trs.fold sum (return 0) trs
;;


let list_sum f = 
 let add s x = f x >>= fun fx -> return (s + fx) in
 foldl add 0
;;

let list_min' f = function
  [] -> raise (C.GiveUp "Selection: Minimum over empty list")
 | z :: zs ->
  let min (x,fx) y = 
   f y >>= fun fy -> return (if fx < fy then (x,fx) else (y,fy))
  in
  f z >>= fun fz -> foldl min (z,fz) zs
;;

let list_min f = function
 | []  -> return max_int
 | l -> list_min' f l >>= fun (_, fx) -> return fx
;;

let list_min_elt f l = list_min' f l >>= fun (x, _) -> return x

let (<+>) xm ym = xm >>= fun x -> ym >>= fun y -> return (x + y)

let negative ym = ym >>= fun y -> return ((-1) * y)

let hfind t k = try Some(Hashtbl.find t k) with Not_found -> None

(* function returning cost value according to property of a 
   node as integer *)
let rec termpaircost (s, t) nodes = function
 | Str.MaxSize -> return (max (Term.size s) (Term.size t))
 | Str.SumSize -> return (Term.size s + (Term.size t))
and eqscost eset nodes = function
 | Str.ESum c -> 
  let sumf = fun e -> termpaircost (Equation.terms e) nodes c in
  list_sum sumf eset
 | Str.Cardinal -> return (List.length eset)
and trscost trs nodes = function
 | Str.TSum c -> 
  let pair r = Rule.lhs r, Rule.rhs r in
  let sumf = fun r -> termpaircost (pair r) nodes c in
  trs_sum sumf trs
 | Str.CPs c -> 
  Trsx.critical_pairs trs >>= fun cps ->
  eqscost cps nodes c
 | Str.Size -> return (Trs.size trs)
and processcost p nodes c = 
 match hfind processcosts (p,c) with 
  | Some v -> return v
  | None -> (
  let vm = 
   match c with
    | Str.PPlus (c1, c2) -> 
     (processcost p nodes c1) <+> (processcost p nodes c2)
    | Str.EProjection c -> 
     NSA.project_e p nodes >>= fun eqs -> 
     eqscost eqs nodes c
    | Str.RProjection c ->
     NSA.project_r p nodes >>= fun trs ->
     trscost trs nodes c
    | Str.CProjection c ->
     NSA.project_c p nodes >>= fun trs ->
     trscost trs nodes c
  in vm >>= fun v -> Hashtbl.replace processcosts (p,c) v; return v )
and processsetcost ps nodes = function
 | Str.PMin c -> list_min (fun p -> processcost p nodes c) ps
 | Str.PSum c -> list_sum (fun p -> processcost p nodes c) ps
 | Str.Count -> return (List.length ps)
and nodecost i nodes c = 
 match c with
  | Str.Age -> return i
  | Str.DataProperty c -> 
   N.data i >>= fun ts -> termpaircost ts nodes c
  | Str.Plus(c1, c2) -> 
   (nodecost i nodes c1) <+> (nodecost i nodes c2)
  | Str.Negative c -> negative (nodecost i nodes c)
  | Str.ELabel c -> N.eo i >>= fun ps -> processsetcost ps nodes c
;;

(* given a strategy, returns a list of properties according to which 
nodes will be compared, i.e., eliminates all probabilities by taking 
a random value *)
let rec propertylist_from_strategy = function
 | Str.Random -> []
 | Str.Property(p, s) -> p :: (propertylist_from_strategy s)
 | Str.Probability(p, s, s') ->
  let r = Random.float 1.0 in
  if r <= p then 
   propertylist_from_strategy s
  else
   propertylist_from_strategy s'
;;

let rec nodecostlist n ns ps = map (nodecost n ns) ps

(*
let output i c = 
 let rec str = function
  | [] -> ""
  | v :: cs -> (string_of_int v) ^ ", " ^ (str cs)
 in
 N.by_id i >> fun n ->
 Format.printf " Node %s has cost (%s)\n" (N.to_string n) (str c)
;;*)

let apply s =
 NS.open_nodes >>= fun nodes2choose ->
 NS.all_nodes >>= fun allnodes ->
 (* clear hashtable *)
 Hashtbl.clear processcosts;
 (* fix properties used in this selection, eliminate probabilities *)
 let l = propertylist_from_strategy s in
 let costmap n = nodecostlist n allnodes l >>= fun c -> return (c,n) in
 (* map nodes to their cost, take minimum *)
(* Format.printf "Nodes 2 choose: %i\n%!" (List.length nodes2choose);*)
 list_min_elt costmap nodes2choose
;;

let of_string s =
 let lexbuf = Lexing.from_string s in
 SelectionParser.start SelectionLexer.token lexbuf
;;

