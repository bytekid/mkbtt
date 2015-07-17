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

(*** OPENS ***************************************************************)

(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module Term = U.Term;;
module Rule = U.Rule;;
module ACPos = ACPosition;;
module Sub = U.Substitution;;


module CompletionProcess = struct
 type t = int list

 let initial = []
end

module Overlap = struct
type t = {
 source: Term.t;
 outer_rule: (Rule.t * CompletionProcess.t list * (int * bool));
 position: ACPosition.t;
 inner_rule: (Rule.t * CompletionProcess.t list * (int * bool));
 sub: Sub.t;
 cp : Rule.t
 }

let source o = o.source;;
let inner_rule o = o.inner_rule;;
let outer_rule o = o.outer_rule;;
let position o = o.position;;
let sub o = o.sub;;
let mom o = Util.Triple.thd o.outer_rule;;
let dad o = Util.Triple.thd o.inner_rule;;
let cp o = o.cp;;

let processes o = 
 let ps, ps' = Util.Triple.snd o.outer_rule, Util.Triple.snd o.inner_rule in
 Util.List.intersect ps ps'
;;

let make t orule p irule s cp'= {
 source=t;
 outer_rule=orule;
 position=p;
 inner_rule=irule;
 sub=s;
 cp = cp'
}

let is_nontrivial o =
 let t = source o in
 not ((dad o = (mom o)) && (o.position = (ACPosition.root t)))


end

module Node = struct
 module TCP = CompletionProcess;;
 
 type ppair = TCP.t list * TCP.t list

 type poss = {
   all : ACPosition.t list;
   nonsymmetric : ACPosition.t list;
 }

 type t = {
   data    : Equation.t;
   r0      : ppair * ppair;
   r1      : ppair * ppair;
   e       : ppair;
   c0      : TCP.t list;
   c1      : TCP.t list;
   pos     : poss * poss;
   parents : occurrence list
  }
 (* if n contains s=t then (n,true) is regarded as s -> t whereas
   (n,false) is associated with t->s *)
 and t_rule = int * bool
 and occurrence =
  (* a node containing this occurrence was supplied as an axiom *)
  | Axiom
  (* a node containing this occurrence is part of the theory S *)
  | SAxiom
  (* node deduced from respective overlap for given process set *)
  | Deduce of Overlap.t
  (* Deduce(s->t,p,l->r,e) was obtained from a deduce inference
   with node <s=t, r', ...> using <l=r, r, ...> at position
   p in t, where e is the intersection of r and r' *)
  | Rewrite of Overlap.t
  (* Rewrite(s->t, l->r, l1r l3r) was obtained from rewrite inference
   with node <s=t, l1, l2, l3, ...> using <l=r, r, ...> at position
   p in t. l1r and l3r are the produced labels. Can be rewrite1 or rewrite2
   inference *)
  | Extend of t_rule * TCP.t list
  (* Derived in an extend inference from the respective rule *)
 ;;

let hash n = Equation.hash n.data

let fprintf fmt n =
 let s, t = Equation.terms n.data in
 Format.fprintf fmt "<@[%a@ :@ %a@]>"
  Term.fprintf s Term.fprintf t
;;

let make s t r0 r1 e c0 c1 pos ps =
 let eq = Equation.of_terms s t in
 {data    = eq;
  r0      = r0;
  r1      = r1;
  e       = e;
  c0      = c0;
  c1      = c1;
  pos     = pos;
  parents = ps}
;;

let copy n =
 let s, t = Equation.terms n.data in
 make s t n.r0 n.r1 n.e n.c0 n.c1 n.pos n.parents
;;

let compare n n' = Equation.compare n.data n'.data

let all_pos n = Util.Pair.map (fun ps -> ps.all) n

let nonsymm_pos n = Util.Pair.map (fun ps -> ps.nonsymmetric) n

let make_pos_part a ns = {all = a; nonsymmetric = ns}

let make_pos (a0,ns0) (a1,ns1) = make_pos_part a0 ns0,make_pos_part a1 ns1

let create_pos ps0 ps1 = make_pos (ps0, []) (ps1, [])

end

module NodeEntry = struct
 type t = int * bool

(*
 let get_term (i, b) =
  N.by_id i >>= fun n ->
  return (Rule.lhs (Node.brule b n))
 ;;
*)

 let to_string (i, b) =
(*  let s = Term.to_string in
  N.by_id i >>= fun n -> let l, r = Rule.to_terms (Node.brule b n) in
  return ("<"^(s l)^":"^(s r)^", "^(string_of_int i)^">")*)
  (string_of_int i) ^ ":" ^ (if b then "1" else "0")
 ;;

 let compare = Pervasives.compare
end

(*
module NodeTermIndex = IndexWrapper.Make (NodeEntry);;
*)

module WaldmeisterGoalNode = struct
 module TCP = CompletionProcess;;

 type origin = Initial | Reduct of int * int * TCP.t list;;

 type t = {
  term      : Term.t;
  processes : TCP.t list * TCP.t list;
  origins   : origin list * origin list;
 } 

 let hash n = Term.hash n.term

 let make t ps os = {term = t; processes = ps; origins = os}

 let copy n = make n.term n.processes n.origins
 
 let compare n n' = Term.compare n.term n'.term

 let fprintf fmt n = Term.fprintf fmt n.term
end

module Label = struct
 type t = Star | Fun of Rewriting.Function.t | AC of Rewriting.Function.t

 let compare = Pervasives.compare

 let to_string = function
  | Star -> "*"
  | AC f
  | Fun f -> Rewriting.Function.to_string f
 ;;

 let fprintf fmt = function
  | Star -> Format.fprintf fmt "@[*@]"
  | AC f
  | Fun f -> Format.fprintf fmt "@[%s@]" (Rewriting.Function.to_string f)
 ;;

end
 
module SymbolMap = Map.Make(Label);;

module ACDiscTree = struct
 type 'a values = (U.Term.t * 'a list) list

 type 'a t_node =
   Leaf of 'a values
 | Node of 'a t_edges
 | ACLeaf of ('a values * U.Term.t list * 'a t_node * Rewriting.Position.t)
 | ACNode of ('a t_edges * U.Term.t list * 'a t_node * Rewriting.Position.t)
 and 'a t_edges = ('a t_node) SymbolMap.t

 type 'a t = 'a t_node

 let empty = Node(SymbolMap.empty)
end

