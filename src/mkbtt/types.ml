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

(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module Term = U.Term;;

module CompletionProcess = struct
 type t = int list

 let initial = []
end

module Node = struct
 module TCP = CompletionProcess;;

 type t = {
   data    : Equation.t;
   r0      : TCP.t list * TCP.t list;
   r1      : TCP.t list * TCP.t list;
   e       : TCP.t list * TCP.t list;
   c0      : TCP.t list;
   c1      : TCP.t list;
   parents : occurrence list
  }
 (* if n contains s=t then (n,true) is regarded as s -> t whereas
   (n,false) is associated with t->s *)
 and t_rule = int * bool
 and occurrence =
  (* a node containing this occurrence was supplied as an axiom *)
  | Axiom
  (* node deduced from respective overlap for given process set *)
  | Deduce of t_rule * Pos.t * t_rule * TCP.t list
  (* Rewrite(s->t, l->r, l1r l3r) was obtained from rewrite inference
   with node <s=t, l1, l2, l3, ...> using <l=r, r, \ldots> at position
   p in t. l1r and l3r are the produced labels. Can be rewrite1 or rewrite2
   inference *)
  | Rewrite of t_rule * Pos.t * t_rule * TCP.t list * TCP.t list
  (* Rewrite2(equation, p, rule, l1r, l2r, l3r) was obtained from rewrite1 
   inference with node <equation, l1, l2, l3, ...> using <rule, r, \ldots> 
   where l1r = l1 \cap r, l2r = l2 \cap r and l3r = l3 \cap r *)
 (*  | Rewrite2 of t_rule * Pos.t * t_rule * processes * processes * processes*)
  | Instance of t_rule * TCP.t list
 ;;

let hash n = Equation.hash n.data

let fprintf fmt n =
 let s, t = Equation.terms n.data in
 Format.fprintf fmt "<@[%a@ :@ %a@]>"
  Term.fprintf s Term.fprintf t
;;

let make s t r0 r1 e c0 c1 ps =
 let eq = Equation.of_terms s t in
 {data    = eq;
  r0      = r0;
  r1      = r1;
  e       = e;
  c0      = c0;
  c1      = c1;
  parents = ps}
;;

let copy n =
 let s, t = Equation.terms n.data in
 make s t n.r0 n.r1 n.e n.c0 n.c1 n.parents
;;

let compare n n' = Equation.compare n.data n'.data

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

module NodeTermIndex = IndexWrapper.Make (NodeEntry);;
(*module NodeTermIndex = IndexComparison.Make (NodeEntry);;*)

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

module Goal = struct
 type poss_pair = Pos.t list * Pos.t list

 type t =
  | NodeGoal of int list
  | WaldmeisterGoal of int list
  | ExistentialGoal of (int * poss_pair) list
  | NoGoal
 ;;
end
