(* Copyright 2008 Martin Korp, Christian Sternagel, Harald Zankl
 * GNU Lesser General Public License
 *
 * This file is part of TTT2.
 * 
 * TTT2 is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * TTT2 is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with TTT2. If not, see <http://www.gnu.org/licenses/>.
 *)

(*** OPENS ********************************************************************)
open Monad;;
open Util;;
open Operators;;

(*** MODULES ******************************************************************)
module F = Formula;;
module Ass = Assignment;;

(*** TYPES ********************************************************************)
type poly = (int*int) list;; (* (coeff, var) *) (*!!differs in minisat+ *)
type kind = poly option;;    (* None -> any solution, Some x -> minimize x*)

type constr = 
 | Lt  of poly * int
 | Leq of poly * int
 | Eq  of poly * int
 | Ge  of poly * int
 | Gt  of poly * int
 | Min of poly
;;

(*** FUNCTIONS ****************************************************************)
let eq x y = Eq (x,y);;
let gt x y = Gt (x,y);;
let ge x y = Ge (x,y);;
let mn x = Min x;;

let top = Ge ([(0,0)], 0);;
let bot = Ge ([(0,0)], 1);;

(* generate sum of PB variables for diophantine variable*)
let gen a = get >>= fun c ->
 if not (Hashtbl.mem c.State.gen_tbl a) then (
  let spec = F.a_spec a in
  let bits = Int.bits_int64 (spec.Formula.min) in
  let s = !(c.State.count) in
  let ls = List.range s (s+bits) in
  let ls = List.mapi (fun i x -> (Int.pow 2 i,x)) ls in
  c.State.count := !(c.State.count) + bits;
  Hashtbl.add c.State.gen_tbl a ls;
 );
 return (Hashtbl.find c.State.gen_tbl a)
;;

(* negate sum of PB variables *)
let invert ls = List.map (fun (c,v) -> (-c,v)) ls;;
(* scale sum of PB variables *)
let scale i ls = List.map (fun (c,v) -> (c*i,v)) ls;;

(* interaction with Minisat+*)
let send tuples rhs ineq =
 let c, v = List.split tuples in
 let v = List.map string_of_int v in
 MinisatPlus.add_constraint v c rhs ineq 
;;

let send_constraint = function
 | Lt (monos, sum) -> send monos sum (-2)
 | Leq (monos, sum) -> send monos sum (-1)
 | Eq (monos, sum) -> send monos sum 0
 | Ge (monos, sum) -> send monos sum 1
 | Gt (monos, sum) -> send monos sum 2
 | Min (monos) -> send monos 0 3
;;

let termlist = function
 | Min monos
 | Lt (monos, _) 
 | Leq (monos, _)
 | Eq (monos, _) 
 | Ge (monos, _)
 | Gt (monos, _) -> monos
;;

(* get variables in [pbcs]*)
let variables pbcs = 
 let ht = Hashtbl.create 200 in
 let vars_pbc pbc = 
  let check (v, _) = if not(Hashtbl.mem ht v) then Hashtbl.add ht v () in 
  List.iter check (termlist pbc)
 in
 List.iter vars_pbc pbcs;
 Hashtbl.fold (fun k _ acc -> k :: acc) ht []
;;

let fail s f x = 
 Format.fprintf Format.str_formatter s f x;
 failwith (Format.flush_str_formatter ());
;;

let to_int b = int_of_string (Number.to_string b);;

(* transform arithmetic formula to PB *)
let rec tf = function
 | F.Top          -> return [top]
 | F.Bot          -> return [bot]
 | F.And (x,y)    -> lift2 (@) (tf x) (tf y)
 | F.Eq (a,F.C b) -> sequence [lift2 eq (t a) (return (to_int b))]
 | F.Ge (a,F.C b) -> sequence [lift2 ge (t a) (return (to_int b))]
 | F.Gt (a,F.C b) -> sequence [lift2 gt (t a) (return (to_int b))]
(*
 | F.P _ -> failwith "prop variable not allowed"
 | F.Not _ -> failwith "negation not allowed"
 | F.Or (x, y) -> failwith "disjunction not allowed"
 | F.Implies (x, y) -> impl ctx (tf ctx x) (tf ctx y)
 | F.Iff (x, y) -> failwith "iff not allowed"
 *)
 | p -> fail "%a operator not suppored" Formula.fprintf_p p;
and t = function
 | F.A a -> gen a
 | F.C r as c when c = Formula.zero -> return []
 | F.Add (a, b) -> lift2 (@) (t a) (t b)
 | F.Sub (a, b) -> lift2 (@) (t a) (lift invert (t b))
 | F.Mul (F.C i, b) -> lift2 scale (return (to_int i)) (t b)
 (*
 | F.Fresh a -> failwith "how to represent fresh?" 
 | F.Mul (a, b) -> mul ctx (t ctx a) (t ctx b) 
 | F.Ite (x, C 1, C 0) -> 
 | F.Ite (x, a, b) -> ite ctx (tf ctx x) (t ctx a) (t ctx b)
 *)
 | a -> fail "%a operator not suppored" Formula.fprintf_a a
;;

(* evaluation functions *)
let val_of_pb ass x = match Ass.val_of (F.P x) ass with
  | false -> 0
  | true  -> 1
;;

let val_of_a ass a = get >>= fun c -> gen (F.unwrap_a a) >>= fun ls ->
 return (Number.of_int (List.foldr (fun (c,x) -> (+) (c*(val_of_pb ass x))) 0 ls))
;;

let rec val_of ass = function
 | F.Top          -> return true
 | F.Bot          -> return false
 | F.And (x,y)    -> lift2 (&&) (val_of ass x) (val_of ass y)
 | F.Eq (a,F.C b) -> lift2 Number.eq (val_of2 ass a) (return b)
 | F.Ge (a,F.C b) -> lift2 Number.ge (val_of2 ass a) (return b)
 | F.Gt (a,F.C b) -> lift2 Number.gt (val_of2 ass a) (return b)
(*
 | F.P _ -> failwith "prop variable not allowed"
 | F.Not _ -> failwith "negation not allowed"
 | F.Or (x, y) -> failwith "disjunction not allowed"
 | F.Implies (x, y) -> impl ctx (tf ctx x) (tf ctx y)
 | F.Iff (x, y) -> failwith "iff not allowed"
 *)
 | p -> fail "operator %a not suppored" Formula.fprintf_p p
and val_of2 ass = function
 | F.A a            -> return (Ass.find_a (F.A a) ass)
 | F.C r as c when (c = Formula.zero) -> return Number.zero
 | F.Add (a, b)     -> lift2 Number.add (val_of2 ass a) (val_of2 ass b)
 | F.Sub (a, b)     -> lift2 Number.sub (val_of2 ass a) (val_of2 ass b)
 | F.Mul (F.C a, b) -> lift2 Number.mul (return a) (val_of2 ass b) 
 (*
 | F.Fresh a -> failwith "how to represent fresh?"
 | F.SMul (i, b) -> mul ctx (of_int ctx i) (t ctx b)
 | F.Ite (x, C 1, C 0) -> 
 | F.Ite (x, a, b) -> ite ctx (tf ctx x) (t ctx a) (t ctx b)
 *)
 | a -> fail "operator %a not suppored" Formula.fprintf_a a
;;

let add_a k vm am = vm >>= fun v -> am >>= fun a -> return (Ass.add_a k v a);;

(*TODO: bug on 64 bit machine (seldomly wrong strings are returned) *)
(*let int_of_string i = try int_of_string i with Failure _ -> 0;; *)

let build_ass ls = get >>= fun c ->
 let ls = List.map (fun i -> F.P (int_of_string i)) ls in
 (*first construct ass for propositional variables ...*)
 let ass = List.foldr (fun k -> Ass.add_p k true) Ass.empty ls in
 let vs = Hashtbl.fold (fun k v acc -> (k,v)::acc) c.State.gen_tbl [] in
 (*... then compute values for arithmetical variables *)
 List.foldr (fun (k,v) -> add_a (F.A k) (val_of_a ass (F.A k))) (return Ass.empty) vs
;;

let solve ?(goal=None) phi = 
 tf phi >>= fun pbcs ->
 Option.fold (fun gf -> lift2 cons (lift mn (t gf)) (return pbcs)) (return pbcs) goal 
  >>= fun pbcs ->
 let mode = Option.fold (const 0) 1 goal in
 let n_var = List.length (variables pbcs) in
 let n_pbc = List.length pbcs in
 MinisatPlus.init n_var n_pbc mode; 
 List.iter send_constraint pbcs;
 try
  lift Option.some (build_ass (MinisatPlus.solve ()))
 with 
  | Failure _ -> 
  return None (* C-stubs send error if unsatisfiable *)
;;

let rec eval_p p ass = match p with
 | F.Top           -> true
 | F.Bot           -> false
 | F.And (x, y)    -> eval_p x ass && eval_p y ass
 | F.Eq (a, F.C b) -> Number.eq (xt a ass) b
 | F.Ge (a, F.C b) -> Number.ge (xt a ass) b
 | F.Gt (a, F.C b) -> Number.gt (xt a ass) b
(*
 | F.P _ -> failwith "prop variable not allowed"
 | F.Not _ -> failwith "negation not allowed"
 | F.Or (x, y) -> failwith "disjunction not allowed"
 | F.Implies (x, y) -> impl ctx (tf ctx x) (tf ctx y)
 | F.Iff (x, y) -> failwith "iff not allowed"
 *)
 | p -> fail "%a operator not suppored" Formula.fprintf_p p
and xt a ass = match a with
 | F.A a            -> Ass.find_a (F.A a) ass
 | F.C r            -> r
 | F.Add (a, b)     -> Number.add (xt a ass) (xt b ass)
 | F.Sub (a, b)     -> Number.sub (xt a ass) (xt b ass)
 | F.Mul (F.C a, b) -> Number.mul a (xt b ass) 
 (*
 | F.Fresh a -> failwith "how to represent fresh?"
 | F.SMul (i, b) -> mul ctx (of_int ctx i) (t ctx b)
 | F.Ite (x, C 1, C 0) -> 
 | F.Ite (x, a, b) -> ite ctx (tf ctx x) (t ctx a) (t ctx b)
 *)
 | a -> fail "%a operator not suppored" Formula.fprintf_a a;
;;

let eval_a a ass = try Ass.find_a a ass with | Not_found -> Number.zero;;
