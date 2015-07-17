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
open Util;;
open Processors;;
open Processors.Rewritingx;;

(*** MODULES ******************************************************************)
module E = Either;;
module M = Monad;;
module P = Problem;;
module S = Signature;;
module V = Rewriting.Variable;;

(*** TYPES ********************************************************************)
type term = Var of string | Fun of string * term list;;
type condition = Strong of term * term | Low of term * term;;

type rule =
 | Strict of term * term * condition list
 | Weak of term * term * condition list
;;

type theory =
 | Theory of string * string list
 | Equations of (term * term) list
;;

type strategy =
 | Full
 | Innermost
 | Outermost
 | Contextsensitive of (string * int list) list
;;

type t = {rules : rule list; theories : theory list; strategy : strategy};;
type data = {strict : Trs.t; weak : Trs.t};;

(*** GLOBALS ******************************************************************)
let symbols = 100;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Constructors and Destructors *)
let add_rules rs r = {r with rules = rs @ r.rules};;
let add_theories ts r = {r with theories = ts @ r.theories};;
let empty = {rules = []; theories = []; strategy = Full};;
let set_rules rs r = {r with rules = rs};;
let set_theories ts r = {r with theories = ts};;
let set_strategy s r = {r with strategy = s};;

let rec to_term = function
 | Var n -> M.create_var n >>= (M.return <.> Term.make_var)
 | Fun (n,ts) ->
  M.create_fun (List.length ts) n >>= fun f ->
  M.map to_term ts >>= (M.return <.> Term.make_fun f)
;;

let to_rule l r =
 to_term l >>= fun l -> to_term r >>= (M.return <.> Rule.of_terms l)
;;

let add_strict r d = {d with strict = Trs.add r d.strict};;
let add_weak r d = {d with weak = Trs.add r d.weak};;
let get_strict d = d.strict;;
let get_weak d = d.weak;;

let to_trs r =
 let update f l r m = to_rule l r >>= fun r -> m >>= (M.return <.> f r) in
 let rec to_trs m = function
  | [] -> m
  | Strict (l,r,[]) :: s -> to_trs (update add_strict l r m) s
  | Weak (l,r,[]) :: s -> to_trs (update add_weak l r m) s
  | _ -> failwith "conditions are not supported"
 in
 let r = List.rev r and s = S.empty symbols in
 let m = M.return {strict = Trs.empty; weak = Trs.empty} in
 let m = to_trs m r >>= fun r -> M.get >>= (M.return <.> pair r) in
 (E.right <?> "error occured") (M.run s m)
;;

let to_problem r = match r.theories with
 | [] ->
  let strategy = match r.strategy with
   | Full -> P.Full
   | Innermost -> P.Innermost
   | Outermost -> P.Outermost
   | _ -> failwith "contextsensitive strategies are not supported"
  in
  let to_problem d =
   let s = get_strict d and w = get_weak d in
   if Trs.is_empty w then P.make_sp P.All strategy s
   else P.make_rp P.All strategy s w
  in
  Pair.apply to_problem id (to_trs r.rules)
 | _ -> failwith "theories are not supported"
;;

let to_trs_with r sign =
 let update f l r m = to_rule l r >>= fun r -> m >>= (M.return <.> f r) in
 let rec to_trs m = function
  | [] -> m
  | Strict (l,r,[]) :: s -> to_trs (update add_strict l r m) s
  | Weak (l,r,[]) :: s -> to_trs (update add_weak l r m) s
  | _ -> failwith "conditions are not supported"
 in
 let r = List.rev r and s = sign in
 let m = M.return {strict = Trs.empty; weak = Trs.empty} in
 let m = to_trs m r >>= fun r -> M.get >>= (M.return <.> pair r) in
 (E.right <?> "error occured") (M.run s m)
;;

let set_theory ths s = 
 let id_of = function
  (*"A" -> Label.A |*) "C" -> Label.C | "AC" -> Label.AC
  | i -> failwith ("Unsupported theory identifier '"^i^"'")
 in
 let addf tid (s,map) n =
  try 
   let f = S.find_fun n s in
   let f',s = S.set_theory f tid s in s,(f,f')::map
  with 
   Not_found -> s,map
 in
 let add_theory_part (s,map) = function
 | Theory(i, ids)  -> List.foldl (addf (id_of i)) (s,map) ids
 | Equations _ -> failwith "Unsupported theory description"
 in List.foldl add_theory_part (s,[]) ths
;;

let label m trs =
 let rec replace = function
  | Term.Var _ as x -> x
  | Term.Fun(g, ts) when not (List.mem_assoc g m) -> 
   Term.Fun(g, List.map replace ts)
  | Term.Fun(g, ts) -> let g' = List.assoc g m in
   Term.Fun(g', List.map replace ts)
 in
 Trs.project (Rule.project replace) trs
;;

let theory m =
 let theory_for (_,f) =
  let x,y,z = V.zero, V.next(V.zero), V.next(V.next(V.zero)) in
  let x,y,z = Term.Var x, Term.Var y, Term.Var z in
  let assoc f = Rule.of_terms (Term.Fun(f, [Term.Fun(f,[x;y]);z]))
   (Term.Fun(f,[x;Term.Fun(f,[y;z])])) in
  let comm f = Rule.of_terms (Term.Fun(f, [x;y])) (Term.Fun(f,[y;x])) in
  [assoc f; comm f]
 in Trs.of_list (List.flat_map theory_for m)
;;

let to_problem_with r sign = 
 let (trs, sign) = to_trs_with r.rules sign in
 let sign, m = set_theory r.theories sign in
  let strategy = match r.strategy with
   | Full -> P.Full
   | Innermost -> P.Innermost
   | Outermost -> P.Outermost
   | _ -> failwith "contextsensitive strategies are not supported"
  in
  let to_problem d =
   let s = get_strict d and w = get_weak d in
   if not (Trs.is_empty w) then P.make_rp P.All strategy s w
   else if List.is_empty r.theories then P.make_sp P.All strategy s
   else
    P.make_ep P.All strategy (theory m) (label m s)
  in
  Pair.apply to_problem id (trs,sign)
;;

(* Access Functions *)
let get_rules r = r.rules;;
let get_theories r = r.theories;;
let get_strategy r = r.strategy;;

(* Printers *)
let fprintf_term fmt =
 let rec fprintf fmt = function
  | Var x -> Format.fprintf fmt "@[%s@]" x
  | Fun (f,ts) ->
   Format.fprintf fmt "@[%s@[(@,%a@]@,)@]" f (List.fprintf fprintf ",@ ") ts
 in
 Format.fprintf fmt "%a" fprintf
;;

let fprintf_rule fmt = function
 | Strict (l,r,_) ->
  Format.fprintf fmt "@[%a@ ->@ %a@]" fprintf_term l fprintf_term r
 | Weak (l,r,_) ->
  Format.fprintf fmt "@[%a@ ->=@ %a@]" fprintf_term l fprintf_term r
;;

let fprintf_theory fmt = function
 | Theory (t,_) -> Format.fprintf fmt "@[Theory %s _@]" t
 | Equations _ -> Format.fprintf fmt "@[Equations _@]"
;;

let fprintf_strategy fmt = function
 | Full -> Format.fprintf fmt "@[Full@]"
 | Innermost -> Format.fprintf fmt "@[Innermost@]"
 | Outermost -> Format.fprintf fmt "@[Outermost@]"
 | Contextsensitive _ -> Format.fprintf fmt "@[Contextsensitive _@]"
;;

let fprintf fmt r =
 let fprintf_rules = List.fprintf fprintf_rule "@\n" in
 let fprintf_theories = List.fprintf fprintf_theory "@\n" in
 Format.fprintf fmt "@[SRS:@\n{%a}@\nTheories:@\n{%a}@\nStrategy:@ %a@]"
  fprintf_rules r.rules fprintf_theories r.theories fprintf_strategy r.strategy
;;

let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;
