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

(*** TYPES ********************************************************************)
type term = Var of string | Fun of string * term list;;
type rule = Strict of term * term | Weak of term * term;;
type strategy = Full | Leftmost | Rightmost;;
type t = {rules : rule list; strategy : strategy};;
type data = {strict : Trs.t; weak : Trs.t};;

(*** GLOBALS ******************************************************************)
let symbols = 100;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Constructors and Destructors *)
let add_rules rs r = {r with rules = rs @ r.rules};;
let empty = {rules = []; strategy = Full};;
let set_rules rs r = {r with rules = rs};;
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

let to_srs r =
 let update f l r m = to_rule l r >>= fun r -> m >>= (M.return <.> f r) in
 let rec to_srs m = function
  | [] -> m
  | Strict (l,r) :: s -> to_srs (update add_strict l r m) s
  | Weak (l,r) :: s -> to_srs (update add_weak l r m) s
 in
 let r = List.rev r and s = S.empty symbols in
 let m = M.return {strict = Trs.empty; weak = Trs.empty} in
 let m = to_srs m r >>= fun r -> M.get >>= (M.return <.> pair r) in
 (E.right <?> "error occured") (M.run s m)
;;

let to_problem r = match r.strategy with
 | Full ->
  Pair.apply (P.make_sp P.All P.Full <.> get_strict) id (to_srs r.rules)
 | _ -> failwith "strategies are not supported"
;;

(* Access Functions *)
let get_rules r = r.rules;;
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
 | Strict (l,r) ->
  Format.fprintf fmt "@[%a@ ->@ %a@]" fprintf_term l fprintf_term r
 | Weak (l,r) ->
  Format.fprintf fmt "@[%a@ ->=@ %a@]" fprintf_term l fprintf_term r
;;

let fprintf_strategy fmt = function
 | Full -> Format.fprintf fmt "@[Full@]"
 | Leftmost -> Format.fprintf fmt "@[Leftmost@]"
 | Rightmost -> Format.fprintf fmt "@[Rightmost@]"
;;

let fprintf fmt r =
 Format.fprintf fmt "@[SRS:@\n{%a}@]" (List.fprintf fprintf_rule "@\n") r.rules;
 Format.fprintf fmt "@[Strategy:@ %a@]" fprintf_strategy r.strategy
;;

let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;
