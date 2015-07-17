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
open Rewritingx;;

(*** MODULES ******************************************************************)
module F = Format;;
module LL = LazyList;;
module M = Monad;;
module Con = Context;;
module Fun = Function;;
module Sub = Substitution;;
module Var = Variable;;

(*** TYPES ********************************************************************)
type t = Term.t list * Con.t * Sub.t;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Constructors *)
let make = Triple.make;;

(* Miscellaneous *)
let length l = List.length (Triple.fst l);;

let rec find_option f = function
 | [] -> None
 | x::xs -> match f x with None -> find_option f xs | x -> x
;;

let context_substitution s t =
 Option.the (find_option (fun p -> try
  let mgu = Elogic.match_term (Term.subterm p t) s in
  Some(Con.of_term p t,p,mgu)
 with Elogic.Not_matchable -> None) (Term.funs_pos t))
;;

let context_substitutions s t =
 List.foldl (fun cs p -> try
  let mgu = Elogic.match_term (Term.subterm p t) s in
  lazy(LL.Cons((Con.of_term p t,p,mgu),cs))
 with Elogic.Not_matchable -> cs) LL.empty (Term.funs_pos t)
;;

let of_term_rules_positions s rs =
 let rec of_term_rules_positions t acc = function
  | [] ->
   LL.map (fun (c,p,mu) -> (List.rev acc,c,mu)) (context_substitutions s t)
  | (rule,p)::rs ->
   let (l,r) = Rule.to_terms rule in
   let mu    = Elogic.match_term (Term.subterm p t) l in
   let u     = Term.replace p (Sub.apply_term mu r) t in
   of_term_rules_positions u (t::acc) rs
 in
 of_term_rules_positions s [] rs
;;

let rewrites s t trs = 
 List.mem t (Rewrite.reducts ~n:1 s trs)
;;

let is_loop trs (ts,c,mu) = 
 let ts = ts@[Context.apply (Sub.apply_term mu (List.hd ts)) c] in
 let ys = List.tl ts@[Context.apply (Sub.apply_term mu (List.hd ts)) c] in
 List.for_all2 (fun s t -> rewrites s t trs) ts ys
;;

let of_term_list trs ts =
 if List.length ts < 2 then failwith "this is no loop" else (
 let s = List.hd ts and t = List.last ts in
 let (c,p,mu) = context_substitution s t in
 (*uncomment if positions also wanted *)
 (*
 let rec loop acc = function
  | [t] -> List.rev((t,p) :: acc)
  | s::(t::_ as ts) ->
   let p = List.find (fun p -> Trs.exists (fun rule -> try
    let mu = Elogic.match_term (Term.subterm p s) (Rule.lhs rule) in
    t = Term.replace p (Sub.apply_term mu (Rule.rhs rule)) s
   with Elogic.Not_matchable -> false) trs) (Term.pos s)
   in loop ((s,p) :: acc) ts
  in
  (loop [] (List.init ts),c,mu)
  *)
 (List.init ts,c,mu)
);;

let dp_origin s weak =
 let (s,t) = Rule.to_terms s in
 let r = List.find (fun rule -> let l,r = Rule.to_terms rule in
  l = s && List.mem t (Term.subterms r)
  ) weak
 in
 r
;;

let get_context s t = 
 let p = List.find (fun p -> Term.subterm p s = t) (Term.pos s) in
 Con.of_term p s
;;

let root_rewrites s t rule = Rule.rewrites s Position.root rule t

let get_rp s trs t = 
 let prs = List.product (Trs.to_list trs) (Term.pos s) in
 List.find (fun (r,p) -> Rule.rewrites s p r t) prs
;;

let loop_detailed (ts,c,mu) p =
 let trs = uncurry Trs.union (Problem.get_sw p) in
 let rec tf acc = function
  | [_] -> List.rev acc
  | s::t::ts -> let (r,p) = get_rp s trs t in tf ((s,p,r)::acc) (t::ts)
  | _ -> failwith "illegal value"
 in
 let le = (ts@[Con.apply (Sub.apply_term mu (List.hd ts)) c]) in
 (tf [] le,c,mu)
;;

(* transforms loop in (strict, weak) into loop in weak) *)
let transform p (ts,c,mu) =
 let strict, weak = Problem.get_sw p in
 let strict, weak = Trs.to_list strict, Trs.to_list weak in
 let rec tr c = function
  | [] -> failwith "empty list"
  | [t] -> [(t,c)]
  | s::t::ts -> 
  try
   let sr = List.find (root_rewrites s t) strict in
   let wr = dp_origin sr weak in
   let d = get_context (Rule.rhs wr) (Rule.rhs sr) in
   let c' = Con.compose d c in
   (s,c)::tr c' (t::ts)
  with 
   | Not_found -> (s,c)::tr c (t::ts)
 in
 let tsc = tr Con.Hole (ts@[Con.apply (Sub.apply_term mu (List.hd ts) ) c]) in
 let ts = List.map (fun (t,c) -> Con.apply t c) tsc in
 let _,c = List.last tsc in
 (List.init ts,c,mu)
;;

(* Printers *)
let fprintf fmt l =
 let ts = Triple.fst l and c = Triple.snd l and s = Triple.thd l in
 F.fprintf fmt "@[@[loop length:@ %d@]" (length l);
 F.fprintf fmt "@\n@[<1>terms:@\n";
 M.fprintf Term.fprintfm "@\n" fmt ts >>= fun _ ->
 F.fprintf fmt "@]@\n@[<1>context:@ "; Con.fprintfm fmt c >>= fun _ ->
 F.fprintf fmt "@]@\n@[<1>substitution:@\n"; Sub.fprintfm fmt s >>= fun _ ->
 M.return (F.fprintf fmt "@]@]")
;;

let fprintfx_redex fmt (t,p,r) =
 F.fprintf fmt "<redex>";
 Term.fprintfx fmt t >>= fun _ ->
 Position.fprintfx fmt p;
 Rule.fprintfx fmt r >>= fun _ ->
 M.return (F.fprintf fmt "</redex>");
;;

let fprintfx fmt l p =
 let l = loop_detailed l p in
 let ts = Triple.fst l and c = Triple.snd l and s = Triple.thd l in
 F.fprintf fmt "<loop>"; 
 M.iter (fprintfx_redex fmt) ts >>= fun _ ->
 Sub.fprintfx fmt s >>= fun _ ->
 Con.fprintfx fmt c >>= fun _ -> 
 M.return (F.fprintf fmt "</loop>")
;;
