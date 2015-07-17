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

(** Auxiliary functions mainly related to terms 
 @author Sarah Winkler
 @since  2008/01/16 *)

(*** OPENS ********************************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module Var = Rewriting.Variable;;
module VSub = Util.Replacement.Make (Var) (Var);;
module M = U.Monad;;
module Term = U.Term;;
module Sig = U.Signature;;
module Sub = U.Substitution;;
module Rule = U.Rule;;
module Elogic = U.Elogic;;

(*** TYPES ***************************************************************)
(*** EXCEPTIONS **********************************************************)
(*** GLOBALS *************************************************************)
(*** FUNCTIONS ***********************************************************)
let (>>=) = M.(>>=)

let (>>) = M.(>>)

let return = M.return

let empty_signature = Sig.empty 50


let fresh_var sigma =
 let x, sigma = Sig.fresh_var sigma in
 let xn, sigma = Sig.create_var_name x sigma in
 Term.Var x, Sig.add_var x xn sigma
;;
(*
let fresh_fun name a sigma = Sig.create_fun a name sigma 

(* Rename variables in a term to get it to some kind of normal form.
   This is to be able to easily compare nodes and avoid variant checks *)
let rec normalize_term' sigma sub = function
 | Term.Var x ->
  begin try
   (Sub.find x sub, sub, sigma)
  with
   | Not_found ->
    begin
    let v, sigma' = fresh_var sigma in 
    (v, Sub.add x v sub, sigma')
    end
  end
 | Term.Fun (f, ts) ->
  let fold_norm (l, sub, sigma) t =
    begin
    let (t', sub', sigma') = normalize_term' sigma sub t in
    (t'::l, sub', sigma')
    end
  in
   let (ts', sub', sigma') = List.fold_left fold_norm ([], sub, sigma) ts in
   (Term.Fun (f, List.rev ts'), sub', sigma')
;;

(* return normalized term together with applied substitution *)
let normalize t =
 let sigma = empty_signature in
 let (t', rho, _) = normalize_term' sigma Sub.empty t in
 (t', rho)
;;*)

let rec normalize_term' v sub = function
 | Term.Var x ->
  begin try
   (Sub.find x sub, sub, v)
  with
   | Not_found ->
    (Term.Var v, Sub.add x (Term.Var v) sub, Var.next v)
  end
 | Term.Fun (f, ts) ->
  let fold_norm (l, sub, v) t =
   let (t', sub', v') = normalize_term' v sub t in (t'::l, sub', v')
  in
   let (ts', sub', v') = List.fold_left fold_norm ([], sub, v) ts in
   (Term.Fun (f, List.rev ts'), sub', v')
;;

let normalize t =
 let (t', rho, _) = normalize_term' Var.zero Sub.empty t in
 (t', rho)
;;

let normalize_rule rule =
 let l, r = Rule.lhs rule, Rule.rhs rule in
 let (l', sub, v) = normalize_term' Var.zero Sub.empty l in
 let (r', _, _) = normalize_term' v sub r in
 Rule.of_terms l' r'
;;

let normalize_both s t =
 (* if first term contains more variables, rename second after the first
    term. If both sides contain the same variables, fix one order for
    normalization (by Term.compare) to hopefully keep result
    deterministic ... it doesnt. (neither side's variables subset!?)
    so normalize both terms, compare them then*)
 let s', _ = normalize s in
 let t', _ = normalize t in
 let l, r = if (Term.compare s' t') > 0 then s, t else t, s in
 let rl = normalize_rule (Rule.of_terms l r) in
 Rule.lhs rl, Rule.rhs rl
;;

let compare_size ((r1,_, _), _, (r2, _, _)) ((s1,_, _), _, (s2, _, _)) =
 let rule_size r =
  (Term.size (Rule.lhs r)) + (Term.size (Rule.rhs r))
 in
 let rsize = (rule_size r1) + (rule_size r2) in
 let ssize = (rule_size s1) + (rule_size s2) in
 (compare rsize ssize)
;;

(* functions to skolemize a term *)
let rec skolemization' s = function
 | Term.Var x ->
  if Sub.mem x s then M.return s
  else M.fresh_fun >>=
   M.create_fun_name >>= (M.create_fun 0) >>= (fun c ->
   M.return (Sub.add x (Term.Fun(c,[])) s))
 | Term.Fun (f,ts) -> M.foldl skolemization' s ts
;;

let skolemization = skolemization' Sub.empty

(* replace every variable by a fresh constant *)
let skolemize s t =
 skolemization s >>= (fun sigma -> 
 skolemization' sigma t >>= (fun sigma ->
 return (Pair.map (Sub.apply_term sigma) (s, t))))
;;

let lex f g (a, b) (a', b') =
 let c = f a a' in
 if c <> 0 then c
 else g b b'
;;

let rec nonvar_subterms' p = function
 | Term.Var _ -> []
 | Term.Fun (f, ts) as t ->
  List.fold_lefti
   (fun i res y ->
    let p' = Pos.add_last i p in
    List.rev_append (nonvar_subterms' p' y) res
   )
   [t, p]
   ts
;;

(* only proper subterms! *)
let nonvar_pos_proper_subterms = function
 | Term.Var _ -> []
 | Term.Fun (f, ts) ->
  List.fold_lefti
   (fun i res y ->
    let p' = Pos.make i in
    List.rev_append (nonvar_subterms' p' y) res
   )
   []
   ts
;;

let rec rename_to sigma t t' =
 try (
  match t, t' with
   | Term.Var x, Term.Var y -> VSub.add x y sigma
   | Term.Fun (f, ts), Term.Fun (f', ts') when f = f' -> 
    List.fold_left2 rename_to sigma ts ts'
   | _ -> raise Elogic.Not_matchable
 ) with VSub.Inconsistent -> raise Elogic.Not_matchable
;;

let fresh_fun name a sigma = Sig.create_fun a name sigma

let rec fresh_vars = function
   | Term.Var _ ->
    M.get >>= fun s -> let (x,s) = Sig.fresh_var s in M.set s >> 
    M.return (Term.Var x)
   | Term.Fun (f,ts) ->
    let g ti ts = M.lift (flip List.cons ts) (fresh_vars ti) in
    M.foldr g [] ts >>= fun ts -> M.return (Term.Fun (f,ts))
;;

let test_signature =
 let sigma = empty_signature in
 let c, sigma = fresh_fun "c" 0 sigma in
 let f, sigma = fresh_fun "f" 1 sigma in
 let g, sigma = fresh_fun "g" 2 sigma in
 let h, sigma = fresh_fun "h" 1 sigma in
 let k, sigma = fresh_fun "k" 2 sigma in
 let varx, sigma = fresh_var sigma in
 let vary, sigma = fresh_var sigma in
 let varz, sigma = fresh_var sigma in
 let vars = (varx, vary, varz) in
 let funs = (c, f, g, h, k) in
 vars, funs, sigma
;;
