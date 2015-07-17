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
module L = List;;
module Pos = Rewriting.Position;;
module Var = Rewriting.Variable;;
module Fun = Rewriting.Function;;
module VSub = Util.Replacement.Make (Var) (Var);;
module M = U.Monad;;
module T = U.Term;;
module C = U.Context;;
module Sig = U.Signature;;
module Sub = U.Substitution;;
module R = U.Rule;;
module Elogic = U.Elogic;;
module Lab = U.Label;;

(*** OPENS ***************************************************************)
open M;;

(*** FUNCTIONS ***********************************************************)
let var = function
 | T.Var x -> x
 | _ -> failwith "invalid argument to Termx.var"
;;

let empty_signature = Sig.empty 50

let fresh_var sigma =
 let x, sigma = Sig.fresh_var sigma in
 let xn, sigma = Sig.create_var_name x sigma in
 T.Var x, Sig.add_var x xn sigma
;;
(*
let fresh_fun name a sigma = Sig.create_fun a name sigma 

(* Rename variables in a term to get it to some kind of normal form.
   This is to be able to easily compare nodes and avoid variant checks *)
let rec normalize_term' sigma sub = function
 | T.Var x ->
  begin try
   (Sub.find x sub, sub, sigma)
  with
   | Not_found ->
    begin
    let v, sigma' = fresh_var sigma in 
    (v, Sub.add x v sub, sigma')
    end
  end
 | T.Fun (f, ts) ->
  let fold_norm (l, sub, sigma) t =
    begin
    let (t', sub', sigma') = normalize_term' sigma sub t in
    (t'::l, sub', sigma')
    end
  in
   let (ts', sub', sigma') = L.fold_left fold_norm ([], sub, sigma) ts in
   (T.Fun (f, L.rev ts'), sub', sigma')
;;

(* return normalized term together with applied substitution *)
let normalize t =
 let sigma = empty_signature in
 let (t', rho, _) = normalize_term' sigma Sub.empty t in
 (t', rho)
;;*)

let is_ac_symbol = M.is_theory Lab.AC

let find_term_with f =
 let rooted_f = function T.Fun (g,_) when f=g -> true | _ -> false in
 let rec find ys = function
  | [] -> None
  | x::xs when rooted_f x -> Some(x, ys@xs)
  | x::xs -> find (x::ys) xs
 in find []
;;

(* sorts list of terms such that constants come before compound
   terms where the root has positive arity come before variables *)
let rec lex = function
  | [],[] -> 0
  | [],_ -> -1
  |_, [] -> 1
  | x::xs,y::ys -> let c = my_compare x y in if c=0 then lex (xs,ys) else c
and my_compare t t' =
 match t, t' with
  | T.Var x, T.Var y -> Var.compare x y
  | T.Fun _, T.Var _ -> -1
  | T.Var _, T.Fun _ -> 1
  | T.Fun(_,[]), T.Fun(_,t::ts) -> -1
  | T.Fun(_,t::ts), T.Fun(_,[]) -> 1
  | T.Fun(f,fs), T.Fun(g,gs) when f=g -> lex (fs,gs)
  | T.Fun(f,_), T.Fun(g,_) -> Fun.compare f g
;;

(* recursively flatten term with respect to all ac symbols *)
let rec flatten = function
 | T.Fun(f, ts) ->
  is_ac_symbol f >>= fun f_is_ac -> (
  if f_is_ac then
   map flatten ts >>= fun ts' ->
   match find_term_with f ts' with
    | None -> return (T.Fun(f, L.sort my_compare ts'))
    | Some (ti,ts'') -> flatten (T.Fun(f, (ts''@(T.args ti))))
  else
   map flatten ts >>= fun us -> return (T.Fun(f,us)))
 | v -> return v
;;

(* compute variant renaming *)
let variant_renaming s t =
 let rec is_variant (s,t) = function
  | None -> None
  | Some sigma -> ( match (s,t) with
   | T.Fun(f, ss),T.Fun(g,ts) when f = g ->
    if L.length ss <> (L.length ts) then None
    else L.foldr is_variant (Some sigma) (L.zip ss ts)
   | T.Var x, ((T.Var y) as t) ->
    (try Some (Sub.add x t sigma) with Sub.Inconsistent -> None)
   | _ -> None)
 in match is_variant (s,t) (Some Sub.empty) with
   None -> None
 | Some sigma -> if Sub.is_renaming sigma then Some sigma else None
;;

let rec normalize_term' v sub = function
 | T.Var x ->
  begin try
   (Sub.find x sub, sub, v)
  with
   | Not_found ->
    (T.Var v, Sub.add x (T.Var v) sub, Var.next v)
  end
 | T.Fun (f, ts) ->
  let fold_norm (l, sub, v) t =
   let (t', sub', v') = normalize_term' v sub t in (t'::l, sub', v')
  in
   let (ts', sub', v') = L.fold_left fold_norm ([], sub, v) ts in
   (T.Fun (f, L.rev ts'), sub', v')
;;

let normalize t =
 let (t', rho, _) = normalize_term' Var.zero Sub.empty t in
 (t', rho)
;;

let normalize_rule rule =
 let l, r = R.lhs rule, R.rhs rule in
 let (r', sub, v) = normalize_term' Var.zero Sub.empty r in
 let (l', sub, _) = normalize_term' v sub l in
 R.of_terms l' r', sub
;;

let normalize_both s t =
 (* if first term contains more variables, rename second after the first
    term. If both sides contain the same variables, fix one order for
    normalization (by T.compare) to hopefully keep result
    deterministic ... it doesnt. (neither side's variables subset!?)
    so normalize both terms, compare them then*)
 let s', _ = normalize s in
 let t', _ = normalize t in
 let l,r,b = if (T.compare s' t') < 0 then s,t,true else t,s,false in
 let rl,_ = normalize_rule (R.of_terms l r) in
 (R.lhs rl, R.rhs rl), b
;;

(* s',t' was obtained by flatten from s,t, so that variables are sorted 
   differently (s and t are already flattened wrt structure). To obtain
   more uniqueness, retrieve variable renamings sigma and tau from 
   unifying s,s' and t,t', respectively, and apply these to t' and s',
   yielding terms t'' and s''. Take s' or s'', whatever is smaller, and
   the same for t',t''. *) 
let take_smaller (s,t) (s',t') =
 try
  let sigma = Option.the (variant_renaming s s') in
  let tau = Option.the (variant_renaming t t') in
  let s'' = Sub.apply_term tau s' in
  let t'' = Sub.apply_term sigma t' in
  let min x y = if T.compare x y < 0 then x else y in
  return (min s' s'', min t' t'')
 with Failure _ ->
  T.to_stringm s >>= fun ss ->
  T.to_stringm s' >>= fun ss' ->
  T.to_stringm t >>= fun ts ->
  T.to_stringm t' >>= fun ts' ->
  Format.printf "No renaming: %s to %s, or %s to %s\n%!" ss ss' ts ts';
  return (s',t')
;;

let normalize_flatten_both s t =
(* project flatten (s,t) >>= fun (s,t) ->*)
 return (normalize_both s t) >>= fun (st,b) ->
 project flatten st >>= fun st' -> (* sort variables *)
 (*take_smaller st st' >>= fun st ->*) return (st',b)
;;

let ac_equivalent s t =
 normalize_flatten_both s t >>= fun ((s,t),_) ->
 return (s = t)
;;

let compare_size ((r1,_, _), _, (r2, _, _)) ((s1,_, _), _, (s2, _, _)) =
 let rule_size r =
  (T.size (R.lhs r)) + (T.size (R.rhs r))
 in
 let rsize = (rule_size r1) + (rule_size r2) in
 let ssize = (rule_size s1) + (rule_size s2) in
 (Pervasives.compare rsize ssize)
;;

(* functions to skolemize a term *)
let rec skolemization' s = function
 | T.Var x ->
  if Sub.mem x s then M.return s
  else M.fresh_fun >>=
   M.create_fun_name >>= (M.create_fun 0) >>= (fun c ->
   M.return (Sub.add x (T.Fun(c,[])) s))
 | T.Fun (f,ts) -> M.foldl skolemization' s ts
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
 | T.Var _ -> []
 | T.Fun (f, ts) as t ->
  L.fold_lefti
   (fun i res y ->
    let p' = Pos.add_last i p in
    L.rev_append (nonvar_subterms' p' y) res
   )
   [t, p]
   ts
;;

(* only proper subterms! *)
let nonvar_pos_proper_subterms = function
 | T.Var _ -> []
 | T.Fun (f, ts) ->
  L.fold_lefti
   (fun i res y ->
    let p' = Pos.make i in
    L.rev_append (nonvar_subterms' p' y) res
   )
   []
   ts
;;

let rec rename_to sigma t t' =
 try (
  match t, t' with
   | T.Var x, T.Var y -> VSub.add x y sigma
   | T.Fun (f, ts), T.Fun (f', ts') when f = f' -> 
    L.fold_left2 rename_to sigma ts ts'
   | _ -> raise Elogic.Not_matchable
 ) with VSub.Inconsistent -> raise Elogic.Not_matchable
;;

let fresh_fun name a sigma = Sig.create_fun a name sigma

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

let test () =
 let sigma = Sig.empty 20 in
 let x,sigma = Sig.create_var "x" sigma in
 let y,sigma = Sig.create_var "y" sigma in
 let z,sigma = Sig.create_var "z" sigma in
 let u,sigma = Sig.create_var "u" sigma in
 let v,sigma = Sig.create_var "v" sigma in
 let w,sigma = Sig.create_var "w" sigma in
 let x,y,z,u,v,w = T.Var x, T.Var y, T.Var z, T.Var u, T.Var v, T.Var w in
 let f,sigma = Sig.create_fun 2 "f" sigma in
 let f,sigma = Sig.set_theory f U.Label.AC sigma in
 let a,sigma = Sig.create_fun 0 "a" sigma in
 let b,sigma = Sig.create_fun 0 "b" sigma in
 let c,sigma = Sig.create_fun 0 "c" sigma in
 let d,sigma = Sig.create_fun 0 "d" sigma in
 let e,sigma = Sig.create_fun 0 "e" sigma in
 let g,sigma = Sig.create_fun 1 "g" sigma in
 let a_ = T.Fun(a, []) in
 let c_ = T.Fun(c, []) in
 let d_ = T.Fun(d, []) in
 let e_ = T.Fun(e, []) in
 let faa = T.Fun(f, [a_;a_]) in
 let fxx = T.Fun(f, [x;x]) in
 let fyy = T.Fun(f, [y;y]) in
 let fzz = T.Fun(f, [z;z]) in
 let fax = T.Fun(f, [a_;x]) in
 let gx = T.Fun(g, [x]) in
 let gu = T.Fun(g, [u]) in
 let gfaa = T.Fun(g,[faa]) in
 let gfax = T.Fun(g,[fax]) in
 let rec flist = function
  [s; t] -> T.Fun(f, [s; t])
 | s :: ts -> T.Fun(f, [s; flist ts])
 | _ -> failwith "Unexpected pattern"
 in
 (* testing flatten *)
 let run_flatten t = Either.right (M.run sigma (
  flatten t >>= fun t ->
  T.to_stringm t >>= fun ts ->
  Format.printf "Flattened: %s\n%!" ts;
  return ()))
 in
 run_flatten (flist [fyy; fax]);
 run_flatten fax;
 run_flatten (flist [z;c_;d_;e_]);
 run_flatten (flist [fxx;a_;gfaa]);
 run_flatten (flist [fzz;gx;gu;gfax; fzz]);
;;


let context_subterm t (p,s) =
 let t' = T.subterm p t in
 let c = C.of_term p t in
 if L.is_empty s then c,t' 
 else
  let f,args = Option.the (T.root t'), T.args t' in
  let split i t (a,a') = if L.mem i s then (t::a,a') else (a,t::a') in
  let args, args' = L.foldri split ([],[]) args in
  let subterm = T.Fun(f,args) in
  let c' =
   if L.is_empty args' then C.Hole
   else C.More(f,args',C.Hole,[])
  in
  let context = C.compose c' c in
  context, subterm
;;

let subterm t p = snd (context_subterm t p)

let replace p tp t =
 let c,_ = context_subterm t p in
 C.apply tp c
;;

let subsets_ge2 n =
 let rec subsets k = 
  if k = n then [[]] else
  let ns = subsets (k+1) in
  L.rev_append (L.map (L.cons k) ns) ns
 in
 L.filter (fun l -> L.length l > 1) (subsets 0)
;;



let rec pos p = function
 | T.Var _ as x -> return (if p x then [Pos.root,[]] else [])
 | T.Fun (f,ts) as t ->
   is_ac_symbol f >>= fun f_is_ac ->
   let add_pair i (p,l) = Pos.add_first i p,l in
   let addi i t = pos p t >>= (return <.> (L.map (add_pair i))) in
   flat_mapi addi ts >>= fun ps ->
   if not f_is_ac then
    return (if p t then (Pos.root,[])::ps else ps)
   else
    let ss = subsets_ge2 (L.length ts) in
    return (if p t then (L.map (Pair.make Pos.root) ss) @ ps else ps) 
;;

let funs_pos = pos T.is_fun

let funs_pos_below_root t = 
 funs_pos t >>= (return <.> (L.filter (fun (p,_) -> p <> Pos.root)))
;;

(*
let remove_root = L.filter (fun (p,_) -> p <> Pos.root)

let remove_ac_root t pos = 
 match t with
  | T.Var _  -> []
  | T.Fun(_,ts) -> L.remove (Pos.root,List.mapi (fun i _ -> i) ts) pos
;;
*)

(* compute pairwise non-symmetric positions *)
let symmetric_renamings rl = 
 let vs = R.vars rl in
 let ps = L.combinations (L.length vs) (L.map T.make_var vs) in
 let ps = L.remove (L.map T.make_var vs) ps in
 let ps = L.map (fun p -> Sub.of_list (L.zip vs p)) ps in 
 let mapm f r = project f r >>= fun (l,r) -> return (R.of_terms l r) in
 let sub s = mapm (flatten <.> (Sub.apply_term s)) (R.to_terms rl) in
 map (apply sub return <.> Pair.create) ps >>=
 (return <.> (L.map snd) <.> (L.filter (fun (rl',s) -> rl' = rl)))
;;

let exists_symmetric t sigmas p ps =
 let ts = L.map (flip Sub.apply_term t) sigmas in
 let expos t' = L.exists (fun p' -> (subterm t p) = (subterm t' p')) ps in
 let b = L.exists expos ts in
(* Format.printf "Symmetry for %s in %s: %i \n%!" (to_string p) tss (if b then 1 else 0);*)
 b
;;

let rec filter_nonsymmetric t sigmas = function
 | [] -> return []
 | p :: ps -> 
  filter_nonsymmetric t sigmas ps >>= fun ps ->
  return (if exists_symmetric t sigmas p ps then ps else p::ps)
;;

let funs_pos_nonsymm rl = 
 (*R.to_stringm rl >>= fun rls ->*)
 funs_pos (R.lhs rl) >>= fun pos ->  
 symmetric_renamings rl >>= fun rens ->
 filter_nonsymmetric (R.lhs rl) rens pos >>= fun pos' ->
 (*if L.length pos <> (L.length pos') then
  Format.printf "Removed %s for %s (%i rens)\n%!" 
   (all_to_string (L.diff pos pos')) rls (L.length rens);*)
 return pos'
;;

let fun_root_pos = function
 | T.Var _ -> return []
 | T.Fun (f,ts) ->
   is_ac_symbol f >>= fun f_is_ac ->
   if not f_is_ac then
    return [Pos.root,[]]
   else
    return [Pos.root,L.mapi (fun i _ -> i) ts]
;;

(* find index of variable added for extended rule *)
let common_subterm_index rl =
 match R.to_terms rl with
  | T.Fun(f,ts), T.Fun(g,ss) when f=g ->
   L.position (fun t -> (T.is_var t) && (L.mem t ss)) ts
  | _ -> failwith "Inappropriate Rule in common_subterm_index"
;;


let funs_pos_extended orule t =
 let contain i (p,s) = (p = Pos.root) && L.mem i s in
 let i = common_subterm_index orule in
 funs_pos t >>= fun pos ->
 let pos = L.filter (contain i) pos in
 (*Format.printf "Positionsx: %s\n%!" (all_to_string pos);*)
 return pos
;;

let rec unflatten = function
  | T.Fun(f,ts) ->
   is_ac_symbol f >>= fun f_is_ac ->
   if f_is_ac && (List.length ts > 2) then(
    unflatten (List.hd ts) >>= fun x ->
    unflatten (T.Fun(f, List.tl ts)) >>= fun t ->
    return (T.Fun(f,[x;t])))
   else
    map unflatten ts >>= fun ts -> return (T.Fun(f,ts))
  | t -> return t
;;

let rec remove_labels = function
  | T.Fun(f,ts) ->
   is_ac_symbol f >>= fun f_is_ac ->
   (if f_is_ac then drop_theory f else return f) >>= fun f ->
    map remove_labels ts >>= fun ts -> return (T.Fun(f,ts))
  | t -> return t
;;

let readable t =  unflatten t >>= remove_labels
