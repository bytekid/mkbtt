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

(*** MODULES ******************************************************************)
module E = Prelude.Elogic;;
module F = Format;;
module Fun = Prelude.Function;;
module M = Monad;;
module P = Position;;
module S = Substitution;;

(*** OPENS ********************************************************************)
open Util;;

(*** INCLUDES *****************************************************************)
include Prelude.Trs;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let (>>) = M.(>>);;

(* Labeling and Unlabeling Functions *)
let update_trs l r =
 M.map (fun r ->
  let u = Rule.lhs r and v = Rule.rhs r in
  l u >>= fun u -> M.lift (Rule.of_terms u) (l v)) (to_list r) >>=
 (M.return <.> of_list)
;;

let label_dp = update_trs Term.label_dp;;
let label_height = update_trs <.> Term.label_height;;
let unlabel_dp = update_trs Term.unlabel_dp;;
let unlabel_height = update_trs Term.unlabel_height;;

(* Miscellaneous *)
let etcap t r =
 let module Map = Map.Partial (Fun) in
 let rec generate_map = function
  | (Term.Fun (f,_) as l) :: ls ->
   Option.map (fun m ->
    let ls = catch Not_found (Map.find f) [] m in
    Map.add f (l :: ls) m) (generate_map ls)
  | Term.Var _ :: ts -> None
  | [] -> Some Map.empty
 in
 let make_var = M.fresh_var >>= (M.return <.> Term.make_var) in
 let rec etcap m = function
  | Term.Fun (f,ts) ->
    M.map (etcap m) ts >>= fun ts ->
    let t = Term.Fun (f,ts) and ls = catch Not_found (Map.find f) [] m in
    if List.exists (E.ground_matches t) ls then make_var else M.return t
  | _ -> make_var
 in
 Option.fold (flip etcap t) make_var (generate_map (lhs r))
;;

let cap f t r =
 let rec cap ls = function
  | Term.Var x -> f x
  | Term.Fun (f,ts) ->
   M.lift (Term.make_fun f) (M.map (cap ls) ts) >>= fun t ->
   if List.exists (E.are_unifiable t) ls then M.lift Term.make_var M.fresh_var
   else M.return t
 in
 cap (lhs r) t
;;

let tcap = cap (const (M.lift Term.make_var M.fresh_var));;
let icap = cap (M.return <.> Term.make_var);;

let linear r =
 fold (fun r m ->
  let l = Rule.lhs r and r = Rule.rhs r in
  let xs = List.times (List.map (flip Term.var_pos l) (Term.vars l)) in
  let ren ps = Term.ren ~p:(const <.> not <.> flip List.mem ps) l in
  let update ls = M.return <.> flip List.cons ls in
  M.foldl (fun ls ps -> ren ps >>= update ls) [] xs >>= fun ls -> m >>=
  flip (M.foldl (fun s l -> M.return (add (Rule.of_terms l r) s))) ls)
  (M.return empty) r 
;;

let linearize r =
 fold (fun r m ->
  let l = Rule.lhs r in
  let max p q = if P.length p > P.length q then p else q in
  let ps = List.map (List.foldl1 max <.> flip Term.var_pos l) (Term.vars l) in
  Term.ren ~p:(const <.> not <.> flip List.mem ps) l >>= fun l ->
  m >>= (M.return <.> add (Rule.of_terms l (Rule.rhs r)))) (M.return empty) r
;;

let ac_rencap t r =
 let ds = def_symbols r in
 let rec cap = function
  | Term.Var x -> M.return (Term.make_var x)
  | Term.Fun (f,ts) ->
   if List.mem f ds then M.lift Term.make_var M.fresh_var
   else M.lift (Term.make_fun f) (M.map cap ts)
 in (match t with
  | Term.Var x -> M.return (Term.make_var x)
  | Term.Fun (f,ts) -> M.lift (Term.make_fun f) (M.map cap ts))
 >>= Term.ren
;;

let is_strongly_nonoverlapping r =
  linearize r >>= fun r' ->
  overlaps r' >>= fun os ->
  if os = []
    then M.return true
    else M.return false
;;


let recursors r =
 let ds = def_symbols r in
 let is_recursor r = List.exists (flip List.mem ds) (Rule.right_funs r) in
 fold (fun r rs -> if is_recursor r then r::rs else rs) [] r
;;

(* right-hand sides of forward closures *)
let generate f t =
 let rec generate root = function
  | Term.Var x as t -> [(t,[x])]
  | Term.Fun (g,ts) ->
   let xs = List.rev_map (generate false) ts in
   let add (ts,xs) (ti,ys) = (ti::ts,List.rev_append xs ys) in
   let ys = List.flat_times add ([],[]) xs in
   let ts = List.map (Pair.apply (Term.make_fun g) id) ys in
   if root then ts else (Term.Fun (f,[]),[]) :: ts
 in
 let add s x =  S.add x (Term.Fun (f,[])) s in
 let s = List.foldl add S.empty (Term.vars t) in
 List.map (Pair.apply id (List.foldl (flip S.remove) s)) (generate true t)
;;

let sharp r =
 M.fresh_fun >>= fun f -> M.add_ari f 0 >>
 M.return (pair f (diff (unique (fold (fun r s ->
  let l = Rule.lhs r and r = Rule.rhs r in
  let update s (l,sub) = add (Rule.of_terms l (S.apply_term sub r)) s in
  List.foldl update s (generate f l)) empty r)) r))
;;

(* Printers *)
let fprintfx fmt r =
 F.fprintf fmt "@{<rules>"; M.iter (Rule.fprintfx fmt) (to_list r) >>= fun _ ->
 M.return (F.fprintf fmt "@}")
;;

let to_stringx r =
 fprintfx F.str_formatter r >>= (M.return <.> F.flush_str_formatter)
;;

let extend trs = 
 let ext r = Rule.extend r >>= 
  function None -> M.return [] | Some r -> M.return [r] in
 (M.lift of_list <.> (M.flat_map ext) <.> to_list) trs >>= fun trsx ->
 M.return (union trs trsx)
;;

(* SW *)
let srules trs = 
 M.filter (M.is_theory Label.AC) (funs trs) >>= fun funs ->
 M.fresh_var >>= fun x -> M.fresh_var >>= fun y -> M.fresh_var >>= fun z ->
 let x,y,z = Term.Var x, Term.Var y, Term.Var z in
 let rule1 f = (Term.Fun(f, [Term.Fun(f,[x;y]);z])), Term.Fun(f,[x;y]) in
 let rule2 f = (Term.Fun(f, [x;Term.Fun(f,[y;z])])), Term.Fun(f,[y;z]) in
 let of_pair = uncurry Rule.of_terms in
 let rules f = [of_pair (rule1 f); of_pair (rule2 f)] in
 let label r = M.project Term.label_dp (Rule.to_terms r) in
 M.map label (List.flat_map rules funs) >>= fun rs ->
 M.return (of_list (List.map (uncurry Rule.of_terms) rs))
;;

let theory trs =
 M.filter (M.is_theory Label.AC) (funs trs) >>= fun funs ->
 M.fresh_var >>= fun x -> M.fresh_var >>= fun y -> M.fresh_var >>= fun z ->
 let x,y,z = Term.Var x, Term.Var y, Term.Var z in
 let assoc f = (Term.Fun(f, [Term.Fun(f,[x;y]);z])), 
  Term.Fun(f,[x;Term.Fun(f,[y;z])]) in
 let comm f = (Term.Fun(f, [x;y])), Term.Fun(f,[y;x]) in
 let of_pair = uncurry Rule.of_terms in
 let rules f = [of_pair (assoc f); of_pair (comm f)] in
 let eqs = of_list (List.flat_map rules funs) in
 M.return (union eqs (invert eqs))
;;
