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
module M = Monad;;

(*** OPENS ********************************************************************)
open Prelude;;
open Util;;

(*** MODULES ******************************************************************)
module Make (L : LABEL) = struct
 (*** MODULES *****************************************************************)
 module E = Elogic.Make (L);;
 module M = M.Make (L);;
 module Rule = Rule.Make (L);;
 module Sub = Substitution.Make (L);;
 module Term = Term.Make (L);;
 module Trs = Trs.Make (L);;

 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type term = Term.t;;
 type rule = Rule.t;;
 type trs = Trs.t;;
 type position = Position.t;;

 type step = {
  term : Term.t;
  pos :  Position.t;
  rule :  Rule.t;
  lab : int list;
 };;
 type t = Full;;
 
 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;

 (*constructors for step*)
 let step_make t p r = {
   term = t; 
   pos = p; 
   rule = r;
   lab = [];
 };;
 (*accessors for step*)
 let step_get_term s = s.term;;
 let step_get_pos s = s.pos;;
 let step_get_rule s = s.rule;;
 let step_get_lab s = s.lab;;
 let step_add_lab_left s l  = {s with lab = l::s.lab};;
 let step_add_lab_right s l = {s with lab = s.lab@[l]};;
 (*accessors end*)
 (*combined accessors for step*)
 let step_get_reduct s = 
  Rule.rewrite (step_get_term s) (step_get_pos s) (step_get_rule s)
 ;;
 (*combined accessors end*)

(*
 let join_get_sequences (ls,rs) = (ls,rs);;
 let join_to_join (ls,rs) = (ls,rs);;
 let sequence_to_steps ss = ss;;
 let sequence_of_steps ss = ss;;
 *)

 let positions_rules trs s t = 
  let rs = Trs.to_list trs in
  let ps = Term.funs_pos s in
  let prs = List.product ps rs in
  List.filter (fun (p,r) -> Rule.rewrites s p r t) prs
 ;;

(*get steps between [s] and [t]*)
 let step trs s t = 
  let prs = positions_rules trs s t in
 (*TODO: give all steps *)
  List.hd (List.map (fun (p,r) -> step_make s p r)  prs)
 ;;

 let fprintf_lab fmt lab = 
 Format.fprintf fmt "%a" (List.fprintf Int.fprintf ",") lab
 ;;

 let fprintfm_step fmt ?(reduct=true) s = 
  Format.fprintf fmt "@[";
  Term.fprintfm fmt (step_get_term s) >>= fun _ ->
  Format.fprintf fmt " -%a[%a]-> " Position.fprintf (step_get_pos s) 
   fprintf_lab (step_get_lab s);
  (if reduct then
    Term.fprintfm fmt (step_get_reduct s) >>= fun _ -> M.return ()
  else M.return ()) >>= fun _ ->
  Format.fprintf fmt "@]";
  M.return ();
 ;;

 let fprintfm_sequence fmt = function
  | [] -> M.return ();
  | seq -> 
   M.iter (fprintfm_step ~reduct:false fmt) (List.init seq) >>= fun _ ->
   fprintfm_step fmt ~reduct:true (List.last seq) >>= fun _ ->
   M.return ();
 ;;


(*get rewrite sequences *)
 let rec sequence trs = function
  | [] -> []
  | [x] -> []
  | s::t::ts -> step trs s t::sequence trs (t::ts)
 ;;

  (* Rewrite Strategies *)
 let rec reducts n acc ts trs = match n with
  | 0 -> acc
  | n ->
   let rs = List.unique_hash (List.flat_map (flip Trs.reducts trs) ts) in
   let acc' = acc@ts in
   let ts' = List.diff rs acc' in
   if ts' = [] then acc'
   else reducts (n-1) (acc'@ts') ts' trs
 ;;

 let reducts ?(s = Full) ?(n = ~-1) t trs = 
  if s <> Full then failwith "rewrite: strategy not supported";
  reducts n [] [t] trs
 ;;
 
 (*compute rewrite closure as set of nodes (s,t) if s -> t*)
 (*acc: n-rewrite closure*)
 (*ts: pairs (s,t) obtained in (n-th) iteration*)
 let closure1 trs s = List.map (fun t -> (s,t)) (Trs.reducts s trs);;

 let rec closure n acc ts trs = match n with
  | 0 -> (acc,ts)
  | n ->
   let rs = List.flat_map (fun (_,t) -> closure1 trs t) ts in
   let acc' = acc@ts in
   let ts' = List.diff (List.unique_hash rs) acc' in
   if ts' = [] then (acc',[])
   else closure (n-1) (acc@ts') ts' trs
 ;;

 let extend t cs u ps =
  if t = u then 
   [ps]
  else
   let pre = List.filter (fun (_,q) -> q = u && q <> t) cs in
   List.map (fun (p,_) -> p::ps) pre
 ;;


(* [pps] are paths from [t] to one of [cs] of length at most [n] *)
 let rec paths n t cs pps =
  if n = 0 then 
   List.filter (function p::ps -> p = t) (List.unique_hash pps)
  else
   let pps' = List.flat_map (function p::ps -> extend t cs p (p::ps)) pps in
   paths (n-1) t cs pps'
 ;;

 let sequences ?(s = Full) ?(n = ~-1) t u trs = 
  if s <> Full then failwith "rewrite: strategy not supported";
  let cs,_ = closure n [] [(t,t)] trs in
  paths n t cs [[u]]
 ;;

 let are_joinable ?(s = Full) ?(n = ~-1) u v trs = 
  List.intersect (reducts ~n:n u trs) (reducts ~n:n v trs) <> []
 ;;

 let joins_c n t cs0 u cs1 c =
  let ps0 = paths n t cs0 [[c]] in
  let ps1 = paths n u cs1 [[c]] in
  List.product ps0 ps1
 ;;

 let common cs0 cs1 = 
  List.unique_hash (List.intersect (List.map snd cs0) (List.map snd cs1))
 ;;

 let rec join n t (cs0,ts0) u (cs1,ts1) trs =
  let cs0 = cs0@ts0 and cs1 = cs1@ts1 in
  let cs = common cs0 cs1 in
  if cs <> [] then List.flat_map (joins_c n t cs0 u cs1) cs
  else join (n+1) t (closure 1 cs0 ts0 trs) u (closure 1 cs1 ts1 trs) trs
 ;;

 let min_joins ?(s = Full) t u trs =
  if s <> Full then failwith "rewrite: strategy not supported";
  join 0 t ([],[(t,t)]) u ([],[(u,u)]) trs
 ;;
 
 (* Confluence *)
 let critical_pairs r =
  let cp (r,p,r') =
   Rule.rename r >>= fun r ->
   let l = Rule.lhs r and l' = Rule.lhs r' and r' = Rule.rhs r' in
   let s = E.unify (Term.subterm p l) l' in
   M.return (Sub.apply_term s (Term.replace p r' l),Sub.apply_term s (Rule.rhs r))
  in
  Trs.overlaps r >>= fun ols ->
  M.map cp ols >>= fun cps ->
  M.return (List.unique cps) 
 ;;
 
 let is_wcr ?(s = Full) ?(n = ~-1) r =
  critical_pairs r >>=
  (M.return <.> List.for_all (fun (u,v) -> are_joinable ~s:s ~n:n u v r))
 ;;
end
