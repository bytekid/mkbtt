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
module F = Format;;
module M = Monad;;
module O = Util.Option;;
module Pos = Position;;

(*** OPENS ********************************************************************)
open Prelude;;
open Util;;

(*** MODULES ******************************************************************)
module Make (L : LABEL) = struct
 (*** MODULES *****************************************************************)
 module E = Elogic.Make (L);;
 module M = M.Make (L);;
 (* module Parser = Parser.Make (L);; *)
 (* module Xml = Parser.Xml;; *)
 module S = Signature.Make (L);;
 module Sub = Substitution.Make (L);;
 module Term = Term.Make (L);;
 module Rule = Rule.Make (L);;
 module Trs = Trs.Make (L);;
 module Rewrite = Rewrite.Make (L);;

 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t
 type term = Term.t = Var of Variable.t | Fun of Function.t * term list;;
 type rule = Rule.t;;
 type step = Rewrite.step;;
 type trs = Trs.t;;
 type t = (step * step) * (step list * step list) list;;
 
 (*** FUNCTIONS ***************************************************************)
 (* Parsers *)
 let (<?>) = Util.(<?>);;
 let (>>=) = M.(>>=);;
 let (>>) = M.(>>);;
 
 (* Iterators *)
 (* Scan Functions *)
 (* Constructors and Destructors *)
 let peak (p,_) = p;;
 let joins (_,js) = js;;
 let make p js = (p,js);;

 (* Compare Functions *)
 let compare = compare;;
 let equal r r' = compare r r' = 0;;

 let diagram trs (r1,p,r2) =
  Rule.rename r1 >>= fun r1' ->
  let mu = E.unify (Rule.lhs r1') (Term.subterm p (Rule.lhs r2)) in
  let s = Sub.apply_term mu (Rule.lhs r2) in
  let t = Rule.rewrite s p r1 in
  let u = Rule.rewrite s Position.root r2 in
  let l = Rewrite.step_make s p r1  and r = Rewrite.step_make s Position.root r2  in
  let js = Rewrite.min_joins t u trs in
  let ss = List.map (Pair.map (Rewrite.sequence trs)) js in
  M.return ((l,r),ss)
 ;;

 let critical trs = 
  Trs.overlaps trs >>= fun os ->
  M.map (diagram trs) os;
 ;;
 
 (* label a diagram *)
 let rec label_seq label_step n = function
  | [] -> []
  | s::ss -> 
     let (m,s) = label_step n s in
     s::label_seq label_step m ss
 ;;
 
 let label_j label_step nl nr (ls,rs) = 
  let ls = label_seq label_step nl ls in
  let rs = label_seq label_step nr rs in
  (ls,rs)
 ;;
  
 let label label_step d = 
  let (l,r) = peak d in
  let (nl,l) = label_step 0 l in
  let (nr,r) = label_step 0 r in
  let js = List.map (label_j label_step nl nr) (joins d) in
  make (l,r) js
 ;;

 (*test decreasingness*)

 let lex_eq ls rs = List.for_all2 (=) ls rs;;

 let rec lex_gt ls rs = match ls,rs with
  | l::ls,r::rs when l > r -> true
  | l::ls,r::rs when l = r -> lex_gt ls rs
  | _ -> false
 ;;

 let is_decreasing_l2 alpha beta ls = 
  List.for_all (fun l -> lex_gt alpha l || lex_gt beta l) ls
 ;;

 let rec is_decreasing_l alpha beta = function
  | [] -> true
  | l::ls when lex_gt alpha l -> is_decreasing_l alpha beta ls
  | l::ls when lex_eq beta l -> is_decreasing_l2 alpha beta ls
  | ls -> is_decreasing_l2 alpha beta ls
 ;;

 let is_decreasing_s alpha beta ss = 
  is_decreasing_l alpha beta (List.map Rewrite.step_get_lab ss)
 ;;

 let is_decreasing_j alpha beta (ls,rs) =
  is_decreasing_s alpha beta ls &
  is_decreasing_s beta alpha rs
 ;;

 let is_decreasing d = 
  let (l,r) = peak d in
  let (nl,nr) = (Rewrite.step_get_lab l, Rewrite.step_get_lab r) in
  List.exists (is_decreasing_j nl nr) (joins d)
  ;;
 
 (*
 (* Printers *)
 let fprintf fmt r =
  (* F.fprintf fmt "@[%a@ ->@ %a@]" Term.fprintf (lhs r) Term.fprintf
  (rhs r) *)
  ();
 ;;
*)

 let fprintfm_peak fmt (l,r) = 
  Term.fprintfm fmt (Rewrite.step_get_reduct l) >>= fun _ ->
  F.fprintf fmt " <-%a- " Position.fprintf (Rewrite.step_get_pos l);
  Rewrite.fprintfm_step ~reduct:true fmt r >>= fun _ ->
  M.return ();
 ;;

 let fprintfm_join fmt (ls,rs) = 
  Rewrite.fprintfm_sequence fmt ls >>= fun _ ->
  Format.fprintf fmt "@\n";
  Rewrite.fprintfm_sequence fmt rs >>= fun _ ->
  M.return ();
 ;;

 let fprintfm fmt d =
  F.fprintf fmt "@[@[<1>peak:@\n";
  fprintfm_peak fmt (peak d) >>= fun _ ->
  F.fprintf fmt "@]@\n";
  F.fprintf fmt "@[<1>joins:";
  M.iter (fun j -> Format.fprintf fmt "@\n";
   fprintfm_join fmt j >>= fun _ -> M.return ()) (joins d) >>= fun _ ->
  F.fprintf fmt "@]@]";
  M.return ();
 ;;

(*
 let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;

 let to_stringm r =
  (* fprintfm F.str_formatter r >>= (M.return <.> F.flush_str_formatter) *)
  M.return ();
 ;; 
 *)
end
