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
 module Parser = Parser.Make (L);;
 module Xml = Parser.Xml;;
 module S = Signature.Make (L);;
 module Sub = Substitution.Make (L);;
 module Term = Term.Make (L);;

 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type 'a p = 'a Parser.t;;
 type 'a x = 'a Xml.t;;
 type substitution = Sub.t;;
 type term = Term.t = Var of Variable.t | Fun of Function.t * term list;;
 type t = Term.t * Term.t;;
 
 (*** FUNCTIONS ***************************************************************)
 (* Parsers *)
 let (<?>) = Parser.(<?>);;
 let (>>=) = Parser.(>>=);;
 let (>>) = Parser.(>>);;

 let parse vars =
  Term.parse vars <?> "left-hand side" >>= fun lhs ->
  Parser.lex (Parser.string "->") <?> "arrow '->'" >>
  Term.parse vars <?> "right-hand side" >>= fun rhs ->
  Parser.return (lhs,rhs)
 ;;
 
 let of_string vars input =
  M.(>>=) M.get (fun s ->
   let m t = Parser.get_state >>= (Parser.return <.> Pair.make t) in
   let m = parse vars >>= m in
   match Parser.run m s (Parsec.StringInput.of_string input) with
    | Left e -> M.fail (Parsec.Error.to_string e)
    | Right(t,s) -> M.(>>) (M.set s) (M.return t))
 ;;
 
 (* xml transformer *)
 let (>>=) = Xml.(>>=);;
 let (>>) = Xml.(>>);;
 let (<?>) = Xml.(<?>);;
 let (<?) = Xml.(<?);;

 let xml = "rule" <?> Xml.node(
   "lhs" <?> Xml.node Term.xml >>= fun l ->
   "rhs" <?> Xml.node Term.xml >>= fun r ->
   Xml.return(l,r)
 ) <? "failed to transform rule";;

 let of_xml_string input =
   M.(>>=) M.get (fun s ->
   let module XP = Parsec.Xml.MakeParser(Parsec.StringParser) in
   match XP.parse (Parsec.StringInput.of_string input) with
     | Left e     -> M.fail(Parsec.Error.to_string e)
     | Right(_,x) ->
       let m t = Xml.get_state >>= (Xml.return <.> Pair.make t) in
       let m = xml >>= m in
       match Xml.run m s x with
         | Left e -> M.fail e
         | Right(t,s) -> M.(>>) (M.set s) (M.return t)
 );;
 
 let (<?>) = Util.(<?>);;
 let (>>=) = M.(>>=);;
 let (>>) = M.(>>);;
 
 (* Iterators *)
 let apply = Pair.apply;;
 let fold f = Pair.fold f f;;
 let count_fun f = fold ((+) <.> Term.count_fun f) 0
 let map = Pair.map;;
 let project = Pair.map;;
 let uncurry = Pair.uncurry;;
 
 (* Scan Functions *)
 let for_all f = Pair.for_all f f;;
 let exists f = Pair.exists f f;;
 
 (* Constructors and Destructors *)
 let invert = Pair.flip;;
 let lhs = fst;;
 let of_terms = Pair.make;;
 let reflect = map Term.reflect;;
 let reverse = map Term.reverse <?> "term is not a string";;
 let rhs = snd;;
 let to_terms = id;;
 
 (* Monadic Constructors and Destructors *)
 let rename r =
  E.renaming (lhs r) >>= (M.return <.> flip map r <.> Sub.apply_term)
 ;;
 
 (* Compare Functions *)
 let compare = compare;;
 let equal r r' = compare r r' = 0;;
 
 (* Rule Symbols *)
 let symbols f = uncurry List.union <.> map f;;
 let cons = symbols Term.cons;;
 let funs = symbols Term.funs;;
 let vars = symbols Term.vars;;
 let symbols = symbols Term.symbols;;
 let left_cons r = Term.cons (lhs r);;
 let left_funs r = Term.funs (lhs r);;
 let left_vars r = Term.vars (lhs r);;
 let left_symbols r = Term.symbols (lhs r);;
 let right_cons r = Term.cons (rhs r);;
 let right_funs r = Term.funs (rhs r);;
 let right_vars r = Term.vars (rhs r);;
 let right_symbols r = Term.symbols (rhs r);;
 
 let left_root r =
  (O.the <.> Term.root <.> lhs <?> "left-hand side is a variable") r
 ;;
 
 let right_root r =
  (O.the <.> Term.root <.> rhs <?> "right-hand side is a variable") r
 ;;
 
 let roots r =
  let root g = try [O.the (Term.root (g r))] with Failure _ -> [] in
  let f = root lhs and g = root rhs in if f = g then f else List.rev_append f g
 ;;
 
 (* Rewriting *)
 let rewrite t p r = 
  let s = E.match_term (Term.subterm p t) (lhs r) in
  Term.replace p (Sub.apply_term s (rhs r)) t
 ;;

 let rewrites s p r t = try rewrite s p r = t with E.Not_matchable -> false;; 

 let reducts t r = 
  let f rs p = try (rewrite t p r :: rs) with E.Not_matchable -> rs in
  (List.foldl f [] <.> flip Term.fun_pos t <.> O.the <.> Term.root <.> lhs <?>
   "left-hand side is a variable") r
 ;;
 
 let redex_pos t r =
  let f p = E.matches (Term.subterm p t) (lhs r) in
  (List.filter f <.> flip Term.fun_pos t <.> O.the <.> Term.root <.> lhs <?>
   "left-hand side is a variable") r
 ;;
 
 (* Properties *)
 let is_build = for_all <.> Term.is_build;;
 let is_collapsing r = Term.is_var (rhs r);;
 let is_dummy = for_all Term.is_dummy;;
 
 let is_duplicating r =
  let rec count c x = function
   | Var y -> if x = y then c + 1 else c
   | Fun (_,ts) -> List.foldl (flip count x) c ts
  in
  let (l,r) = to_terms r in
  List.exists (fun x -> count 0 x l < count 0 x r) (Term.vars r)
 ;;
  
 let is_proper_embedded r =
  List.exists (Term.is_embedded (rhs r)) (Term.subterms (lhs r))
 ;;

 let is_embedded = uncurry (flip Term.is_embedded);;
 let is_flat = for_all Term.is_flat;;
 let is_ground = for_all Term.is_ground;;
 let is_linear = for_all Term.is_linear;;
 let is_shallow = for_all Term.is_shallow;;
 let is_string = for_all Term.is_string;;
 let is_size_preserving r = Term.size (lhs r) >= Term.size (rhs r);;
 let is_left_build fs = Term.is_build fs <.> lhs;;
 let is_left_dummy r = Term.is_dummy (lhs r);;
 let is_left_flat r = Term.is_flat (lhs r);;
 let is_left_ground r = Term.is_ground (lhs r);;
 let is_left_linear r = Term.is_linear (lhs r);;
 let is_left_shallow r = Term.is_shallow (lhs r);;
 let is_left_string r = Term.is_string (lhs r);;
 let is_right_build fs = Term.is_build fs <.> rhs;;
 let is_right_dummy r = Term.is_dummy (rhs r);;
 let is_right_flat r = Term.is_flat (rhs r);;
 let is_right_ground r = Term.is_ground (rhs r);;
 let is_right_linear r = Term.is_linear (rhs r);;
 let is_right_shallow r = Term.is_shallow (rhs r);;
 let is_right_string r = Term.is_string (rhs r);;

 let is_growing r =
  let l = lhs r in
  let check x = List.for_all ((>=) 1 <.> Pos.length) (Term.var_pos x l) in
  List.for_all check (List.intersect (Term.vars l) (Term.vars (rhs r)))
 ;;
 
 let is_variant r r' =
  try Sub.is_renaming (E.unify_problem [(lhs r,lhs r');(rhs r,rhs r')])
  with E.Not_unifiable -> false
 ;;
 
 let matches r r' =
  try const true (E.match_problem [(lhs r,lhs r');(rhs r,rhs r')])
  with E.Not_matchable -> false
 ;;
 
 let subsumes = flip matches;;
 let is_erasing = not <.> uncurry List.is_subset <.> map Term.vars;;
 let is_contained = uncurry (flip E.contains);;
 
 let is_rewrite_rule r =
  Term.is_fun (lhs r) && uncurry List.is_supset (map Term.vars r)
 ;;
 
 let is_normal_form t r = try reducts t r = [] with Failure _ -> false;;
 let is_redex t r = E.matches t (lhs r);;
 
 (* Monadic Properties *)
 let compute_overlaps f ps r r1 =
  rename r1 >>= fun r' ->
  let l = lhs r and l' = lhs r' in
  let g = Term.root l in
  let ps = if ps = [] then O.fold (flip Term.fun_pos l') [] g else ps in
  let sub p = Term.subterm p l' in
  let is_overlap p = try E.are_unifiable (sub p) l with Failure _ -> false in
  let check p = not (Pos.is_root p) || not (is_variant r r') in
  M.return (f r r1 (fun p -> check p && is_overlap p) ps)
 ;;
 
 let is_overlap p r = compute_overlaps (const (const List.exists)) [p] r;;
 let are_overlapping r = compute_overlaps (const (const List.exists)) [] r;;
 
 (* Miscellaneous *)
 let copy = id;;
 let hash r = Hashtbl.hash (map Term.hash r);;
 let depth = uncurry max <.> map Term.depth;;
 
 let overlaps r =
  let add f r r' os p = if f p then (r,p,r') :: os else os in
  compute_overlaps (fun r r' f ps -> List.foldl (add f r r') [] ps) [] r
 ;;
 
 (* Equational Logic *)
 let apply_sub s = map (Sub.apply_term s);;
 
 (* Printers *)
 let fprintf fmt r =
  F.fprintf fmt "@[%a@ ->@ %a@]" Term.fprintf (lhs r) Term.fprintf (rhs r)
 ;;

 let fprintfm fmt r =
  F.fprintf fmt "@["; Term.fprintfm fmt (lhs r) >>= fun _ ->
  F.fprintf fmt "@ ->@ "; Term.fprintfm fmt (rhs r) >>= fun _ ->
  M.return (F.fprintf fmt "@]")
 ;;

 let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;

 let to_stringm r =
  fprintfm F.str_formatter r >>= (M.return <.> F.flush_str_formatter)
 ;; 
end
