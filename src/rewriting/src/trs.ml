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
 module Rule = Rule.Make (L);;
 module Term = Term.Make (L);;

 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type 'a p = 'a Parser.t;;
 type 'a x = 'a Xml.t;;
 type rule = Rule.t;;
 type term = Term.t;;
 type t = Rule.t list;;
 
 (*** FUNCTIONS ***************************************************************)
 (* Parsers *)
 let (>>=) = Parser.(>>=);;
 let (>>) = Parser.(>>);;

 let parse vars = Parser.many1 (Rule.parse vars);;
 
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

 let xml = Xml.many Rule.xml <? "failed to transform TRS";;

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
 
 let (>>=) = M.(>>=);;
 let (>>) = M.(>>);;
 let (<?>) = Util.(<?>);;
 
 (* Iterators *)
 let flat_map = List.flat_map;;
 let fold f = List.foldr f;;
 let count_fun f = fold ((+) <.> Rule.count_fun f) 0;;
 let iter = List.iter;;
 let map = List.map;;
 let project = List.map;;
 
 (* Scan Functions *)
 let exists = List.exists;;
 let for_all = List.for_all;;
 let mem u = List.mem u;;
 
 (* Search Functions *)
 let choose r = (List.hd <?> "empty TRS") r;;
 let filter = List.filter;;
 let find = List.find;;
 
 (* Constructors and Destructors *)
 let add = List.cons;;
 let diff r = List.diff r;;
 let empty = [];;
 let invert r = map Rule.invert r;;
 let lhs r = List.unique_hash (fold (List.cons <.> Rule.lhs) [] r);;
 let of_list = id;;
 let partition = List.partition;;
 let reflect = map Rule.reflect;;
 let remove u = List.remove_all u;;
 let reverse = map Rule.reverse <?> "not a SRS";;
 let rhs r = List.unique_hash (fold (List.cons <.> Rule.rhs) [] r);;
 let singleton = List.singleton;;
 let to_list = id;;
 let intersect r = List.intersect r;;
 let union r = List.union r;;
 let unique r = List.unique_hash r;;
 
 let terms r =
  List.unique_hash (fold (fun r ts -> Rule.lhs r :: Rule.rhs r :: ts) [] r)
 ;;
 
 (* Monadic Constructors and Destructors *)
 let rename = M.map Rule.rename;;
 
 (* Miscellaneous *)
 let copy = id;;
 let depth = fold (max <.> Rule.depth) 0;;
 let hash r = Hashtbl.hash (List.rev_map Rule.hash r);;
 let size = List.length;;
 let overlaps r = M.flat_map (flip M.flat_map r <.> Rule.overlaps) r;;
 
 (* TRS Symbols *)
 let compute_symbols f = fold (List.union <.> f) [];;
 let cons = compute_symbols Rule.cons;;
 let funs = compute_symbols Rule.funs;;
 let symbols = compute_symbols Rule.symbols;;
 let vars = compute_symbols Rule.vars;;
 let left_cons r = compute_symbols Rule.left_cons r;;
 let left_funs r = compute_symbols Rule.left_funs r;;
 let left_symbols r = compute_symbols Rule.left_symbols r;;
 let left_vars r = compute_symbols Rule.left_vars r;;
 let right_cons r = compute_symbols Rule.right_cons r;;
 let right_funs r = compute_symbols Rule.right_funs r;;
 let right_symbols r = compute_symbols Rule.right_symbols r;;
 let right_vars r = compute_symbols Rule.right_vars r;;
 
 let roots f =
  List.unique_hash <.>
  fold (fun r fs -> try (f r) :: fs with Failure _ -> fs) []
 ;;
 
 let left_roots r = roots Rule.left_root r;;
 let right_roots r = roots Rule.right_root r;;
 let roots = List.unique_hash <.> fold (List.rev_append <.> Rule.roots) [];;
 let def_symbols = left_roots;;
 let con_symbols r = List.diff (funs r) (def_symbols r);;
 let left_con_symbols r = List.diff (left_funs r) (def_symbols r);;
 let right_con_symbols r = List.diff (right_funs r) (def_symbols r);;
 
 (* Rewriting *)
 let reducts t =
  List.unique_hash <.>
  flat_map (Rule.reducts t) <?> "left-hand side is a variable"
 ;;
 
 let rewrite t p = 
  let f r ts = try Rule.rewrite t p r :: ts with E.Not_matchable -> ts in
  List.unique_hash <.> fold f []
 ;;

 let rewrites s p trs t = 
 List.exists (fun r -> Rule.rewrites s p r t) trs;;
 
 (* Properties *)
 let is_build fs = for_all (Rule.is_build fs);;
 let is_collapsing r = exists Rule.is_collapsing r;;
 let is_dummy = for_all Rule.is_dummy;;
 let is_duplicating = exists Rule.is_duplicating;;
 let is_erasing = exists Rule.is_erasing;;
 let is_empty = List.is_empty;;
 let is_flat = for_all Rule.is_flat;;
 let is_ground = for_all Rule.is_ground;;
 let is_growing = for_all Rule.is_growing;;
 let is_linear = for_all Rule.is_linear;;
 let is_proper_subset r = List.is_proper_subset r;;
 let is_shallow = for_all Rule.is_shallow;;
 let is_size_preserving = for_all Rule.is_size_preserving;;
 let is_srs = for_all Rule.is_string;;
 let is_subset r = List.is_subset r;;
 let is_trs = for_all Rule.is_rewrite_rule;;
 let is_left_build fs r = for_all (Rule.is_left_build fs) r;;
 let is_left_dummy r = for_all Rule.is_left_dummy r;;
 let is_left_flat r = for_all Rule.is_left_flat r;;
 let is_left_ground r = for_all Rule.is_left_ground r;;
 let is_left_linear r = for_all Rule.is_left_linear r;;
 let is_left_shallow r = for_all Rule.is_left_shallow r;;
 let is_right_build fs r = for_all (Rule.is_right_build fs) r;;
 let is_right_dummy r = for_all Rule.is_right_dummy r;;
 let is_right_flat r = for_all Rule.is_right_flat r;;
 let is_right_ground r = for_all Rule.is_right_ground r;;
 let is_right_linear r = for_all Rule.is_right_linear r;;
 let is_right_shallow r = for_all Rule.is_right_shallow r;;
 
 let is_variant r s =
  let is_variant r s = for_all (flip exists s <.> Rule.is_variant) r in
  is_variant r s && is_variant s r
 ;;
 
 let is_constructor f r =
  let cs = con_symbols r in
  for_all (List.for_all (Term.is_build cs) <.> Term.args <.> f) r
 ;;
 
 let is_left_constructor = is_constructor Rule.lhs;;
 let is_right_constructor = is_constructor Rule.rhs;;
 let is_constructor = is_left_constructor;;
 let is_redex t = exists (Rule.is_redex t);;
 let is_normal_form t = for_all (Rule.is_normal_form t);;
 
 (* Monadic Properties *)
 let is_overlapping r = M.exists (flip M.exists r <.> Rule.are_overlapping) r;;
 
 let is_overlay r =
  let is_root (_,p,_) = Pos.is_root p in
  let check r r' = Rule.overlaps r r' >>= (M.return <.> List.for_all is_root) in
  M.for_all (flip M.for_all r <.> check) r
 ;;
 
 let is_applicative r =
  let rec check_sig b = function
   | [] -> M.return b
   | f::fs ->
    M.find_ari f >>= fun a ->
    if a = 0 then check_sig b fs
    else if a = 2 && not b then check_sig true fs else M.return false
  in
  check_sig false (funs r)
 ;;
 
 (* Compare Functions *)
 let compare = compare;;
 let equal r s = compare r s = 0;;
 let equivalent r s = List.is_subset s r && List.is_supset s r;;
 
 (* Printers *)
 let fprintf fmt = F.fprintf fmt "@[%a@]" (List.fprintf Rule.fprintf "@\n");;

 let fprintfm fmt r =
  F.fprintf fmt "@["; M.fprintf Rule.fprintfm "@\n" fmt r >>= fun _ ->
  M.return (F.fprintf fmt "@]")
 ;;

 let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;

 let to_stringm r =
  fprintfm F.str_formatter r >>= (M.return <.> F.flush_str_formatter)
 ;;
end
