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
module E = Util.Either;;
module F = Format;;
module Fun = Function;;
module M = Monad;;
module P = Util.Pair;;
module Pos = Position;;
module Var = Variable;;

(*** OPENS ********************************************************************)
open Prelude;;
open Util;;

(*** MODULES ******************************************************************)
module Make (L : LABEL) = struct
 (*** MODULES *****************************************************************)
 module M = M.Make (L);;
 module Parser = Parser.Make (L);;
 module Xml = Parser.Xml;;
 module S = Signature.Make (L);;

 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type 'a p = 'a Parser.t;;
 type 'a x = 'a Xml.t;;
 type state = S.t;;
 type t = Var of Var.t | Fun of Fun.t * t list;;
 
 (*** EXCEPTIONS **************************************************************)
 exception Fail;;
 
 (*** FUNCTIONS ***************************************************************)
 (* Parsers *)
 let (>>=) = Parser.(>>=);;
 let (>>) = Parser.(>>);;
 let (<?>) = Parser.(<?>);;
 let (<|>) = Parser.(<|>);;

 let rec parse vars =
  let ident = (Parser.many1 (Parser.noneof " \t\n\r()\",") >>= function
   | ['-';'>'] | ['=';'='] | ['-';'>';'='] | ['-';'>';'<';'-'] -> Parser.fail
   | i -> Parser.return i) <?> "identifier"
  in
  let create_fun id ar =
   let id = String.xml_entities_encode id in
   Parser.get_state >>= fun s -> let (f,s) = S.create_fun ar id s in
   Parser.set_state s >> Parser.return f
  in
  let create_var id =
   Parser.get_state >>= fun s -> let (x,s) = S.create_var id s in
   Parser.set_state s >> Parser.return x
  in
  Parser.lex ident >>= fun i ->
  let id = String.of_char_list i in
  (Parser.lex (Parser.char '(') >>
   Parser.sep_by (parse vars) (Parser.lex (Parser.char ',')) >>= fun ts ->
   create_fun id (List.length ts) >>= fun f ->
   Parser.lex (Parser.char ')') >> Parser.return (Fun (f,ts)))
  <|>
  (if List.mem id vars then create_var id >>= fun x -> Parser.return (Var x)
  else create_fun id 0 >>= fun f -> Parser.return (Fun (f,[])))
 ;;

 let of_string vars input =
  M.(>>=) M.get (fun s ->
   let m t = Parser.get_state >>= (Parser.return <.> P.make t) in
   let m = parse vars >>= m in
   match Parser.run m s (Parsec.StringInput.of_string input) with
    | Left e -> M.fail (Parsec.Error.to_string e)
    | Right (t,s) -> M.(>>) (M.set s) (M.return t))
 ;;

 (* xml transformer *)
 let (>>=) = Xml.(>>=);;
 let (>>) = Xml.(>>);;
 let (<?>) = Xml.(<?>);;
 let (<?) = Xml.(<?);;

 let rec xml() =
   let create_var id =
     Xml.get_state   >>= fun s ->
     let(x,s) = S.create_var id s in
     Xml.set_state s >>
     Xml.return(Var x)
   in
   let create_fun id ar =
     Xml.get_state   >>= fun s ->
     let(f,s) = S.create_fun ar id s in
     Xml.set_state s >>
     Xml.return f
   in
   Xml.choice [
     ("var" <?> Xml.string >>= create_var);
     ("funapp" <?> Xml.node(
       "name" <?> Xml.string               >>= fun id ->
       Xml.many("arg" <?> Xml.node(xml())) >>= fun ts ->
       create_fun id (List.length ts)      >>= fun f ->
       Xml.return(Fun(f,ts))
     ))
   ] <? "failed to transform term"
 ;;
 let xml = xml();;

 let of_xml_string input =
   M.(>>=) M.get (fun s ->
   let module XP = Parsec.Xml.MakeParser(Parsec.StringParser) in
   match XP.parse (Parsec.StringInput.of_string input) with
     | Left e     -> M.fail(Parsec.Error.to_string e)
     | Right(_,x) ->
       let m t = Xml.get_state >>= (Xml.return <.> P.make t) in
       let m = xml >>= m in
       match Xml.run m s x with
         | Left e -> M.fail e
         | Right(t,s) -> M.(>>) (M.set s) (M.return t)
 );;
 
 let (>>=) = M.(>>=);;
 let (>>) = M.(>>);;
 let (<?>) = Util.(<?>);;
 
 (* Compare Functions *)
 let compare = compare;;
 let equal s t = compare s t = 0;;
 
 (* Constructors *)
 let make_fun f ts = Fun (f,ts);;
 let make_var x = Var x;;
 
 let rec reflect = function
  | Var _ as x -> x
  | Fun (f,ts) -> Fun (f,List.rev_map reflect ts)
 ;;
 
 let rec replace p s t =
  if Pos.is_root p then s else (
   let (i,q) = Pos.split_first p in
   match t with
    | Var _ -> failwith "illegal position"; 
    | Fun (f,ts) ->
     let l = List.length ts in
     if i < 0 || i >= l then failwith "illegal position"
     else Fun (f,List.mapi (fun j -> if i = j then replace q s else id) ts))
 ;;
 
 let reverse t =
  let rec reverse fs = function
   | Var _ as x -> List.foldr (fun f t -> Fun (f,[t])) x fs
   | Fun (f,ts) ->
    if List.is_singleton ts then reverse (f :: fs) (List.hd ts)
    else failwith "not a string"
  in
  reverse [] t
 ;;
 
 let rec subterm p t =
  if Pos.is_root p then t else (
   let (i,q) = Pos.split_first p in
   match t with
    | Var _ -> failwith "illegal position"; 
    | Fun (f,ts) ->
     try subterm q (List.nth ts i)
     with Invalid_argument _ -> failwith "illegal position")
 ;;
 
 (* Monadic Constructors *)
 let ren ?(p = const (const true)) =
  let rec ren q = function
   | Var x as t ->
    if p q x then M.fresh_var >>= (M.return <.> make_var)
    else M.return t
   | Fun (f,ts) ->
    M.mapi (ren <.> flip Pos.add_last q) ts >>= (M.return <.> make_fun f)
  in
  ren Pos.root
 ;;
 
 let rename =
  let rec rename m = function
   | Var _ as x ->
    let fresh s = let (y,s) = S.fresh_var s in (y,(x,y)::m,s) in
    let find s = try (List.assoc x m,m,s) with Not_found -> fresh s in
    M.get >>= fun s -> let (x,m,s) = find s in M.set s >> M.return (Var x,m)
   | Fun (f,ts) ->
    let g ti (ts,m) = M.lift (P.apply (flip List.cons ts) id) (rename m ti) in
    M.foldr g ([],m) ts >>= fun (ts,m) -> M.return (Fun (f,ts),m)
  in
  M.lift fst <.> rename []
 ;;
 
 (* Term Symbols *)
 let symbols p f =
  let add s ss = if p s then f s :: ss else ss in
  let rec symbols = function
   | Var x -> add (Left x) []
   | Fun (f,ts) -> add (Right f) (List.flat_map symbols ts)
  in
  List.unique_hash <.> symbols
 ;;
 
 let cons =
  let rec cons = function
   | Var _ -> []
   | Fun (f,ts) ->
    let cs = List.flat_map cons ts in if ts = [] then f::cs else cs
  in
  List.unique_hash <.> cons
 ;;
 
 let funs = symbols (E.either (const false) (const true)) E.right;;
 let vars = symbols (E.either (const true) (const false)) E.left;;
 let symbols = symbols (const true) id;;
 let root = function Var _ -> None | Fun(f,_) -> Some f;;
 
 (* Properties *)
 let rec is_embedded s t = match (s,t) with
  | Var x, Var y -> x = y
  | Fun (f,ss), Fun (g,ts) ->
   (f = g && List.for_all2 is_embedded ss ts) || List.exists (is_embedded s) ts
  | _, Fun (f,ts) -> List.exists (is_embedded s) ts
  | _, _ -> false
 ;;
 
 let rec is_subterm s = function
  | Var _ as x -> x = s
  | Fun (_,ts) as t -> t = s || List.exists (is_subterm s) ts
 ;;
 
 let is_proper_subterm s = function
  | Var _ -> false
  | Fun (_,ts) -> List.exists (is_subterm s) ts
 ;;
 
 let is_cons = function Fun (_,[]) -> true | _ -> false;;
 let is_fun = function Var _ -> false | _ -> true;;
 let is_var t = not (is_fun t);;
 let is_flat = function Var _ -> true | Fun (_,ts) -> List.for_all is_var ts;;
 
 let rec is_build fs = function
  | Var _ -> true
  | Fun (f,ts) -> List.mem f fs && List.for_all (is_build fs) ts
 ;;
 
 let rec is_dummy = function
  | Var _ -> true
  | Fun (_,ts) -> ts = [] || (List.is_singleton ts && is_dummy (List.hd ts))
 ;;
 
 let rec is_ground = function
  | Var _ -> false
  | Fun (_,ts) -> List.for_all is_ground ts
 ;;
 
 let is_linear =
  let rec is_linear xs = function
   | Var x -> if List.mem x xs then raise Fail else x :: xs
   | Fun (_,ts) -> List.foldl is_linear xs ts
  in
  catch Fail (const true <.> is_linear []) false
 ;;
 
 let is_shallow =
  let rec is_shallow d = function
   | Var _ -> d <= 1
   | Fun (_,ts) -> let d = d + 1 in List.for_all (is_shallow d) ts
  in
  is_shallow 0
 ;;
 
 let rec is_string = function
  | Var _ -> true 
  | Fun (f, ts) -> List.is_singleton ts && is_string (List.hd ts)
 ;;
 
 (* Search Functions *)
 let rec mem_fun f = function
  | Var _ -> false
  | Fun (g,ts) -> f = g || List.exists (mem_fun f) ts
 ;;
 
 let rec mem_var x = function
  | Var y -> x = y
  | Fun (_,ts) -> List.exists (mem_var x) ts
 ;;
 
 (* Positions *)
 let rec pos p = function
  | Var _ as x -> if p x then [Pos.root] else []
  | Fun (_,ts) as t ->
   let addi i = List.map (fun p -> Pos.add_first i p) <.> pos p in
   let ps = List.flat_mapi addi ts in if p t then Pos.root::ps else ps
 ;;
 
 let fun_pos f = pos (function Fun (g,_) -> g = f | _ -> false);;
 let var_pos x = pos (function Var y -> y = x | _ -> false);;
 let funs_pos = pos is_fun;;
 let vars_pos = pos is_var;;
 let subterm_pos s = pos (equal s);;
 let pos = pos (const true);;
 
 (* Iterators *)
 let rec map f = function
  | Var _ as x -> x
  | Fun (g,ts) -> Fun (f g,List.map (map f) ts)
 ;;

 (* Folds *)
 let rec fold_funs op d = function
   | Var _ -> d
   | Fun (f, ts) -> op f (List.foldl (fold_funs op) d ts)
 ;;

 let rec count_fun f = fold_funs (fun g s ->
   if Fun.equal f g then s + 1 else s
 ) 0;;
 
 (* Miscellaneous *)
 let args = function Var _ -> [] | Fun (_,ts) -> ts;;
 let copy = id;;
 let hash = Hashtbl.hash;;
 
 let rec size = function
  | Var _ -> 1
  | Fun (_,ts) -> List.foldl (fun s -> (+) s <.> size) 1 ts
 ;;

 let rec fun_size = function
  | Var _ -> 0
  | Fun (_,ts) -> List.foldl (fun s -> (+) s <.> size) 1 ts
 ;;
 
 let rec var_size = function
  | Var _ -> 1
  | Fun (_,ts) -> List.foldl (fun s -> (+) s <.> size) 0 ts
 ;;
 
 let rec depth = function
  | Var _ -> 0
  | Fun (_,ts) ->
   if ts = [] then 0 else 1 + List.foldl (fun d -> max d <.> depth) 0 ts
 ;;
 
 let proper_subterms ?(p = const true) =
  let rec subterms = function
   | Var _ as x -> if p x then [x] else []
   | Fun (_,ts) as t ->
    let ss = List.flat_map subterms ts in if p t then t::ss else ss
  in
  List.unique_hash <.> List.flat_map subterms <.> args
 ;;
 
 let subterms ?(p = const true) t =
  let acc = proper_subterms ~p:p t in if p t then t::acc else acc
 ;;
 
 (* Printers *)
 let rec fprintf fmt = function
  | Var x -> Var.fprintf fmt x
  | Fun (f,ts) ->
   F.fprintf fmt "@[%a@[(%a)@]@]" Fun.fprintf f (List.fprintf fprintf ",") ts
 ;;

 let rec fprintfm fmt = function
  | Var x -> M.fprintf_var fmt x
  | Fun (f,ts) ->
   F.fprintf fmt "@["; M.fprintf_fun fmt f >>= fun _ ->
   (F.fprintf fmt "@[("; M.fprintf fprintfm "," fmt ts) >>= fun _ ->
   M.return (F.fprintf fmt ")@]@]")
 ;;

 let rec fprintfs s fmt = function
  | Var x -> S.fprintf_var fmt x s
  | Fun (f,ts) ->
   F.fprintf fmt "@[";
   let s = S.fprintf_fun fmt f s in
   F.fprintf fmt "@[(";
   let rec fprintfs_list s fmt = function
    | [] -> s
    | [t] -> fprintfs s fmt t
    | t::ts -> let s = fprintfs s fmt t in F.fprintf fmt ","; fprintfs_list s fmt ts
   in
   let s = fprintfs_list s fmt ts in
   F.fprintf fmt ")@]@]";
   s
 ;;


 let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;

 let to_stringm t =
  fprintfm F.str_formatter t >>= (M.return <.> F.flush_str_formatter)
 ;; 

 let to_strings s t =
  let s = fprintfs s F.str_formatter t in
  (s,Format.flush_str_formatter ())
 ;;
end
