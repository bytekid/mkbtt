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
open Processors;;
open Rewritingx;;
open Util;;
open Parsec;;
open Term;;
open Parser;;

(*** MODULES ******************************************************************)
module Var = Variable;;
module Fun = Function;;
module P = Problem;;
module S = Rewritingx.Signature;;

(*** TYPES ********************************************************************)
type strategy = Full | Innermost | Outermost;;

type 'a rule = Std of 'a | Rel of 'a | Dp of 'a;;

type declaration =
 | Comment
 | Vars
 | Rules of Rule.t rule list
 | Strategy of strategy
 | Funs of (Fun.t * Rewritingx.Function.t) list
;;

(*** FUNCTIONS ****************************************************************)
let rec anylist i = if i > 0 then (any >>= function
  | '(' -> anylist(i+1)
  | ')' -> anylist(i-1)
  | _   -> anylist i
) else (
  (notchar ')' >>= function
    | '(' -> anylist(i+1)
    | _   -> anylist i
  ) <|> (return())
);;

let anylist = (anylist 0 >> return Comment) <?> "comment";;

let create_var id =
  get_state   >>= fun s -> let (x,s) = S.create_var id s in
  set_state s >> return x
;;

let ident = (
  many1(noneof"(),\n\t\r ") >>= fun cs ->
  spaces >> return(String.of_char_list cs)
) <?> "identifier"
;;

let var = (ident >>= create_var) <?> "variable";;
let vars = many var >> return Vars;;

let thfun l str =
 let create n =
  get_state   >>= fun s -> 
  let f',s = S.create_fun 2 n s in
  let f,s = S.set_theory f' l s in 
  set_state s >>
  return (f',f)
 in
 (ident >>= create) <?> (str ^ " function symbol");;

let acfuns =  many (thfun Label.AC "ac");;
let cfuns =  many (thfun Label.C "c");;

let strategy = many1 letter >>= function
 | ['I';'N';'N';'E';'R';'M';'O';'S';'T'] -> return (Strategy Innermost)
 | ['O';'U';'T';'E';'R';'M';'O';'S';'T'] -> return (Strategy Outermost)
 | _ -> failwith "STRATEGY is not supported"
;;

let rule lhs rhs =
  let lhs = lhs <?> "left-hand side" in
  let rhs = rhs <?> "right-hand side" in 
    lhs >>= fun l ->
    string "->"  >> (
      (char '=' >> spaces >>
        rhs >>= fun r ->
        return (Rel (Rule.of_terms l r))
      ) <|>
      (char 't' >> spaces >>
        rhs >>= fun r ->
        return (Dp (Rule.of_terms l r))
      ) <|>
      (spaces >>
        rhs >>= fun r ->
        return (Std (Rule.of_terms l r))

    )
  ) <?> "rule"
;;

let rules l r s = sep_end_by (rule l r) s >>= fun rs -> return (Rules rs);;

let split_rules = List.foldr (fun d (std, rel, dps) -> match d with
  | Std r -> (r::std, rel, dps)
  | Rel r -> (std, r::rel, dps)
  | Dp  r -> (std, rel, r::dps))
  ([], [], [])
;;

let spec d = many(between (char '(' >> spaces) d (char ')' >> spaces));;
let trs_of_list = Trs.of_list <.> List.unique_hash;;

let theory =
 let decl = many1 letter >>= function
  | ['A';'C'] -> spaces >> acfuns
  | ['C'] -> spaces >> cfuns
  | _ -> return []
 in spaces >> spec decl >>= fun fs -> return (Funs (List.concat fs))
;;

let equations trs =
 get_state >>= fun s ->
 let fs = List.filter (fun f -> S.is_theory Label.AC f s) (Trs.funs trs) in
 let x,s = S.fresh_var s in 
 let y,s = S.fresh_var s in
 let z,s = S.fresh_var s in
 let x,y,z = Term.Var x, Term.Var y, Term.Var z in
 let assoc f = (Term.Fun(f, [Term.Fun(f,[x;y]);z])),
  Term.Fun(f,[x;Term.Fun(f,[y;z])]) in
 let comm f = (Term.Fun(f, [x;y])), Term.Fun(f,[y;x]) in
 let of_pair = Util.uncurry Rule.of_terms in
 let rules f = [of_pair (assoc f); of_pair (comm f)] in
 let eqs = Trs.of_list (List.flat_map rules fs) (*Trs.empty*) in
 return (Trs.union eqs (Trs.invert eqs))
;;

let replace m trs =
 let rec replace = function
  | Term.Var _ as x -> x
  | Term.Fun(g, ts) when not (List.mem_assoc g m) ->
   Term.Fun(g, List.map replace ts)
  | Term.Fun(g, ts) -> let g' = List.assoc g m in
   Term.Fun(g', List.map replace ts)
 in
 Trs.project (Rule.project replace) trs
;;

let problem decl =
  spaces >> spec decl >>= fun ds -> eoi >>
  let (std, rel, dps, s),acsymbols =
    List.foldl (fun (r,b) d -> match (r, d) with
      | ((std, rel, dps, s), Rules rs)   ->
        let (xs, ys, zs) = split_rules rs in
        (std @ xs, rel @ ys, dps @ zs, s),b
      | ((std, rel, dps, _), Strategy s) -> (std, rel, dps, s),b
      | (_,Comment) | (_,Vars) -> r,b
      | (_,Funs m) -> r,m) (([], [], [], Full),[]) ds
  in
  let s = match s with
    | Full -> P.Full
    | Innermost -> P.Innermost
    | Outermost -> P.Outermost
  in
  match (std, rel, dps) with
    | (rs, [], []) ->
      if List.is_empty acsymbols then
       return (P.make_sp P.All s (trs_of_list rs))
      else
       let trs = replace acsymbols (trs_of_list rs) in
       equations trs >>= fun eqs ->
       return (P.make_ep P.All s eqs trs)
    | (ss, ws, []) ->
      return (P.make_rp P.All s (trs_of_list ss) (trs_of_list ws))
    | (ws, [], ss) ->
      return (P.make_dp P.All s (trs_of_list ss) (trs_of_list ws) P.Complete)
    | (_, _, _) ->
      failwith "mixture of relative rules and DPs"
;;

let of_channel problem chin =
  let m t = get_state >>= (return <.> Pair.make t) in
  let m = problem >>= m in
  match run m (S.empty 100) (StringInput.of_channel chin) with
    | Left e -> Pervasives.failwith(Error.to_string e)
    | Right x -> x
;;

(*** MODULES ******************************************************************)
module Trs = struct
 let term = get_state >>= (Term.parse <.> S.var_names);;
 
 let decl = many1 letter >>= function
   | ['V';'A';'R']                     -> spaces >> vars
   | ['T';'H';'E';'O';'R';'Y']         -> spaces >> theory
   | ['R';'U';'L';'E';'S']             -> spaces >> rules term term (return())
   | ['S';'T';'R';'A';'T';'E';'G';'Y'] -> spaces >> strategy
   | _                                 -> anylist
 ;;
 
 let of_channel = of_channel(problem decl);;
end

module Srs = struct
 let var =
   get_state >>= fun s ->
   let(x,s) = S.fresh_var s in
   let(_,s) = S.create_var_name x s in
   set_state s >>
   return(Var x)
 ;;
 
 let create_fun id =
  let id = String.xml_entities_encode id in
  get_state >>= fun s -> let(f,s) = S.create_fun 1 id s in
  set_state s >> Parser.return f
 ;;
   
 let string_of_idents =
   foldr (fun id t -> create_fun id >>= fun f -> return(Fun(f,[t])))
 ;;

 let lhs x = many_till ident (string"->") >>= string_of_idents x;;
 let rhs x = many_till ident (char ',' <|> char ')') >>= string_of_idents x;;
 
 let decl x = many1 letter >>= function
   | ['R';'U';'L';'E';'S']             ->
     spaces >> rules (lhs x) (rhs x) (char ',' >> spaces)
   | ['S';'T';'R';'A';'T';'E';'G';'Y'] -> spaces >> strategy
   | _                                 -> anylist
 ;;
 
 let of_channel = of_channel(var >>= fun x -> problem(decl x));;
end
