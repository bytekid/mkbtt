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

(** Interface to parser for TPTP format.
 @author Sarah Winkler
 @since  2009/03/28 *)

(*** MODULES *************************************************************)
module M = U.Monad;;
module S = U.Signature;;
module Parser = U.Parser;;
module Term = U.Term;;
module StringInput = Parsec.StringInput;;
module Error = Parsec.Error;;

(*** OPENS ********************************************************************)
open Util;;

(*** TYPES ***************************************************************)

type literal = Equal of Equation.t | NotEqual of Equation.t

type conjecture = Negated | Plain

type clause_status = Axiom | Hypothesis | Conjecture of conjecture

(*** EXCEPTIONS **********************************************************)
(*** GLOBALS *************************************************************)
let axioms_prefix = ref ""
(*** FUNCTIONS ***********************************************************)
let (>>=) = Parser.(>>=);;
let (>>) = Parser.(>>);;
let (<?>) = Parser.(<?>);;
let (<|>) = Parser.(<|>);;

(*let rec parse_term =
 let var = (Parser.count 1 Parser.upper >>= fun c ->
  Parser.many (Parser.noneof " \t\n\r()\",") >>= function
   | ['-';'>'] | ['=';'='] | ['-';'>';'='] | ['-';'>';'<';'-'] -> Parser.fail
   | i -> Parser.return (c @ i)) <?> "identifier"
 in
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
 (Parser.lex var >>= fun i ->
  let id = String.of_char_list i in
  create_var id >>= fun x -> Parser.return (Term.Var x))
 <|>
  (Parser.lex ident >>= fun i ->
  let id = String.of_char_list i in
  (Parser.lex (Parser.char '(') >>
   Parser.sep_by (parse_term) (Parser.lex (Parser.char ',')) >>= fun ts ->
   create_fun id (List.length ts) >>= fun f ->
   Parser.lex (Parser.char ')') >> Parser.return (Term.Fun (f,ts)))
  <|>
  (create_fun id 0 >>= fun f -> Parser.return (Term.Fun (f,[]))))
 ;;*)

 let rec parse vars =
 let var = (Parser.count 1 Parser.upper >>= fun c ->
  Parser.many (Parser.noneof " \t\n\r()\",.") >>= function
   | ['-';'>'] | ['=';'='] | ['-';'>';'='] | ['-';'>';'<';'-'] -> Parser.fail
   | i -> Parser.return (c @ i)) <?> "identifier"
 in
  let ident = (Parser.many1 (Parser.noneof " \t\n\r()\",.") >>= function
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
 (Parser.lex var >>= fun i ->
  let id = String.of_char_list i in
  create_var id >>= fun x -> Parser.return (Term.Var x))
 <|>
  (Parser.lex ident >>= fun i ->
  let id = String.of_char_list i in
  (Parser.lex (Parser.char '(') >>
   Parser.sep_by (parse vars) (Parser.lex (Parser.char ',')) >>= fun ts ->
   create_fun id (List.length ts) >>= fun f ->
   Parser.lex (Parser.char ')') >> Parser.return (Term.Fun (f,ts)))
  <|>
  (create_fun id 0 >>= fun f -> Parser.return (Term.Fun (f,[]))))
;;

let parse_term = parse []


let parse_equality =
 Parser.lex (Parser.string "=") <?> "equality '='" >>
 parse_term <?> "right-hand side" >>= fun rhs ->
 Parser.return (fun lhs -> Equal(Equation.of_terms lhs rhs))
;;

let parse_inequality =
 Parser.lex (Parser.string "!=") <?> "inequality '!='" >>
 parse_term <?> "right-hand side" >>= fun rhs ->
 Parser.return (fun lhs -> NotEqual(Equation.of_terms lhs rhs))
;;

let parse_clause_status =
  (Parser.lex (Parser.string "axiom") >> Parser.return Axiom)
 <|> (Parser.lex (Parser.string "hypothesis") >>
  Parser.return Hypothesis)
 <|> (Parser.lex (Parser.string "conjecture") >>
  Parser.return (Conjecture Plain))
 <|> (Parser.lex (Parser.string "negated_conjecture") >>
  Parser.return (Conjecture Negated))
;;

let parse_term_pair =
 Parser.lex (Parser.char '(') >>
 parse_term <?> "left-hand side" >>= fun lhs ->
 (parse_inequality <|> parse_equality) >>= fun e ->
 Parser.lex (Parser.char ')') >>
 Parser.return (e lhs)
;;

let parse_name =
 Parser.many (Parser.noneof " \t\n\r()\",") >>= fun n ->
 Parser.return n
;;

let parse_clause =
 Parser.lex ((Parser.string "cnf") <|> (Parser.string "input_clause")) >>
 Parser.lex (Parser.char '(') >>
 parse_name >>
 Parser.lex (Parser.char ',') >>
 parse_clause_status >>= fun status ->
 Parser.lex (Parser.char ',') >>
 parse_term_pair >>= fun e ->
 Parser.lex (Parser.string ").") >>
 Parser.return (e, status)
;;

let parse_filename =
 Parser.many (Parser.noneof " \t\n\r()\",'") >>= fun n ->
 Parser.return n
;;

let parse_comment =
 Parser.lex (Parser.char '%') >>
 Parser.many (Parser.noneof "\n") >>= fun n ->
 Parser.lex (Parser.char '\n') >>
 Parser.return ()
;;

let parse_include =
 Parser.lex (Parser.string "include('") >>
 parse_filename >>= fun p ->
 Parser.lex (Parser.string "').") >>
 Parser.return p
;;

let to_string = List.fold_left (fun s c -> s ^ (Char.escaped c)) ""

let parse =
 let include_or_clause =
   ((parse_include >>= fun i -> Parser.return ([!axioms_prefix ^ (to_string i)], []))
   <|> (parse_clause >>= fun c -> Parser.return ([], [c]))
   <|> (parse_comment >> Parser.return ([], [])))
 in Parser.many include_or_clause >>= fun rs ->
 Parser.return (Pair.apply List.concat List.concat (List.split rs))
;;

let rec of_file s f =
  let m t = Parser.get_state >>= (Parser.return <.> Pair.make t) in
  let m = parse >>= m in
  match Parser.run m s (StringInput.of_file f) with
    | Left e -> Pervasives.failwith(f^". "^(Error.to_string e))
    | Right ((is, cs), s) -> let cs', s' =  of_files s is in cs' @ cs, s'
and of_files st  =
 List.fold_left 
  (fun (c, s) f -> let cs, s = of_file s f in cs @ c, s) ([], st) 
;;

let load f = 
  let i = Str.search_backward (Str.regexp (Filename.basename f)) f (String.length f - 1) in
  axioms_prefix := String.sub f 0 i;
(* Format.printf "axiom prefix: %s\n%!" !axioms_prefix;*)
 of_file (S.empty 100) f
;;

