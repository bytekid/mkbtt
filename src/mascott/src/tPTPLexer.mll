{
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

  (* tptp lexer*)
  open TPTPParser
}
rule token = parse 
  | "input_clause" {INPUT_CLAUSE} 
  | "cnf" {CNF}
  | "axiom" {AXIOM} 
  | "hypothesis" {HYPOTHESIS}
  | "conjecture" {CONJECTURE}
  | "negated_conjecture" {NEGCONJECTURE}
  | "equal" {EQUAL}
  | "!=" {DISEQUALSIGN} 
  | "include" {INCLUDE}
  | '=' {EQUALSIGN}
  | ['a' - 'z']['a'-'z' 'A' - 'Z' '0'-'9' '_']* {LSYM(Lexing.lexeme lexbuf)}
  | ['A' - 'Z']['a'-'z' 'A' - 'Z' '0'-'9' '_' '-' '.']* {USYM(Lexing.lexeme lexbuf)}
  | "++" {PLUSPLUS}
  | "--" {MINUSMINUS}
  | '(' {LPAREN}
  | ')' {RPAREN}
  | '[' {LBRACKET}
  | ']' {RBRACKET}
  | '.' {DOT}
  | ',' {COMMA}
  | '\'' {QUOTE}
  | '/' {SLASH}
  | '#' [^'\n']* "\n" {token lexbuf}  (* comment *)
  | '%' [^'\n']* "\n" {token lexbuf}  (* comment *)
  | [' ' '\t' '\n' '\r'] {token lexbuf}
  | eof   {EOF}
  | _ {token lexbuf}
