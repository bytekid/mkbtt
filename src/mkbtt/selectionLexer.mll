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

{
 (* lexing selection strategies *)
 open SelectionParser
}
rule token = parse
  | "smax" {SIZE_MAX}
  | "ssum" {SIZE_SUM}
  | "sum" {SUM}
  | "min" {MIN}
  | "cp" {CP}
  | "data" {DATA}
  | "el" {ELABEL}
  | ['0'-'1']'.'['0' - '9']* {FLOAT(Lexing.lexeme lexbuf)}
  | '?' {RANDOM}
  | '#' {COUNT}
  | '+' {PLUS}
  | '-' {MINUS}
  | '*' {TIMESTAMP}
  | 'e' {E}
  | 'r' {R}
  | 'c' {C}
  | '(' {LPAREN}
  | ')' {RPAREN}
  | ',' {COMMA}
  | ':' {COLON}
  | [' '] {token lexbuf}
  | eof   {EOF}
  | _ {token lexbuf}
  

