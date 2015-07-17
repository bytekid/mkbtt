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

(*** HEADER *******************************************************************)
{
(*** OPENS ********************************************************************)
open Parser;;
open Util;;

(*** FUNCTIONS ****************************************************************)
let create xs =
 let m = Hashtbl.create (List.length xs) in
 List.iter (fun (k,t) -> Hashtbl.add m k t) xs;
 m
;;

(*** GLOBALS ******************************************************************)
let keywords =
 let keywords = [
  ("else",ELSE);
  ("if",IF);
  ("then",THEN)]
 in
 create keywords
;;

let delimiters =
 let delimiters = [
  ('&',AND);
  (':',COLON);
  (',',COMMA);
  ('!',EXCLAMATION_MARK);
  ('>',GREATER);
  ('{',LEFT_BRACE);
  ('[',LEFT_BRACKET);
  ('(',LEFT_PAREN);
  ('%',PERCENT);
  ('|',PIPE);
  ('+',PLUS);
  ('?',QUESTION_MARK);
  ('}',RIGHT_BRACE);
  (']',RIGHT_BRACKET);
  (')',RIGHT_PAREN);
  (';',SEMICOLON);
  ('<',SMALLER);
  ('*',STAR)]
 in
 create delimiters
;;
}

(*** DECLARATIONS *************************************************************)
(* letter *)
let letter = ['_''a'-'z''A'-'Z']

(* digit *)
let digit = ['0'-'9']

(* identifier *)
let id = (letter | digit | ['-''^''\'''.'])+

(* number *)
let number = (digit+ | (digit* ['.'] digit+) | (digit+ ['.'] digit*))

(* delimiters *)
let delimiter = ['('')''['']''{''}''<''>'','':'';''|''&''+''*''!''?''%']

(*** RULES ********************************************************************)
rule token = parse
 | delimiter as d {Hashtbl.find delimiters d}
 | number as n
  {try INT (int_of_string n) with Failure _ -> FLOAT (float_of_string n)}
 | id as s {try Hashtbl.find keywords s with Not_found -> ID s}
 | _ {token lexbuf}
 | eof { EOF }
