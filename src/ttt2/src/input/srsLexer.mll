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
open Util;;
open SrsParser;;

(*** FUNCTIONS ****************************************************************)
let create xs =
 let m = Hashtbl.create (List.length xs) in
 List.iter (fun (k,t) -> Hashtbl.add m k t) xs;
 m
;;

(*** GLOBALS ******************************************************************)
let keywords =
 let keywords = [
  ("LEFTMOST",LEFTMOST);
  ("RIGHTMOST",RIGHTMOST);
  ("RULES",RULES);
  ("STRATEGY",STRATEGY)]
 in
 create keywords
;;

let delimiters =
 let delimiters = [
  (',',COMMA);
  ('(',LEFT_PAREN);
  (')',RIGHT_PAREN)]
 in
 create delimiters
;;

let specials =
 let specials = [
  ("->",ARROW);
  ("->=",ARROW_EQUAL)]
 in
 create specials
;;
}

(*** DECLARATIONS *************************************************************)
(* identifiers *)
let id = [^' ''\t''\n''\r''('')''"'',']+

(* strings *)
let string = '"'[^'"']*'"'

(* delimiters *)
let delimiter = ['('')'',']

(* special sequences *)
let special = "->" | "->="

(*** RULES ********************************************************************)
rule token = parse
 | delimiter as d {Hashtbl.find delimiters d}
 | special as s {Hashtbl.find specials s}
 | id as s {try Hashtbl.find keywords s with Not_found -> ID s}
 | string as s { STRING s }
 | _ {token lexbuf}
 | eof {EOF}
