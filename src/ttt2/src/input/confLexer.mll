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
open ConfParser;;

(*** FUNCTIONS ****************************************************************)
let create xs =
 let m = Hashtbl.create (List.length xs) in
 List.iter (fun (k,t) -> Hashtbl.add m k t) xs;
 m
;;

(*** GLOBALS ******************************************************************)
let delimiters =
 let delimiters = [('\\',BACKSLASH);('=',EQUAL);('\n',NEWLINE);('#',SHARP)] in
 create delimiters
;;
}

(*** DECLARATIONS *************************************************************)
(* identifiers *)
let id = [^' ''\t''\n''\r''=''\\''#']+

(* delimiters *)
let delimiter = ['=''\n''\\''#']

(*** RULES ********************************************************************)
rule token = parse
 | delimiter as d {Hashtbl.find delimiters d}
 | id as s {ID s}
 | _ {token lexbuf}
 | eof {EOF}
