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
open Util;;

(*** MODULES ******************************************************************)
module F = Format;;

(*** TYPES ********************************************************************)
type abbreviation = string * string;;
type t = abbreviation list;;

(*** FUNCTIONS ****************************************************************)
(* Constructors and Destructors *)
let add_abbreviation = List.cons;;
let set_abbreviations = const;;
let empty = [];;
let singleton = List.singleton;;

(* Access Functions *)
let get_abbreviations = id;;

(* Miscellaneous *)
let rec expand s t =
 let id = "[^0-9a-zA-Z_]" in
 let is_contained s t =
  let infix a = Str.regexp (F.sprintf ".*%s%s%s.*" id a id) in
  let postfix a = Str.regexp (F.sprintf ".*%s%s$" id a) in
  let prefix a = Str.regexp (F.sprintf "^%s%s.*" a id) in
  let single a = Str.regexp (F.sprintf "^%s$" a) in
  List.exists (fun (a,_) ->
   Str.string_match (infix a) s 0 || Str.string_match (postfix a) s 0 ||
   Str.string_match (prefix a) s 0 || Str.string_match (single a) s 0) t
 in
 let replace =
  let infix a = Str.regexp (F.sprintf "\\(%s\\)%s\\(%s\\)" id a id) in
  let postfix a = Str.regexp (F.sprintf "\\(%s\\)%s$" id a) in
  let prefix a = Str.regexp (F.sprintf "^%s\\(%s\\)" a id) in
  let single a = Str.regexp (F.sprintf "^%s$" a) in
  List.foldl (fun s (a,s') ->
   let s = Str.global_replace (infix a) ("\\1"^s'^"\\2") s in
   let s = Str.global_replace (postfix a) ("\\1"^s') s in
   let s = Str.global_replace (prefix a) (s'^"\\1") s in
   Str.global_replace (single a) s' s)
 in
 if is_contained s t then expand (replace s t) t else s
;;

(* Printers *)
let fprintf fmt t =
 List.fprintf (fun fmt (a,s) -> F.fprintf fmt "%s = %s" a s) "@\n" fmt t
;;

let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;
