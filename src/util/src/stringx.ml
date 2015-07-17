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
open Prelude;;

(*** INCLUDES *****************************************************************)
include String;;

(*** MODULE TYPES *************************************************************)
module type STRING = sig
 val blit : string -> int -> string -> int -> int -> unit
 val capitalize : string -> string
 val compare: string -> string -> int
 val concat : string -> string list -> string
 val contains : string -> char -> bool
 val contains_from : string -> int -> char -> bool
 val copy : string -> string
 val create : int -> string
 val escaped : string -> string
 val fill : string -> int -> int -> char -> unit
 val get : string -> int -> char
 val index : string -> char -> int
 val index_from : string -> int -> char -> int
 val iter : (char -> unit) -> string -> unit
 val length : string -> int
 val lowercase : string -> string
 val make : int -> char -> string
 val rcontains_from : string -> int -> char -> bool
 val rindex : string -> char -> int
 val rindex_from : string -> int -> char -> int
 val set : string -> int -> char -> unit
 val sub : string -> int -> int -> string
 val uppercase : string -> string
 val uncapitalize : string -> string
end

(*** FUNCTIONS ****************************************************************)
(* Miscellaneous *)
let hash = Hashtbl.hash;;
let rec split ?(d = "[ \t\n\r]+") s = Str.split (Str.regexp d) s;;
let trim s = Listx.join id "" (split ~d:" " s);;

(* Search Functions *)
let last s =
 try get s (length s - 1) with Invalid_argument _ -> raise Not_found
;;

let char_pos c s =
 let rec char_pos acc i =
  try let p = String.index_from s i c in char_pos (p :: acc) (p + 1)
  with Not_found -> acc
 in
 Listx.rev (char_pos [] 0)
;;

(* Constructors *)
let alignx ?(n = 0) i j s =
 let rec align acc i j k = function
  | [] -> acc
  | w :: ws ->
   let l = length w and w = if ws = [] then w else w ^ " " in
   if k + l < i then align (acc ^ w) i j (k + l + 1) ws
   else align (acc ^ "\n" ^ (make n ' ') ^ w) j j l ws
 in
 align "" (i - n) (j - n) 0 (split s)
;;

let align ?(n = 0) i = alignx ~n:n i i;;

let itemizex ?(n = 0) j is =
 let rec itemize acc m k = function
  | [] -> acc
  | (i,s)::is ->
   let delemiter = if is = [] then "" else "\n" ^ (make n ' ') in
   let item = i ^ (make (k - length i) ' ') ^ align ~n:m j s in
   itemize (acc ^ item ^ delemiter) m k is
 in
 let j = Listx.foldl (fun j -> max j <.> length <.> fst) 0 is + 1 in
 itemize "" (n + j) j is
;;

let itemize ?(n = 0) ?(s = "-") j =
 itemizex ~n:n j <.> Listx.map (Pair.make s)
;;

let chop s =
 let l = length s in
 let rec prefix s limit i = match s.[i] with
  | ' ' | '\t' | '\n' | '\r' -> if i < limit then prefix s limit (i + 1) else ""
  | _ -> sub s i (l - i)
 in
 let rec suffix s i = match s.[i] with
  | ' ' | '\t' | '\n' | '\r' -> if i > 0 then suffix s (i - 1) else ""
  | _ -> sub s 0 (i + 1)
 in
 if l = 0 then "" else
  let s = prefix s (l - 1) 0 in let l = length s in
  if l = 0 then "" else suffix s (l - 1)
;;

let shift_left i str = str ^ (make i ' ');;
let shift_right i str = (make i ' ') ^ str;;

(* Properties *)
let is_prefix s t =
 try s = sub t 0 (length s) with Invalid_argument _ -> false
;;

(* Character Lists *)
let to_char_list s =
 let rec to_char_list acc i =
  if i < 0 then acc else to_char_list (get s i :: acc) (i - 1)
 in
 to_char_list [] (length s - 1)
;;

let to_rev_char_list s =
 let l = length s in
 let rec to_rev_char_list acc i =
  if i >= l then acc else to_rev_char_list (get s i :: acc) (i + 1)
 in
 to_rev_char_list [] 0
;;

let of_char_list cs =
 let buf = Buffer.create (Listx.length cs) in
 Listx.iter (Buffer.add_char buf) cs;
 Buffer.contents buf
;;

(* Compare Functions *)
let equal s t = compare s t = 0;;

(* Printers *)
let fprintf fmt s = Format.fprintf fmt "@[%s@]" s;;
let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;

(* XML *)
let xml_entities_encode id = 
 let id = Str.global_replace (Str.regexp_string "&") "&amp;" id in
 let id = Str.global_replace (Str.regexp_string ">") "&gt;" id in
 let id = Str.global_replace (Str.regexp_string "<") "&lt;" id in
 id
;;

let xml_entities_decode id = 
 let id = Str.global_replace (Str.regexp_string "&amp;") "&" id in
 let id = Str.global_replace (Str.regexp_string "&gt;") ">" id in
 let id = Str.global_replace (Str.regexp_string "&lt;") "<" id in
 id
;;

(* URL *)
let url_escape ?(ignore=["/"]) id = 
 let ps = [
 (*order does matter *)
  ("%", "%25"); 
  ("#", "%23"); 
  (" ", "%20"); 
  ("<", "%3C"); 
  (">", "%3E"); 
  ("{", "%7B"); 
  ("}", "%7D"); 
  ("|", "%7C"); 
  ("\\", "%5C");
  ("^", "%5E"); 
  ("~", "%7E"); 
  ("[", "%5B"); 
  ("]", "%5D"); 
  ("`", "%60"); 
  (";", "%3B"); 
  ("/", "%2F"); 
  ("?", "%3F"); 
  (":", "%3A"); 
  ("@", "%40"); 
  ("=", "%3D"); 
  ("&", "%26"); 
  ("$", "%24");
 ] in
 Listx.foldl (fun id (o,n) -> 
  if List.mem o ignore then id 
  else Str.global_replace (Str.regexp_string o) n id) 
 id ps
;;

