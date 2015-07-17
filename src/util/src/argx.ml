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
include Arg;;

(*** MODULE TYPES *************************************************************)
module type ARG = sig
type anon_fun = string -> unit
 type doc = string
 type key = string
 type spec =
  | Unit of (unit -> unit)
  | Bool of (bool -> unit)
  | Set of bool ref
  | Clear of bool ref
  | String of (string -> unit)
  | Set_string of string ref
  | Int of (int -> unit)
  | Set_int of int ref
  | Float of (float -> unit)
  | Set_float of float ref
  | Tuple of spec list
  | Symbol of string list * (string -> unit)
  | Rest of (string -> unit)
 type usage_msg = string
 
 exception Bad of string
 exception Help of string
 
 val align: (key * spec * doc) list -> (key * spec * doc) list
 val current : int ref
 val parse : (key * spec * doc) list -> anon_fun -> usage_msg -> unit
 val parse_argv : ?current: int ref -> string array -> (key * spec * doc) list
  -> anon_fun -> usage_msg -> unit
 val usage : (key * spec * doc) list -> usage_msg -> unit
end

(*** TYPES ********************************************************************)
type error =
 | Unknown of string
 | Wrong of string * string * string
 | Missing of string
;;

(*** EXCEPTIONS ***************************************************************)
exception Stop of error;;

(*** FUNCTIONS ****************************************************************)
let stop arg = function
 | Unknown s -> raise (Bad (Format.sprintf "%s: unknown option `%s'." arg s))
 | Missing s ->
  raise (Bad (Format.sprintf "%s: option `%s' needs an argument." arg s))
 | Wrong (o, a, e) ->
  raise (Bad (Format.sprintf
   "%s: wrong argument `%s'; option `%s' expects %s." arg a o e))
;;

let next a f = function
 | [] -> raise (Stop (Missing a))
 | arg :: args -> 
  try (f arg, args) 
  with
   | Failure "int_of_string" -> raise (Stop (Wrong (a,arg,"an integer")))
   | Failure "float_of_string" -> raise (Stop (Wrong (a,arg,"a float")))
   | Failure "bool_of_string" -> raise (Stop (Wrong (a,arg,"a boolean")))
;;

let rec parsex arg spec = function
 | [] -> ()
 | a :: args ->
  let action =
   try Triple.snd (Listx.find ((=) a <.> Triple.fst) spec)
   with Not_found -> stop arg (Unknown a)
  in
  let process_action a args = function
   | Unit f -> f (); args
   | Bool f -> let (b,rest) = next a bool_of_string args in f b; rest
   | Set b -> b := true; args
   | Clear b -> b := false; args
   | String f -> let (s,rest) = next a id args in f s; rest
   | Set_string s -> let (n,rest) = next a id args in s := n; rest
   | Int f -> let (n,rest) = next a int_of_string args in f n; rest
   | Set_int i -> let (n,rest) = next a int_of_string args in i := n; rest
   | Float f -> let (n,rest) = next a float_of_string args in f n; rest
   | Set_float f -> let (n,rest) = next a float_of_string args in f := n; rest
   | _ -> failwith "not supported"
  in
  let rest = try process_action a args action with Stop e -> stop arg e in
  parsex arg spec rest
;; 

let alignx w l =
 let max m (k,_,_) = let k = Stringx.length k in if k > m then k else m in
 let max_len = Listx.foldl max 0 l in
 Listx.map (fun (k,s,d) ->
  let msg = Stringx.align (w - (max_len + 3)) d in
  let lines = Stringx.split ~d:"[\n]" msg in
  let hl = Listx.hd lines in
  let hl = Stringx.shift_right (max_len - (Stringx.length k)) hl in
  let tl = Listx.tl lines in
  let tl = Listx.map (Stringx.shift_right (max_len + 3)) tl in
  let d = Listx.join id "\n" tl in
  (k, s, hl ^ if d = "" then "" else "\n" ^ d)) l
;;

let make_symlist prefix sep suffix l = match l with
 | [] -> "<none>"
 | h::t -> (Listx.foldl (fun x y -> x ^ sep ^ y) (prefix ^ h) t) ^ suffix
;;

let print_spec buf (key, spec, doc) = match spec with
 | Symbol (l,_) ->
  Printf.bprintf buf "  %s %s%s\n%!" key (make_symlist "{" "|" "}" l) doc
 | _ -> Printf.bprintf buf "  %s %s\n%!" key doc
;;

let usage_bx buf speclist errmsg =
 Printf.bprintf buf "%s\n%!" errmsg; Listx.iter (print_spec buf) speclist;
;;

let usagex speclist errmsg =
 let b = Buffer.create 200 in
 usage_bx b speclist errmsg; Printf.eprintf "%s%!" (Buffer.contents b)
;;
