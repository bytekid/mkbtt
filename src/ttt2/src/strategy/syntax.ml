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

(*** TYPES ********************************************************************)
type condition =
 | Atom of string * string list
 | Not of condition
 | And of condition * condition
 | Or of condition * condition
;;

type modifier = string * string list;;

type t =
 | Strategy of string * string list
 (* combinators *)
 | Combine of t * t
 | Choose of t * t
 | Parallel of t * t
 | Condition of condition * t * t
 (* iterators *)
 | Optional of t
 | Iterate of t
 | Repeat of t
 | Replicate of int * t
 | Iterate_timed of float * t
 (* specifiers *)
 | Modify of t * modifier
 | Stop of t
 | Strict of t
 | Timed of float * t
;;

(*** FUNCTIONS ****************************************************************)
let print = List.fprintf (fun fmt -> Format.fprintf fmt "%s") "@ ";;

(* Printers *)
let rec cfprintf fmt = function
 | Atom (n,fs) -> Format.fprintf fmt "@[%s@ %a@]" n print fs
 | Not c -> Format.fprintf fmt "@[!(%a)@]" cfprintf c
 | And (c,c') -> Format.fprintf fmt "@[(%a)@ &@ (%a)@]" cfprintf c cfprintf c'
 | Or (c,c') -> Format.fprintf fmt "@[(%a)@ |@ (%a)@]" cfprintf c cfprintf c'
;;

let mfprintf fmt (m,fs) = Format.fprintf fmt "@[%s@ %a@]" m print fs;;

let rec fprintf fmt = function
 | Strategy (n,fs) -> Format.fprintf fmt "@[%s@ %a@]" n print fs
 (* combinators *)
 | Combine (s,s') -> Format.fprintf fmt "@[(%a;%a)@]" fprintf s fprintf s'
 | Choose (s,s') -> Format.fprintf fmt "@[(%a | %a)@]" fprintf s fprintf s'
 | Parallel (s,s') -> Format.fprintf fmt "@[(%a || %a)@]" fprintf s fprintf s'
 | Condition (c,s,s') ->
  Format.fprintf fmt "@[(if %a then %a else %a)@]" cfprintf c fprintf s fprintf s'
 (* iterators *)
 | Optional s -> Format.fprintf fmt "@[(%a?)@]" fprintf s
 | Iterate s -> Format.fprintf fmt "@[(%a*)@]" fprintf s
 | Repeat s -> Format.fprintf fmt "@[(%a+)@]" fprintf s
 | Replicate (n,s) -> Format.fprintf fmt "@[(%a%d*)@]" fprintf s n
 | Iterate_timed (t,s) -> Format.fprintf fmt "@[(%a[%.3f]*)@]" fprintf s t
 (* specifiers *)
 | Modify (s,m) -> Format.fprintf fmt "@[({%a}%a)@]" fprintf s mfprintf m
 | Stop s -> Format.fprintf fmt "@[(%a%%)@]" fprintf s
 | Strict s -> Format.fprintf fmt "@[(%a!)@]" fprintf s
 | Timed (t,s) -> Format.fprintf fmt "@[(%a[%.3f])@]" fprintf s t
;;

let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;
