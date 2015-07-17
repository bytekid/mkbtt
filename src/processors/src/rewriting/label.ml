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
open Rewriting;;

(*** MODULES ******************************************************************)
module Fun = Function;;
module F = Index.Isomorphic (Function) (String);;
module V = Index.Isomorphic (Variable) (String);;

(*** TYPES ********************************************************************)
type signature = F.t * V.t;;

type theory = A | C | AC

type t =
 (* currying *)
 | Curry of int
 (* dependency pairs *)
 | Dp
 (* match-bounds *)
 | Height of int
 (* plain integer label *)
 | Int of int list
 (* root labeling *)
 | Rlab of Fun.t list (* stored in reversed order *)
 (* quasi root labeling *)
 | Qlab of int list (* stored in reversed order *)
 (* semantic labeling *)
 | Slab of int
 (* associative/commutative symbols *)
 | Theory of theory
;;

(*** FUNCTIONS ****************************************************************)
(* Miscellaneous *)
let copy = id;;
let hash = Hashtbl.hash;;

(* Constructors and Destructors *)
let make_curry n = Curry n;;
let make_dp = Dp;;
let make_height n = Height n;;
let make_int i = Int [i];;
let make_int_list is = Int is;;
let make_rlab fs = Rlab fs;;
let make_qlab ns = Qlab ns;;
let make_slab n = Slab n;;
let make_theory n = Theory n;;


let add_rlab f = function
 | Rlab fs -> Rlab (f :: fs)
 | _ -> failwith "unknown label"
;;

let add_int i = function
 | Int is -> Int (i :: is)
 | _ -> Int [i]
;;

let add_qlab n = function | Qlab ns -> Qlab(n::ns) | _ -> failwith "unknown label"
;;

let incr_curry = function
 | Curry n -> Curry (n + 1)
 | _ -> failwith "unknown label"
;;

(* Search Functions *)
let get_curry = function
 | Curry n -> n
 | _ -> failwith "unknown label"
;;

let get_height = function
 | Height n -> n
 | _ -> failwith "unknown label"
;;

let get_int = function
 | Int (i::is) -> i
 | Int ([]) -> failwith "no int label"
 | _ -> failwith "unknown label"
;;

let get_int_list = function
 | Int is -> is
 | _ -> []
;;

let drop_int = function
 | Int (i::is) -> Int is
 | Int [] -> failwith "no int label"
 | _ -> failwith "unknown label"
;;

let get_rlab = function
 | Rlab fs -> fs
 | _ -> failwith "unknown label"
;;

let get_qlab = function
 | Qlab ns -> ns
 | _ -> failwith "unknown label"
;;

let get_slab = function
 | Slab n -> n
 | _ -> failwith "unknown label"
;;

let get_theory = function
 | Theory t -> t
 | _ -> failwith "unknown label"
;;


(* Properties *)
let is_curry = function Curry _ -> true | _ -> false;;
let is_dp = function Dp -> true | _ -> false;;
let is_height = function Height _ -> true | _ -> false;;
let is_int = function Int _ -> true | _ -> false;;
let is_rlab = function Rlab _ -> true | _ -> false;;
let is_qlab = function Qlab _ -> true | _ -> false;;
let is_slab = function Slab _ -> true | _ -> false;;
let is_theory tl = function Theory tl' -> tl=tl' | _ -> false;;
let is_some_theory = function Theory _ -> true | _ -> false;;

(* Compare Functions *)
let compare = compare;;
let equal l l' = compare l l' = 0;;


(* Printers *)
let fprintf_list fmt fpf =
 Format.fprintf fmt "@[(%a)@]" (List.fprintf fpf ",");;

let fprintf fmt = function
 | Curry a -> Format.fprintf fmt "@[%i@]" a
 | Dp -> Format.fprintf fmt "@[#@]"
 | Height h -> Format.fprintf fmt "@[%i@]" h
 | Int is -> fprintf_list fmt Int.fprintf (List.rev is)
 | Rlab fs -> fprintf_list fmt Fun.fprintf (List.rev fs)
 | Qlab ns -> fprintf_list fmt Int.fprintf (List.rev ns)
 | Slab id -> Format.fprintf fmt "@[%i@]" id
 | Theory t -> Format.fprintf fmt "@[%s@]" 
  (match t with A -> "A" | C -> "C" | AC -> "AC")
;;

let fprintfs f g fmt l s = match l with
 | Curry a -> Format.fprintf fmt "@[%i@]" a; s
 | Dp -> Format.fprintf fmt "@[#@]"; s
 | Height h -> Format.fprintf fmt "@[%i@]" h; s
 | Int is -> 
  Format.fprintf fmt "@[(%a)@]"
  (List.fprintf Int.fprintf ",") (List.rev is); s
 | Rlab fs ->
  Format.fprintf fmt "@[(";
  let fprintfs i = if i > 0 then Format.fprintf fmt ","; flip (f fmt) in
  let s = List.foldli fprintfs s (List.rev fs) in
  Format.fprintf fmt ")@]"; s
 | Qlab ns ->
  Format.fprintf fmt "@[(%a)@]" (List.fprintf Int.fprintf ",") (List.rev ns);
  s
 | Slab id -> Format.fprintf fmt "@[%i@]" id; s
 | Theory t -> Format.fprintf fmt "@[%s@]" 
  (match t with A -> "A" | C -> "C" | AC -> "AC") ; s
;;

let fprintfx f g fmt l s = match l with
 | Curry a  -> Format.fprintf fmt "@{<numberLabel>@{<number>%i@}@}" a; s
 | Dp       -> Format.fprintf fmt "<sharp/>"; s
 | Height h -> Format.fprintf fmt "@{<numberLabel>@{<number>%i@}@}" h; s
 | Int is ->
   Format.fprintf fmt "@{<numberLabel>%a@}"
    (List.fprintf Int.fprintfx ",") (List.rev is); s
 | Rlab fs  ->
   Format.fprintf fmt "@{<symbolLabel>";
   let s = List.foldl (flip (f fmt)) s (List.rev fs) in
   Format.fprintf fmt "@}"; s
 | Qlab ns  ->
   Format.fprintf fmt "@{<numberLabel>";
   Format.fprintf fmt "@[(%a)@]" (List.fprintf Int.fprintf ",") (List.rev ns);
   Format.fprintf fmt "@}"; s
 | Slab id  -> Format.fprintf fmt "@{<numberLabel>@{<number>%i@}@}" id; s
 | Theory t -> Format.fprintf fmt "@{<numberLabel>@{<number>%s@}@}" 
  (match t with A -> "A" | C -> "C" | AC -> "AC"); s
;;

let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;
