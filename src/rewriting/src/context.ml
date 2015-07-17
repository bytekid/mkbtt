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

(*** MODULES ******************************************************************)
module F = Format;;
module Fun = Function;;
module M = Monad;;
module Pos = Position;;
module Var = Variable;;

(*** OPENS ********************************************************************)
open Prelude;;
open Util;;

(*** MODULES ******************************************************************)
module Make (L : LABEL) = struct
 (*** MODULES *****************************************************************)
 module S = Signature.Make (L);;
 module M = M.Make (L);;
 module Term = Term.Make (L);;
 
 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type term = Term.t = Var of Var.t | Fun of Fun.t * term list;;
 type t = Hole | More of Fun.t * term list * t * term list;;
 
 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;
 let (>>) = M.(>>);;
 
 (* Miscellaneous *)
 let hash = Hashtbl.hash;;
 
 let rec hole_pos = function
  | Hole -> Pos.root
  | More (_,us,c,_) -> Pos.add_first (List.length us) (hole_pos c)
 ;;
 
 let rec pos = function
  | Hole -> [Pos.root]
  | More (f,us,c,vs) ->
   let addi i = List.map (Pos.add_first i) in
   let f i ps ti = List.rev_append (addi i (Term.pos ti)) ps in
   let term_pos l = List.foldli (fun i -> f (i + l)) in
   let l = List.length us in
   term_pos 0 (term_pos (l + 1) (addi l (pos c)) vs) us
 ;;
 
 (* Constructors and Destructors *)
 let rec apply t = function
  | Hole -> t
  | More (f,us,c,vs) -> Fun (f,us @ apply t c :: vs)
 ;;
 
 let rec compose c = function
  | Hole -> c
  | More (f,us,d,vs) -> More (f,us,compose c d,vs)
 ;;
 
 let rec of_term p t = 
  if Pos.is_root p then Hole else (
   let (i,q) = Pos.split_first p in
   match t with
    | Var _ -> failwith "illegal position"
    | Fun (f,ts) ->
     try
      if List.length ts <= i then failwith "illegal position";
      let (us,vs) = List.split_at i ts in
      More (f,us,of_term q (List.hd vs),List.tl vs)
     with Failure "tl" -> failwith "illegal position")
 ;;
 
 let rec subcontext p c =
  if Pos.is_root p then c else (
   let (i,q) = Pos.split_first p in
   match c with
    | Hole -> failwith "illegal position"; 
    | More (f,us,c,_) ->
     if i <> List.length us then failwith "illegal position"
     else subcontext q c)
 ;;
 
 (* Properties *)
 let is_empty = function Hole -> true | _ -> false;;
 
 (* Compare Functions *)
 let compare = compare;;
 let equal c d = compare c d = 0;;
 
 (* Printers *)
 let rec fprintf fmt = function
  | Hole -> F.fprintf fmt "@[[]@]"
  | More (f,us,c,vs) ->
   let lus = List.length us and lvs = List.length vs in
   let fs =
    if lus = 0 && lvs = 0 then format_of_string "@[%a(%a%a%a)@]"
    else if lus = 0 then format_of_string "@[%a(%a%a,%a)@]"
    else if lvs = 0 then format_of_string "@[%a(%a,%a%a)@]"
    else format_of_string "@[%a(%a,%a,%a)@]"
   in
   F.fprintf fmt fs Fun.fprintf f (List.fprintf Term.fprintf ",") us
    fprintf c (List.fprintf Term.fprintf ",") vs
 ;;

 let rec fprintfm fmt = function
  | Hole -> M.return (F.fprintf fmt "@[[]@]")
  | More (f,us,c,vs) ->
   let lus = List.length us and lvs = List.length vs in
   let print_separator l = if l = 0 then () else F.fprintf fmt "," in
   F.fprintf fmt "@["; M.fprintf_fun fmt f >>= fun _ ->
   F.fprintf fmt "("; M.fprintf Term.fprintfm "," fmt us >>= fun _ ->
   print_separator lus; fprintfm fmt c >>= fun _ ->
   print_separator lvs; M.fprintf Term.fprintfm "," fmt vs >>= fun _ ->
   M.return (F.fprintf fmt ")@]")
 ;;

 let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;

 let to_stringm c =
  fprintfm F.str_formatter c >>= (M.return <.> F.flush_str_formatter)
 ;;
end
