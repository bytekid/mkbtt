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
module Fun = Prelude.Function;;
module M = Monad;;
module Var = Prelude.Variable;;

(*** OPENS ********************************************************************)
open Util;;

(*** MODULES ******************************************************************)
module Filter = struct
 (*** TYPES *******************************************************************)
 type t = Collapsing of int | List of int list;;

 (*** FUNCTIONS ***************************************************************)
 (* Predicates *)
 let is_collapsing = function Collapsing _ -> true | _ -> false;;
 let is_list = function List _ -> true | _ -> false;;

 (* Compare Functions *)
 let compare f g = match (f,g) with
  | Collapsing p, Collapsing q -> compare p q
  | Collapsing _, _ -> ~-1
  | _, Collapsing _ -> 1
  | List ps, List qs -> compare (List.sort compare ps) (List.sort compare qs)
 ;;

 (* Printers *)
 let fprintf fmt = function
  | Collapsing p -> F.fprintf fmt "@[%i@]" p
  | List ps ->
   let fprintf = List.fprintf (flip F.fprintf "%i") "," in
   F.fprintf fmt "@[[%a]@]" fprintf ps
 ;;

 let fprintfx fmt = function
  | Collapsing p -> F.fprintf fmt "@{<collapsing>%i@}" (p+1)
  | List ps      -> F.fprintf fmt "@{<nonCollapsing>%a@}"
    (List.fprintf (fun fmt p -> F.fprintf fmt "@{<position>%i@}" (p+1)) "") ps
 ;;
end

module Map = Map.Make (Fun) (Filter);;

(*** TYPES ********************************************************************)
type term = Term.t = Var of Var.t | Fun of Fun.t * term list;;
type filter = Filter.t = Collapsing of int | List of int list;;
type t = Map.t;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Constructors *)
let empty = Map.empty;;
let add = Map.add;;
let of_list = List.foldl (flip (uncurry add)) Map.empty;;
let to_list af = Map.fold (fun f -> cons <.> pair f) af [];;
let singleton = Map.singleton;;

(* Search Functions *)
let find = Map.find;;

(* Apply Functions *)
let rec apply_term af = function
 | Var _ as x -> x
 | Fun (f,ts) ->
  try match Map.find f af with
   | Collapsing i -> apply_term af (List.nth ts i)
   | List ps ->
    let add i ts ti = if List.mem i ps then apply_term af ti :: ts else ts in
    Fun (f,List.rev (List.foldli add [] ts))
  with Not_found -> failwith "incomplete filtering"
;;

let apply_rule = Rule.project <.> apply_term;;
let apply_trs = Trs.project <.> apply_rule;;

(* Predicates *)
let is_collapsing = Map.for_all (const Filter.is_collapsing);;

(* Printers *)
let fprintf fmt af =
 let fprintf fmt =
  Map.iteri (fun i f fi ->
   if i > 0 then F.fprintf fmt "@\n";
   F.fprintf fmt "@[pi(%a)@ =@ %a@]" Fun.fprintf f Filter.fprintf fi)
 in
 F.fprintf fmt "@[%a@]" fprintf af
;;

let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;

let fprintfm fmt af =
 let fprintfm fmt af =
  Map.foldi (fun i f fi m ->
   m >>= fun _ ->
   if i > 0 then F.fprintf fmt "@\n@[pi(" else F.fprintf fmt "@[pi(";
   M.fprintf_fun fmt f >>= fun _ ->
   M.return (F.fprintf fmt ")@ =@ %a@]" Filter.fprintf fi)) af (M.return ())
 in
 F.fprintf fmt "@["; fprintfm fmt af >>= fun _ -> M.return (F.fprintf fmt "@]")
;;

let to_stringm af =
 fprintfm F.str_formatter af >>= (M.return <.> F.flush_str_formatter)
;;

let fprintfx fmt af =
 let fprintfx fmt af =
  Map.fold (fun f fi m ->
   m >>= fun _ ->
   let tag = if Filter.is_list fi then "argumentFilterEntry" else "projEntry" in
   F.fprintf fmt "@{<%s>" tag;
   M.fprintfx_fun fmt f >>= fun _ ->
   M.find_ari f         >>= fun i ->
   F.fprintf fmt "@{<arity>%i@}" i;
   M.return (F.fprintf fmt "%a@}" Filter.fprintfx fi)) af (M.return ())
 in
 let tag =
  if is_collapsing af then "simpleProjection" else "argumentFilter"
 in
 F.fprintf fmt "@{<%s>" tag; fprintfx fmt af >>= fun _ ->
 M.return (F.fprintf fmt "@}")
;;

let to_stringx af =
 fprintfx F.str_formatter af >>= (M.return <.> F.flush_str_formatter)
;;
