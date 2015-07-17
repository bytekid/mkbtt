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
module type MONOMIAL = sig
 type t
 type elt
 type var
 val add : t -> t -> t
 val coefficient : t -> elt
 val compare : t -> t -> int
 val is_constant : t -> bool
 val make : elt -> var list -> t
 val mul : t -> t -> t
 val normal_form : t -> t
 val scale : elt -> t -> t
 val split : t -> (elt * var list)
 val fprintf : Format.formatter -> t -> unit
 val to_string : t -> string
 val variables : t -> var list
end

module type POLYNOMIAL = sig
 type t
 type elt
 type var
 type mono
 val add : t -> t -> t
 val coefficients : t -> elt list
 val constant_part : t -> mono
 val filter : (mono -> bool) -> t -> t
 val make : mono list -> t
 val map : (mono -> 'a) -> t -> 'a list
 val map2 : (mono -> mono -> 'a) -> t -> t -> 'a list
 val mem : mono -> t -> bool
 val mul : t -> t -> t
 val non_constant_part : t -> t
 val normal_form : t -> t
 val scale : elt -> t -> t
 val subst: (var -> t) -> t -> t
 val to_string : t -> string
 val fprintf : Format.formatter -> t -> unit
 val split : t -> (elt * var list) list
 val variables : t -> var list list
end

module Make (M : MONOMIAL) : (POLYNOMIAL with type elt = M.elt 
 and type var = M.var and type mono = M.t) = struct
 (*** TYPES *******************************************************************)
 type var = M.var;;
 type elt = M.elt;;
 type mono = M.t;;
 type t = mono list;;

 (*** FUNCTIONS ***************************************************************)
 let normal_form p = List.sort M.compare p;;

 let fprintf ppf p = List.fprintf M.fprintf " + " ppf p;;
 let to_string p = Format.fprintf Format.str_formatter "@[%a@]" fprintf p;
 Format.flush_str_formatter ();;

 (* tail recursive *)
 let insert m ps = 
  let rec insert m acc = function
   | [] -> m :: acc
   | p :: ps ->
    if M.variables m = M.variables p then
     (M.add p m) :: List.rev_append acc ps
    else insert m (p :: acc) ps
  in
  normal_form (insert m [] ps)
 ;;

 let mem m p = List.mem m p;;

 (* tail recursive *)
 let add p0 p1 = 
  normal_form (List.fold_left (fun p m -> insert m p) p0 p1)
 ;;


 let make ms = 
  normal_form (List.fold_left (fun p m -> add [m] p) [] ms)
 ;;

 (* tail recursive *)
 let product f ls0 ls1 = 
  List.rev_map (fun x -> List.rev_map (f x) ls1) ls0
 ;;

 let mul p0 p1 =
  normal_form
   (List.fold_left add [] ((product M.mul p0 p1)))
 ;;

 let scale c p = List.map (M.scale c) p;;

 let constant_part p = 
  match List.filter M.is_constant p with
   | [] -> raise Not_found
   | [cp] -> cp
   | _ -> raise (Match_failure ("Polynomial.constant_part", 0, 0))
 ;;

 let non_constant_part p = 
  List.filter (fun m -> not (M.is_constant m)) p
 ;;

 let split = List.map M.split;;
 let coefficients = List.map M.coefficient;;
 let variables = List.map M.variables;;
(*
 let variables p =
  List.fold_left
   (fun s m -> List.union [M.variables m] s)
   []
   p
 ;;
*)

 let filter f p = List.filter f p;;

 let map f p = List.map f p;;

 let map2 f p0 p1 = List.map2 f p0 p1;;

 let subst_mono sigma m = match M.variables m with
  | [] -> make [m]
  | xs -> scale (M.coefficient m) (List.foldl1 mul (List.map sigma xs))
 ;;

 let subst sigma p = List.foldl1 add (List.map (subst_mono sigma) p)

end
