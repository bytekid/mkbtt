(* Copyright 2010 Sarah Winkler
 * GNU Lesser General Public License
 *
 * This file is part of MKBtt.
 * 
 * MKBtt is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * MKBtt is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with MKBtt. If not, see <http://www.gnu.org/licenses/>.
 *)

(** Path strings for path indexing
@author Sarah Winkler
@since  2007/04/30 *)

(** Implements path strings as used in path indexing. Following the 
    notation in [1], these are p-strings in a simplified form.
    For example, $f.2.g.1.f.1.*$ denotes the variable position in the term
    $f(b,g(f(x),a))$. *)


(*** EXCEPTIONS **********************************************************)
exception Transformation_error;;
(* Transformation_error: indicates failure in pattern matching *)

(*** SUBMODULES **********************************************************)
module Fun = Rewriting.Function;;
module T = U.Term;;
module Position = Rewriting.Position;;
module Var = Rewriting.Variable;;

(*** TYPES ***************************************************************)

type t =
 | Var (* Corresponds to * in [1] *)
 | Const of Fun.t (* for constants *)
 | Fct of Fun.t*int*t (* Trslib.Position not useful *)
;;

(*** GLOBALS *************************************************************)

(*** FUNCTIONS ***********************************************************)

let attach f n pstring =
 Fct (f, n, pstring)
;;

let attach_to_all f ps=
let distr = fun (n, res) s -> (n+1, (List.map (attach f n) s)@res) in
 let _ , plist =
  List.fold_left distr (1, []) ps
 in
 plist
;;

let rec get = function
  | T.Var _ -> [Var]
  | T.Fun (f, []) -> [Const f]
  | T.Fun (f, tlist) -> 
   let plist = 
    List.map get tlist   
   in
   attach_to_all f plist
;;

let rec to_string' s res = 
 match s with
  | Var -> 
   res ^ "*"
  | Const c -> 
   res ^ (Fun.to_string c)  
  | Fct(f, p, s') -> 
   let res' = (res ^ (Fun.to_string f) ^ "." ^ (string_of_int p) ^ ".") in
   to_string' s' res'
;;

let to_string s = to_string' s ""

(*
let test () =
 Format.printf "testing module PathString\n";
 let c = Fun.of_string "c" 0 in
 let f = Fun.of_string "f" 1 in
 let g = Fun.of_string "g" 2 in
 let x = Term.Var (Var.of_string "x") in
 let y = Term.Var (Var.of_string "y") in
 let f_x = Term.Fun (f, [x]) in
 let f_f_x = Term.Fun (f, [f_x]) in
 let c_ = Term.Fun (c, []) in
 let g_f_f_x_c = Term.Fun (g, [f_f_x; c_]) in
 let u = Term.Fun(g,[g_f_f_x_c; c_]) in
 let pstr = get g_f_f_x_c in
 let pstr_u = get u in
(*** Some asssertions ****************************************************)
 Setx.iter (fun s -> Format.printf "%s\n" (to_string s)) pstr_u;
 assert ((Setx.is_singleton (get x)) && (Setx.mem Var (get x)) );
 assert (Setx.equal (get y) (get x)); (* non-linearity *)
 assert (Setx.mem (Fct (f, 1, Var)) (get f_x));
 assert (Setx.cardinal pstr = 2);
 assert (Setx.cardinal pstr_u = 3)
;;*)

