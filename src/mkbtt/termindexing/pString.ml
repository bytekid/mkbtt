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

(** p-Strings for discrimination tree indexing
@author Sarah Winkler
@since  2007/05/30 *)

(*** EXCEPTIONS **********************************************************)

exception Empty_pstring

(*** SUBMODULES **********************************************************)
module Fun = Rewriting.Function;;
module Var = Rewriting.Variable;;
module Term = U.Term;;

module Label = struct
 type t = Symbol of Fun.t | Star

 let compare = Pervasives.compare

 let to_string = function
 | Star -> "*"
 | Symbol f -> Fun.to_string f
;;
 
 let fprintf fmt = function
 | Star -> Format.fprintf fmt "@[*@]"
 | Symbol f -> Format.fprintf fmt "@[%s@]" (Fun.to_string f)
;;

end
(*** TYPES ***************************************************************)
(* for more efficient retrieval operations, tag each symbol in string
   with its after element *)
type label = Label.t

(* Element(f, next, after) *)
(* these are kind of lists with shortcuts *)
type t = Nil | Element of label * t * t

(*** GLOBALS *************************************************************)

(*** FUNCTIONS ***********************************************************)

let rec to_string = function
 | Nil -> "[]"
 | Element(e, p, _) -> (Label.to_string e) ^ "." ^ (to_string p)
;;

(* the empty pstring *)
let nil = []

(* create pstring from term *)
let rec of_term' next after = function
 | Term.Var _ -> Element(Label.Star, next, after) (* next=after *)
 | Term.Fun(f, ts) ->
  let next', _ = 
   List.fold_right 
    (fun ti (n, a) -> let this = of_term' n a ti in (this, this)) 
    ts 
    (next, after)
  in Element(Label.Symbol f, next', after)
;;

let time = ref 0.0

let of_term = of_term' Nil Nil 

(* operation to get after element *)
let after = function
 | Nil -> raise Empty_pstring
 | Element(_, _, a)  -> a
;;

let next = function
 | Nil -> raise Empty_pstring
 | Element(_, n, _) -> n
;;

(*** TESTS ***************************************************************)
(*
let test () =
 Format.printf "testing module PString\n";
 let one = Fun.of_string "one" 0 in
 let i = Fun.of_string "i" 1 in
 let m = Fun.of_string "m" 2 in
 let x = Term.Var (Var.of_string "x") in
 let y = Term.Var (Var.of_string "y") in
 let i_x = Term.Fun (i, [x]) in
 let one_ = Term.Fun (one, []) in
 let m_i_x_o = Term.Fun (m, [i_x; one_]) in
 let m_x_y = Term.Fun (m, [x; y]) in
 let m_m_x_y_m_i_x_o = Term.Fun(m, [m_x_y; m_i_x_o]) in
 Format.printf "m_i_x_o is %s\n" (to_string (of_term m_i_x_o));
 Format.printf "m_m_x_y_m_i_x_o is %s\n" (to_string (of_term m_m_x_y_m_i_x_o))
;;*)

(* test () *)
