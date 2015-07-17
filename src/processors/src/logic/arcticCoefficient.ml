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
open Rewritingx;;
open Logic.Operators;;

(*** MODULES ******************************************************************)
module Number = Logic.Number;;

(*** FUNCTIONS ****************************************************************)
 type t = Logic.a;;
 let add = Logic.max;;
 let mul = (<+>);;
 let is_zero _ = failwith "is_zero";;
 let is_one _ = failwith "is_one";;
 let zero = Logic.constant (Number.get_minf ());;
 let one = Logic.zero;;
 let fprintf = Logic.fprintf_a;;
 let fprintfx ppf = failwith "ArcticCoefficient.fprintx: not supported";;
 let to_string a = 
  Format.fprintf Format.str_formatter "%a" fprintf a;
  Format.flush_str_formatter ();;
 let gt = (<>>);;
 let eq = (<=>);;
 let geq = (<>=>);;
 let of_int = Logic.Number.of_int;;
 let big_sum = List.fold_left (fun acc elt -> add acc elt) zero;;
