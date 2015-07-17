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

(*** TYPES ********************************************************************)
type nat = Formula.p list      (*bitlist, little endian, e.g., [0;0;1] = 4*)
type bint = (Formula.p * nat)  (*signed int, binnat*)
type rat = (bint * Integer.t)        (*bint, denumerator*)
type real = (rat * rat)        (*rat, sqrt(2)*rat*)
type minf = (real * Formula.p) (*real, minf*)

(*type t = nat;; *)
(*type t = int;; *)
(*type t = rat;; *)
(*type t = real *)
type t = minf
