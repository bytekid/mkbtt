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

(*** TYPES ***************************************************************)
type termpairproperty =
 | MaxSize
 | SumSize

type eqsproperty =
   ESum of termpairproperty
 | Cardinal

type trsproperty =
   TSum of termpairproperty
 | CPs of eqsproperty
 | Size

type processproperty =
 | PPlus of processproperty * processproperty
 | EProjection of eqsproperty
 | RProjection of trsproperty
 | CProjection of trsproperty

type processsetproperty =
 | PMin of processproperty
 | PSum of processproperty
 | Count

type nodeproperty =
 | Age
 | DataProperty of termpairproperty
 | Plus of nodeproperty * nodeproperty
 | Negative of nodeproperty
 | ELabel of processsetproperty (* only open *)

type t =
   Property of nodeproperty * t
 | Probability of float * t * t
 | Random

type macro =
   Old
 | JapanSum
 | JapanMax

(*** VALUES ******************************************************************)

val japan_max : t
 (** A selection strategy choosing a process with 'small' sets of constraints
 rules and equations, and after that a node with 'small' data and a large
 number of processes in the equation label. Here, 'small' means that the 
 maximum of the term sizes in an equation/rule is minimal. *)

val japan_sum : t
 (** A selection strategy choosing a process with 'small' sets of constraints
 rules and equations, and after that a node with 'small' data and a large
 number of processes in the equation label. Here, 'small' means that the 
 sum of the term sizes in an equation/rule is minimal. *)

val slothrop : t
 (** Strategy corresponding to Slothrop's equation selection: first a 
 process is chosen for which the sum of the number of equations, constraint 
 rules and critical pairs among rewrite rules is minimal. Then a node is 
 selected with minimal data size. *)

val to_string : t -> string
 (** [to_string s] returns a string representation of the selection 
 strategy [s] as understood by the parser. *)
