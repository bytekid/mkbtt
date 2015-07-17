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

(** Define strategy to select nodes
@author Sarah Winkler
@since  2009/12/16 *)


(*** TYPES ***************************************************************)
(* do not distinguish between equation and rule properties *)
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

(* finally, type of selection strategy *)
type t = 
   Property of nodeproperty * t
 | Probability of float * t * t 
 | Random

(* convenient to have shorthands for predefined strategies
   with optimzed implementation *) 
type macro =
   Old
 | JapanSum
 | JapanMax

(*** FUNCTIONS ***********************************************************)
(* examples *)
(* process properties: sizes of projected sets *)
let ecardinal = EProjection(Cardinal);;
let rcardinal = CProjection(Size);;
let ccardinal = CProjection(Size);;
let cpscardinal = RProjection(CPs(Cardinal));;

(* age/weight ratio on nodes, max size *)
let ageweight p =
 let r = Random in
 Probability(p, Property(DataProperty(MaxSize), r), Property(Age, r))
;;

(* simple sum |E| + |R| +|C| on processes, age/weight on nodes *)
let projection_sum r =
 let sum = PPlus(ecardinal, PPlus(rcardinal, ccardinal)) in
 Property (ELabel(PMin(sum)), ageweight r)
;;

(* slothrop: |E| + |CP(R)| +|C| on processes, random on nodes*)
let slothrop =
 let sum = PPlus(ecardinal, PPlus(cpscardinal, ccardinal)) in
 Property (ELabel(PMin(sum)), Random)
;;

let baby = Property(ELabel(PMin(ecardinal)), Property(ELabel(PMin(rcardinal)), Property(ELabel(PMin(cpscardinal)), Property(ELabel(PMin(ccardinal)), Random))))

(* simple sum |E| + |R| on processes, age/weight on nodes *)
let old r =
 let sum = PPlus(ecardinal, rcardinal) in
 Property (ELabel(PMin(sum)), ageweight r)
;;

(* Haruhiko-like *)
let japan size =
 let esize = EProjection(ESum(size)) in
 let csize = CProjection(TSum(size)) in
 let minsum = ELabel(PMin(PPlus(esize, csize))) in
 let data = DataProperty(size) in
 let negecard = Negative(ELabel(Count)) in
 Property (minsum, Property(data, Property(negecard, Random)))
;;

let japan_sum = japan SumSize

let japan_max = japan MaxSize

let rec to_string = function
   Property(np, s) -> "("^(np_to_string np)^", "^(to_string s)^")"
 | Probability(r, s1, s2) ->
  (string_of_float r)^"("^(to_string s1)^":"^(to_string s2)^")"
 | Random -> "?"
and np_to_string = function
 | Age -> "*"
 | DataProperty tpp -> "data("^(tpp_to_string tpp)^")"
 | Plus(np1, np2) -> (np_to_string np1)^" + "^(np_to_string np2) 
 | Negative np -> " -"^(np_to_string np)
 | ELabel psp -> "el("^(psp_to_string psp)^")"
and psp_to_string = function
 | PMin pp -> "min("^(pp_to_string pp)^")"
 | PSum pp -> "sum("^(pp_to_string pp)^")"
 | Count -> "#"
and pp_to_string = function
 | PPlus(pp1, pp2) -> (pp_to_string pp1)^" + "^(pp_to_string pp2)
 | EProjection ep -> "e("^(ep_to_string ep)^")"
 | RProjection tp -> "r("^(tp_to_string tp)^")"
 | CProjection tp -> "c("^(tp_to_string tp)^")"
and tp_to_string = function
   TSum tpp -> "sum("^(tpp_to_string tpp)^")"
 | CPs ep -> "cp("^(ep_to_string ep)^")"
 | Size -> "#"
and ep_to_string = function
   ESum tpp -> "sum("^(tpp_to_string tpp)^")"
 | Cardinal -> "#"
and tpp_to_string = function
 | MaxSize -> "smax"
 | SumSize -> "ssum"
;;









