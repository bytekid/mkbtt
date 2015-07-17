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

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module CP = CompletionProcessx;;
module T = U.Term;;
module R = U.Rule;;
module Sub = U.Substitution;;
module Monad = World.Monad;;

include Types.Overlap

(*** FUNCTIONS ***********************************************************)

let source o = o.source;;
let inner_rule o = o.inner_rule;;
let outer_rule o = o.outer_rule;;
let position o = o.position;;
let sub o = o.sub;;
let mom o = Triple.thd o.outer_rule;;
let dad o = Triple.thd o.inner_rule;; 

let make t orule p irule s = {
 source=t; 
 outer_rule=orule;
 position=p; 
 inner_rule=irule;
 sub=s;
};;

let is_nontrivial o =
 let t = source o in
 not ((dad o = (mom o)) && (o.position = (ACPosition.root t)))
;;
