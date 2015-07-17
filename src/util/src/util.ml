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

(*** INCLUDES *****************************************************************)
include Utilx.Prelude;;

(*** MODULES ******************************************************************)
module Arg = Utilx.Argx;;
module Bool = Utilx.Bool;;
module Char = Utilx.Charx;;
module Complexity = Utilx.Complexity;;
module Either = Utilx.Either;;
module Filename = Utilx.Filenamex;;
module Graph = Utilx.Graph;;
module Hashtbl = Utilx.Hashtblx;;
module Index = Utilx.Index;;
module Int = Utilx.Int;;
module Isomorphism = Utilx.Isomorphism;;
module List = Utilx.Listx;;
module LazyList = Utilx.LazyList;;
module Stream = Utilx.Stream;;
module Map = Utilx.Mapx;;
module Monad = Utilx.Monad;;
module Option = Utilx.Option;;
module Pair = Utilx.Pair;;
module Process = Utilx.Process;;
module Process2 = Utilx.Process2;;
module Quadruple = Utilx.Quadruple;;
module Replacement = Utilx.Replacement;;
module String = Utilx.Stringx;;
module Triple = Utilx.Triple;;
module Xsltproc = Utilx.Xsltproc;;

(*** FUNCTIONS ****************************************************************)
let either = Either.either;;
let odd = Int.odd;;
let even = Int.even;;
let cons = List.cons;;
let option = Option.option;;
let some = Option.some;;
let pair = Pair.make;;
let uncurry = Pair.uncurry;;
let uncurry3 = Triple.uncurry;;
