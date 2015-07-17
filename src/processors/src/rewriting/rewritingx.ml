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
type label = Prelude.label;;
type signature = Prelude.signature;;

(*** MODULES ******************************************************************)
module Function = Prelude.Function;;
module Variable = Prelude.Variable;;
module Label = Label;;
module Signature = Signature;;
module Monad = Monad;;
module Position = Position;;
module Parser = Prelude.Parser;;
module Term = Term;;
module Context = Context;;
module Substitution = Substitution;;
module Elogic = Prelude.Elogic;;
module Rule = Rule;;
module Trs = Trs;;
module Rewrite = Prelude.Rewrite;;
module Filtering = Filtering;;
module Graph = Graph;;
module Projection = Projection;;
module Diagram = Diagram;;
module Aclogic = Aclogic;;
