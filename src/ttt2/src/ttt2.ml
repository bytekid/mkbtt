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

(*** MODULES ******************************************************************)
module Strategy = struct
 module Syntax = Ttt2x.Syntax;;
 module Parser = Ttt2x.Parser;;
 module Lexer = Ttt2x.Lexer;;
 module Processor = Ttt2x.Processor;;
 module Proof = Ttt2x.Proof;;
 module Status = Ttt2x.Status;;
 module State = Ttt2x.State;;
 module Monad = Ttt2x.Monad;;
 module Modifier = Ttt2x.Modifier;;
 module Nontermination = Ttt2x.Nontermination;;
 module Predicate = Ttt2x.Predicate;;
 module Termination = Ttt2x.Termination;;
 module Transformation = Ttt2x.Transformation;;
 module Main = Ttt2x.Main;;
end

module Input = struct
 module SrsSyntax = Ttt2x.SrsSyntax;;
 module SrsParser = Ttt2x.SrsParser;;
 module SrsLexer = Ttt2x.SrsLexer;;
 module TrsSyntax = Ttt2x.TrsSyntax;;
 module TrsParser = Ttt2x.TrsParser;;
 module TrsLexer = Ttt2x.TrsLexer;;
 module ConfSyntax = Ttt2x.ConfSyntax;;
 module ConfParser = Ttt2x.ConfParser;;
 module ConfLexer = Ttt2x.ConfLexer;;
 module Xml = Ttt2x.Xml;;
end

module Answer = Ttt2x.Answer;;
module Kernel = Ttt2x.Kernel;;
