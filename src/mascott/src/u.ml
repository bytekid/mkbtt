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

module Rewriting = Processors.Rewritingx;;
module Position = Rewriting.Position;;
module Variable = Rewriting.Variable;;
module Function = Rewriting.Function;;
module Monad = Rewriting.Monad;;
module Term = Rewriting.Term;;
module Context = Rewriting.Context;;
module Parser = Rewriting.Parser;;
module Label = Rewriting.Label;;
module Signature = Rewriting.Signature;;
module Substitution = Rewriting.Substitution;;
module Rule = Rewriting.Rule;;
module Trs = Rewriting.Trs;;
module Elogic = Rewriting.Elogic;;

module MyTTT2 = struct
 let run = Ttt2.Strategy.Main.run
end

module MyProblem = struct
 type t = Processors.Problem.t
 let make_sp = Processors.Problem.make_sp
 let make_ep = Processors.Problem.make_ep
 let get_trs = Processors.Problem.get_trs
end

module MyTrsParser = struct
 type token = Ttt2.Input.TrsParser.token
 let trs = Ttt2.Input.TrsParser.trs
end

module MyTrsLexer = struct
 let token = Ttt2.Input.TrsLexer.token
end

module MyTrsSyntax = struct
 type t = Ttt2.Input.TrsSyntax.t
 let to_problem_with = Ttt2.Input.TrsSyntax.to_problem_with
end
