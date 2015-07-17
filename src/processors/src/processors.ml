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

(* Implementing a new processor:
 * In order to implement a new processor for TTT2 you have to keep three things
 * in mind:
 * - In TTT2 we differ between four kinds of processors: nontermination
 *   processors, termination processors, transformation processors and
 *   modifiers. A nontermination (termination) processor is a processor which
 *   can prove (on its own--at least sometimes) nontermination (termination) of
 *   a given problem. Respectively, a transformation processor is a processor
 *   which can never prove a given problem to be nonterminating or terminating.
 *   Such a processor can only modify a given problem. Finally a modifier is a
 *   processor which modifies a given problem by using history information.
 *   That means, in difference to a transformation processor a modifier gets
 *   also some old problem as input. Note that this distinction is not
 *   equivalent to the one used in the literature. However it is more
 *   convenient within our setting.
 * - Each processor should be put into a seperate module even if it is a
 *   variant of an other processor.
 * - For each processor you have to define a type [t] which characterizes the
 *   proof objects generated by your processor as well as two constants called
 *   [code] and [help]. The first constant has type [string] and defines the
 *   shortcut that is used to refer to this processor within TTT2. The second
 *   constant has type [string * string list * (string * string) list] where
 *   the first component provides some detailed description of the processor,
 *   the second component specifies some keywords which characterizes this
 *   processor, and the last component describes the flags of the processor.
 *   Beside this basic facts, you have to provide a method [solve] which
 *   applies the processor to the given input. This method has one of the
 *   following types:
 *   - string list -> Problem.t -> bool
 *   - string list -> Problem.t -> t option
 *   - string list -> Problem.t -> t option Rewriting.Monad.t
 *   - string list -> Problem.t -> Problem.t -> t option
 *   - string list -> Problem.t -> Problem.t -> t option Rewriting.Monad.t
 *   The first argument specifies the flags given to the method. The second
 *   argument defines the problem to which the technique should be applied.
 *   In case that your processor is a modifier, a third argument exists
 *   which corresponds to the second input problem. The return type depends
 *   on the fact whether your method is a predicate or not. If so, the solve
 *   method returns a boolean value. Otherwise some or none proof combined
 *   with some or none complexity is returned. Note that the optional return
 *   type is important to propagate the fact if your processor failed or not.
 *   In general a processor should fail if either it is not applicable or does
 *   not make any progress. Last but not least your processor module should
 *   also include the two functions [derviational] and [runtime] of type
 *   [t -> Complexity.t] which return information on the complexity of the
 *   termination problem that remains after applying the new processor. Note
 *   that for predicates and nontermiantion processors there is no need to
 *   implement thoes functions.
 *)

(*** MODULES ******************************************************************)
module Rewritingx = Processorsx.Rewritingx;;
module Problem = Processorsx.Problem;;

module Modifier = struct
 module Restore = Processorsx.Restore;;
end

module Nontermination = struct
 module Contained = Processorsx.Contained;;
 module LoopSat = Processorsx.LoopSat;;
 module Unfolding = Processorsx.Unfolding;;
 module Variables = Processorsx.Variables;;
end

module Predicate = struct
 module Applicative = Processorsx.IsApplicative;;
 module Collapsing = Processorsx.IsCollapsing;;
 module Constructor = Processorsx.IsConstructor;;
 module Dummy = Processorsx.IsDummy;;
 module Duplicating = Processorsx.IsDuplicating;;
 module Erasing = Processorsx.IsErasing;;
 module Flat = Processorsx.IsFlat;;
 module Full = Processorsx.IsFull;;
 module Ground = Processorsx.IsGround;;
 module Innermost = Processorsx.IsInnermost;;
 module LeftGround = Processorsx.IsLeftGround;;
 module LeftLinear = Processorsx.IsLeftLinear;;
 module Linear = Processorsx.IsLinear;;
 module Outermost = Processorsx.IsOutermost;;
 module Overlapping = Processorsx.IsOverlapping;;
 module Overlay = Processorsx.IsOverlay;;
 module Relative = Processorsx.IsRelative;;
 module RightGround = Processorsx.IsRightGround;;
 module RightLinear = Processorsx.IsRightLinear;;
 module Shallow = Processorsx.IsShallow;;
 module Srs = Processorsx.IsSrs;;
 module Standard = Processorsx.IsStandard;;
 module StronglyNonOverlapping = Processorsx.IsStronglyNonOverlapping;;
 module Trs = Processorsx.IsTrs;;
end

module Termination = struct
 module Arctic = Processorsx.ArcticInterpretation;;
 module Bounds = Processorsx.Bounds;;
 module Csf = Processorsx.Csf;;
 module Dp = Processorsx.Dp;;
 module Dg = Processorsx.Dg;;
 module Fbi = Processorsx.FixedBaseElementaryInterpretation;;
 module Kbo = Processorsx.Kbo;;
 module Lpo = Processorsx.Lpo;;
 module Tkbo = Processorsx.Tkbo;;
 module Acrpo = Processorsx.Acrpo;;
 module Ackbo = Processorsx.Ackbo;;
 module Matrix = Processorsx.MatrixInterpretation;;
 module Poly = Processorsx.PolynomialInterpretation;;
 module SemanticLabeling = Processorsx.SemanticLabeling;;
 module Sccs = Processorsx.Sccs;;
 module SizeChangeTermination = Processorsx.SizeChangeTermination;;
 module SubtermCriterion = Processorsx.SubtermCriterion;;
 module Trivial = Processorsx.Trivial;;
end

module Transformation = struct
 module Cp = Processorsx.Cp;;
 module Dpify = Processorsx.Dpify;;
 module Linear = Processorsx.Linear;;
 module QuasiRootLabeling = Processorsx.QuasiRootLabeling;;
 module Reflect = Processorsx.Reflect;;
 module Reverse = Processorsx.Reverse;;
 module RootLabeling = Processorsx.RootLabeling;;
 module Rt = Processorsx.Rt;;
 module Split = Processorsx.Split;;
 module St = Processorsx.St;;
 module TypeIntroduction = Processorsx.TypeIntroduction;;
 module Udpac = Processorsx.Udpac;;
 module Uncurry = Processorsx.Uncurry;;
 module Uncurryx = Processorsx.Uncurryx;;
 module Ur = Processorsx.Ur;;
 (*transformations for confluence *)
 module Dup = Processorsx.Dup;;
 module Star = Processorsx.Star;;
end

module Confluence = struct
 module Nonconfluence = Processorsx.Nonconfluence;;
 module RuleLabeling = Processorsx.RuleLabeling;;
 module Shift = Processorsx.Shift;;
end