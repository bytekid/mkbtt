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
open Rewriting;;

(*** MODULE TYPES *************************************************************)
module type CATEGORIZATION = sig
 type 'a m
 type t

 val add : Function.t -> t -> t m
 val create : int -> t
 val remove : Function.t -> t -> t m
 val category : Function.t -> t -> Function.t list m
 val find : Function.t -> t -> Function.t list m
 val funs : t -> Function.t list
 val copy : t -> t
 val compare : t -> t -> int
 val fprintf : Format.formatter -> t -> unit
end

module type REWRITING = sig
 module Function : Rewriting.FUNCTION with type t = Rewriting.Function.t
 module Variable : Rewriting.VARIABLE with type t = Rewriting.Variable.t
 module Signature : Rewriting.STATE
 module Monad : Rewriting.MONAD with type state = Signature.t
 module Position : Rewriting.POSITION with type t = Rewriting.Position.t
 module Term : Rewriting.TERM with type 'a m = 'a Monad.t

 module Context : Rewriting.CONTEXT with
  type 'a m = 'a Monad.t and type term = Term.t

 module Substitution : Rewriting.SUBSTITUTION with
  type 'a m = 'a Monad.t and type term = Term.t and type context = Context.t

 module Elogic : Rewriting.ELOGIC with
  type 'a m = 'a Monad.t and type substitution = Substitution.t and
  type term = Term.t

 module Rule : Rewriting.RULE with
  type 'a m = 'a Monad.t and type substitution = Substitution.t and
  type term = Term.t

 module Trs : Rewriting.TRS with
  type 'a m = 'a Monad.t and type rule = Rule.t and type term = Term.t
end
