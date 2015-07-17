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

(*** VALUES ******************************************************************)
val critical_pairs : U.Trs.t -> Equation.t list U.Monad.t
(** [critical_pairs trs] returns all critical pairs of the given
 rewrite system [trs].*)

val to_equations : U.Trs.t -> Equation.t list
 (** [to_equations trs] maps every rule in [trs] to an equation. *)

val of_equations : Equation.t list -> U.Trs.t
 (** [of_equations eqs] mapse every equation in [eqs] to a rule. *)

val to_wst_stringm : U.Trs.t -> string U.Monad.t
 (** [to_string_wstm trs] maps a TRS to a WST string representation. *)


val normal_form : U.Term.t -> U.Trs.t -> U.Term.t U.Monad.t
 (** [normal_form t trs] returns some normal form of [t] with respect to 
     [trs]. *)

val is_reducible : U.Term.t -> U.Trs.t -> bool U.Monad.t
 (** [is_reducible t trs] checks whether term [t] is reducible using
     rules in [trs]. *)
(*
val reduce : U.Trs.t -> U.Trs.t U.Monad.t
 (** [reduce trs] returns an interreduced TRS equivalent to [trs]. *)
*)
val readable : U.Trs.t -> U.Trs.t U.Monad.t
 (** [reduce trs] returns an interreduced TRS equivalent to [trs]. *)

val unflatten : U.Trs.t -> U.Trs.t U.Monad.t
