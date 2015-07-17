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

(** Automata Library
@author Martin Korp
@since  Thu Jul  9 15:31:11 CEST 2009 *)

(** This library provides an implementation of tree automata. Beside some
standard functionalities, the library provides mainly functions that are
needed to compute the rewrite closure of a regular language by using tree
automata completion. *)

(*** MODULES ******************************************************************)
(** {2 Module State} *)

module type STATE = sig
 (*** TYPES *******************************************************************)
 type t

 (*** VALUES ******************************************************************)
 (** {3 Miscellaneous} *)

 val combinations : int -> t list -> (int * t list list) list
 (** [combinations n ps] returns a list of pairs. Each pair consist of
 some number [i], between [0] and [n], and a list containing all possible
 combinations of at most [i] elements of [ps]. *)
 val copy : t -> t
 (** [copy p] returns a copy of [p]. *)
 val hash : t -> int
 (** [hash p] returns a hash value for [p]. It is guaranteed that
 [hash p = hash q] if and only if [compare p q = 0]. *)
 val max : t -> t -> int
 (** [max p q] returns the maximal state of [p] and [q] with resect to the
 compare function [compare]. *)
 val min : t -> t -> int
 (** [min p q] returns the minimal state of [p] and [q] with resect to the
 compare function [compare]. *)
 val next : t -> t
 (** [next p] returns the successor of [p] with respect to the compare
 function [compare]. *)
 val to_int : t -> int
 (** [to_int p] is equivalent to {!val: Automata.STATE.hash}. *)
 val zero : t
 (** [zero] returns the smallest state with respect to the compare function
 [compare]. *)

 (** {3 Compare Functions} *)

 val compare : t -> t -> int
 (** [compare p q] compares [p] and [q]. This function defines a total
 ordering on states. *)
 val equal : t -> t -> bool
 (** [equal p q] checks if [p] and [q] are equal. This function is equivalent
 to [compare p q = 0]. *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt p] prints [p] using the [OCaml] module [Format]. *)
 val to_string : t -> string
 (** [to_string p] returns a formatted string that represents [p]. *)
end

(** States
@author Martin Korp
@since  Thu Sep  3 11:03:31 CEST 2009 *)

(** This module provides basic functions that deal with states. Note that
the names of states are fixed to type [int]. *)
module State : STATE

(** {2 Module Status} *)

module type STATUS = sig
 (*** TYPES *******************************************************************)
 type t

 (*** VALUES ******************************************************************)
 (** {3 Constructors} *)

 val init : t
 (** [init] returns the initial status. *)
 val add_state : State.t -> t -> t
 (** [add_state p s] adds state [p] to status [s]. I.e., if [p] represents a
 fresh state with respect to [s] then [s] is updated such that [p] represents
 the last generated fresh state. Otherwise [s] is returned unchanged. *)
 
 (** {3 Fresh Symbols} *)

 val create_state : int -> t -> State.t * t
 (** [create_state n s] retuns the state that has name [n]. *)
 val fresh_state : t -> State.t * t
 (** [fresh_state s] returns a fresh state together with the new status. *)

 (** {3 Search Functions} *)

 val find_state : int -> t -> State.t
 (** [find_state n s] retuns the state that has name [n]. *)
 val find_state_name : State.t -> t -> int
 (** [find_state_name p s] returns the name of [p]. *)

 (** {3 Miscellaneous} *)

 val copy : t -> t
 (** [copy s] returns a copy of [s]. *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt s] prints [s] using the [OCaml] module [Format]. *)
 val to_string : t -> string
 (** [to_string s] returns a formatted string that represents [s]. *)
end

(** Status
@author Martin Korp
@since  Thu Sep  3 11:03:31 CEST 2009 *)

(** This module is used to generate fresh states. Note that it is only
guaranteed that a generated state is fresh if and only if the current
status is not out of date. *)
module Status : STATUS

(** {2 Module Monad} *)

module type MONAD = sig
 (*** TYPES *******************************************************************)
 type 'a m

 (*** INCLUDES ****************************************************************)
 include Rewriting.MONAD

 (*** VALUES ******************************************************************)
 (** {3 Access Functions} *)

 val liftm : 'a m -> 'a t
 (** [liftm m] is equivalent to
 {!val: Util.Monad.Transformer.COMBINED_MONAD.liftm}. *)

 (** {3 Constructors} *)

 val add_state : State.t -> unit t
 (** [add_state p] is equivalent to {!val: Automata.STATUS.add_state}
 except that the status is encapsulated in a monad. Note that as a side effect
 the old signature is changed too. *)
 
 (** {3 Fresh Symbols} *)

 val create_state : int -> State.t t
 (** [create_state n] is equivalent to {!val: Automata.STATUS.create_state}
 except that the status is encapsulated in a monad. Note that as a side effect
 the old signature is changed too. *)
 val fresh_state : State.t t
 (** [fresh_state] is equivalent to {!val: Automata.STATUS.fresh_state} except
 that the status is encapsulated in a monad. Note that as a side effect
 the old signature is changed too. *)

 (** {3 Search Functions} *)

 val find_state : int -> State.t t
 (** [find_state n] is equivalent to {!val: Automata.STATUS.find_state} except
 that the status is encapsulated in a monad. Note that as a side effect
 the old signature is changed too. *)
 val find_state_name : State.t -> int t
 (** [find_state_name p] is equivalent to
 {!val: Automata.STATUS.find_state_name} except that the status is encapsulated
 in a monad. Note that as a side effect the old signature is changed too. *)
end

(** State Monad
@author Martin Korp
@since  Thu Sep  3 11:03:31 CEST 2009 *)

(** This module defines a state monad that is used to encapsulate the
status from the functions provided by the modules within this library. *)
module Monad : MONAD with
 type 'a m = 'a Rewriting.Monad.t and
 type state = Rewriting.Signature.t * Status.t

(** {2 Module Parser} *)

(** Parser
@author Martin Korp
@since  Mon Sep  1 12:21:54 CEST 2008 *)

(** This module defines the basic setting needed to parse terms, left-hand
sides of transitions, and tree automata in a functional way. *)
module Parser : Parsec.STATEFUL_CHAR_PARSER with
 type input = Parsec.StringInput.t and
 type state = Rewriting.Signature.t * Status.t 

(** {2 Module Term} *)

module type TERM = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type 'a p
 type term

 type t =
  | Var of Rewriting.Variable.t
  | State of State.t
  | Fun of Rewriting.Function.t * t list

 (*** VALUES ******************************************************************)
(* TODO remove function *) val min : t list -> t list
(* TODO remove function *) val choose : t list -> t
 (** {3 Constructors and Destructors} *)

 val make_fun : Rewriting.Function.t -> t list -> t
 (** [make_fun f ts] returns the term [Fun (f,ts)]. *)
 val make_state : State.t -> t
 (** [make_state p] returns the term [State p]. *)
 val make_var : Rewriting.Variable.t -> t
 (** [make_var x] returns the term [Var x]. *)
 val of_term : term -> t
 (** [of_term t] transforms an ordinary term [t] into a term over the extended
 type {!type: Automata.TERM.t}. Not tail-recursive. *)
 val to_term : t -> term
 (** [to_term t] transforms a term [t] over the extended type
 {!type: Automata.TERM.t} into an ordinary term. Not tail-recursive.
 @raise Failure "state occured" if [t] contains states. *)

 (** {3 Term Symbols} *)

 val cons : t -> Rewriting.Function.t list
 (** [cons t] returns all constants of the term [t]. This function is not
 tail-recursive. *)
 val funs : t -> Rewriting.Function.t list
 (** [funs t] returns all function symbols of the term [t]. This function is
 not tail-recursive. *)
 val root : t -> Rewriting.Function.t option
 (** [root t] returns the root symbol of [t]. *)
 val states : t -> State.t list
 (** [states t] returns all states of the term [t]. This function is not
 tail-recursive. *)
 val vars : t -> Rewriting.Variable.t list
 (** [vars t] returns all variables of the term [t]. This function is not
 tail-recursive. *)

 (** {3 Properties} *)

 val is_fun : t -> bool
 (** [is_fun t] checks if [t] is a function. *)
 val is_ground : t -> bool
 (** [is_ground t] checks if [t] is a ground term, i.e., a term that does not
 contain any variables. *)
 val is_state : t -> bool
 (** [is_state t] checks if [t] is a state. *)
 val is_var : t -> bool
 (** [is_var t] checks if [t] is a variable. *)

 (** {3 Miscellaneous} *)

 val args : t -> t list
 (** [args t] returns the immediate arguments of the term [t]. If [t] is a
 variable or a state, the empty list is returned. *)
 val fun_size : t -> int
 (** [fun_size t] returns the total number of function symbols of the term
 [t]. Note that double occurences of a function symbol are counted twice.
 This function is not tail-recursive. *)
 val size : t -> int
 (** [size t] returns the size of the term [t]. This function is not
 tail-recursive. *)
 val state_size : t -> int
 (** [state_size t] returns the total number of states of the term [t].
 Note that double occurences of a state are counted twice. This function
 is not tail-recursive. *)
 val var_size : t -> int
 (** [var_size t] returns the total number of variables of the term [t].
 Note that double occurences of a variable are counted twice. This function
 is not tail-recursive. *)

 (** {3 Compare Functions} *)

 val compare : t -> t -> int
 (** [compare s t] compares [s] and [t]. This function defines a total
 ordering on terms. *)
 val equal : t -> t -> bool
 (** [equal s t] checks if [s] and [t] are equal. This function is equivalent
 to [compare s t = 0]. *)

 (** {3 Parsers} *)

 val of_string : string list -> string list -> string -> t m
 (** [of_string vs ps s] applies {!val: Automata.TERM.parse} to the input
 string [s] and lifts the result into a state monad. Note that as a side effect
 the old signature is changed too. *)
 val parse : string list -> string list -> t p
 (** [parse vs ps] takes two list of strings (the identifiers that should be
 parsed as variables and the identifiers that should be parsed as states) and
 returns a term-parser (having a signature as well as some status as internal
 state). *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt t] prints [t] using the [OCaml] module [Format]. This
 function is not tail-recursive. *)
 val fprintfm : Format.formatter -> t -> unit m
 (** [fprintfm fmt t] prints [t] using the [OCaml] module [Format]. This
 function uses the information stored in the underlying signature to print
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is printed. Note that as a side
 effect copies of the underlying signature are changed too. This function is
 not tail-recursive. *)
 val to_string : t -> string
 (** [to_string t] returns a formatted string that represents [t]. This
 function is not tail-recursive. *)
 val to_stringm : t -> string m
 (** [to_stringm t] returns a formatted string that represents [t]. This
 function uses the information stored in the underlying signature to represent
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is used. Note that as a side effect
 copies of the underlying signature are changed too. This function is not
 tail-recursive. *)
end

(** Terms
@author Martin Korp
@since  Thu Sep  3 11:03:31 CEST 2009 *)

(** This module provides a specialized implementation of terms, in which in
addition to variables and functions, also states can appear. *)
module Term : TERM with
 type 'a m = 'a Monad.t and type 'a p = 'a Parser.t and
 type term = Rewriting.Term.t

(** {2 Module Substitution} *)

module type SUBSTITUTION = sig
 (*** TYPES *******************************************************************)
 type term

 (*** INCLUDES ****************************************************************)
 include Util.Replacement.SIGNATURE with
  type domain = Rewriting.Variable.t and type range = State.t

 (*** VALUES ******************************************************************)
 (** {3 Apply Substitutions} *)

 val apply_term : t -> term -> term
 (** [apply_term s u] applies the substitution [s] to the term [u]. This
 function is not tail-recursive. *)
end

(** State Substitutions
@author Martin Korp
@since  Thu Sep  3 11:03:31 CEST 2009 *)

(** This module provides basic functions that deal with state substitutions. *)
module Substitution : SUBSTITUTION with type term = Term.t

(** {2 Module Lhs} *)

module type LHS = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type 'a p
 type term
 type t = State of State.t | Fun of Rewriting.Function.t * State.t list

 (*** VALUES ******************************************************************)
 (** {3 Constructors} *)

 val make_fun : Rewriting.Function.t -> State.t list -> t
 (** [make_fun f ps] returns the term [Fun (f,ps)]. *)
 val make_state : State.t -> t
 (** [make_state p] returns the term [State p]. *)
 val of_term : term -> t
 (** [of_term t] transforms the term [t] of type {!type: Automata.TERM.t} into
 a term of type {!type: Automata.LHS.t}. Not tail-recursive.
 @raise Failure "illegal term" if [t] is a variable or an immediate argument
 of [t] is a variable or a function. *)
 val to_term : t -> term
 (** [to_term l] transforms the left-hand side [l] of type
 {!type: Automata.LHS.t} into a term of type {!type: Automata.TERM.t}.
 Not tail-recursive. *)

 (** {3 Term Symbols} *)

 val states : t -> State.t list
 (** [states l] returns all states of the term [l]. *)
 val root : t -> Rewriting.Function.t option
 (** [root l] returns the root symbol of [l]. *)

 (** {3 Properties} *)

 val is_fun : t -> bool
 (** [is_fun l] checks if [l] is a function. *)
 val is_state : t -> bool
 (** [is_state l] checks if [l] is a state. *)

 (** {3 Miscellaneous} *)

 val args : t -> State.t list
 (** [args l] returns the immediate arguments of [l]. If [l] is a state, the
 empty list is returned. *)
 val hash : t -> int
 (** [hash l] returns a hash value for [l]. It is guaranteed that
 [hash l = hash l'] if and only if [compare l l' = 0]. *)

 (** {3 Compare Functions} *)

 val compare : t -> t -> int
 (** [compare l l'] compares [l] and [l']. This function defines a total
 ordering on terms. *)
 val equal : t -> t -> bool
 (** [equal l l'] checks if [l] and [l'] are equal. This function is
 equivalent to [compare l l' = 0]. *)

 (** {3 Parsers} *)

 val of_string : string list -> string -> t m
 (** [of_string ps s] applies {!val: Automata.LHS.parse} to the input
 string [s] and lifts the result into a state monad. Note that as a side
 effect the old signature is changed too. *)
 val parse : string list -> t p
 (** [parse ps] takes a list of strings (the identifiers that should be
 parsed as states) and returns a term-parser for the left-hand sides of
 transitions (having a signature as well as some status as internal state). *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt l] prints [l] using the [OCaml] module [Format]. *)
 val fprintfm : Format.formatter -> t -> unit m
 (** [fprintfm fmt l] prints [l] using the [OCaml] module [Format]. This
 function uses the information stored in the underlying signature to print
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is printed. Note that as a side
 effect copies of the underlying signature are changed too. This function is
 not tail-recursive. *)
 val to_string : t -> string
 (** [to_string l] returns a formatted string that represents [l]. *)
 val to_stringm : t -> string m
 (** [to_stringm l] returns a formatted string that represents [l]. This
 function uses the information stored in the underlying signature to represent
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is used. Note that as a side effect
 copies of the underlying signature are changed too. This function is not
 tail-recursive. *)
end

(** Left-Hand Sides
@author Martin Korp
@since  Thu Sep  3 11:03:31 CEST 2009 *)

(** This module provides an implementation of terms that are used as left-hand
sides in tree automata transitions. *)
module Lhs : LHS with
 type 'a m = 'a Monad.t and type 'a p = 'a Parser.t and type term = Term.t

(** {2 Module Rhs} *)

module type RHS = sig
 (*** TYPES *******************************************************************)
 type t

 (*** VALUES ******************************************************************)
(* TODO remove function *) val make : State.t option -> State.t list -> State.t list -> t
(* TODO remove function *) val set_agent : State.t -> t -> t
(* TODO remove function *) val agent : t -> State.t option
(* TODO remove function *) val violations : t -> State.t list
(* TODO remove function *) val det : t -> t
 (** {3 Constructors and Destructors} *)

 val add : State.t -> t -> t
 (** [add p r] adds state [p] to [r]. *)
 val create : State.t list -> t
 (** [create ps] creates a new right-hand side consisting of the states [ps].
 @raise Failure "rhs empty" if [ps] is empty. *)
 val extend : State.t list -> t -> t
 (** [extend ps r] adds states [ps] to [r]. *)
 val singleton : State.t -> t
 (** [singleton p] creates a new right-hand side consisting of state [p]. *)
 val states : t -> State.t list
 (** [states r] returns the states of the right-hand side [r]. *)

 (** {3 Iterators} *)

 val fold : (State.t -> 'a -> 'a) -> t -> 'a -> 'a
 (** [fold f r d] applies function [f] to all states of [r]. As default value
 [d] is used. *)
 val iter : (State.t -> unit) -> t -> unit
 (** [iter f r] applies function [f] to all states in [r]. *)
 val map : (State.t -> State.t) -> t -> t
 (** [map f r] maps [r] to some new right-hand side by applying function [f]
 to all states in [r]. *)

 (** {3 Scan Functions} *)

 val mem : State.t -> t -> bool
 (** [mem p r] checks if [r] contains [p]. *)

 (** {3 Search Functions} *)

 val filter : (State.t -> bool) -> t -> t option
 (** [filter c r] removes all states of [r] that do no satisfy the condition
 [c]. If the resulting right-hand side is empty [None] is returned. *)
 val remove : State.t -> t -> t option
 (** [remove p r] removes the state [p] from [r]. If the resulting right-hand
 side is empty [None] is returned. *)

 (** {3 Miscellaneous} *)

 val diff : t -> t -> t option
 (** [diff r r'] removes all states of [r] that are contained in [r'].
 If the resulting right-hand side is empty [None] is returned. *)
 val inter : t -> t -> t option
 (** [inter r r'] removes all states of [r] that are not contained in [r'].
 If the resulting right-hand side is empty [None] is returned. *)
 val max : t -> State.t
 (** [max r] returns the maximal state of [r]. *)
 val size : t -> int
 (** [size r] returns the number of states of [r]. *)

 (** {3 Compare Functions} *)

 val compare : t -> t -> int
 (** [compare r r'] compares [r] and [r']. This function defines a total
 ordering on right-hand sides. *)
 val equal : t -> t -> bool
 (** [equal r r'] checks if [r] and [r'] are equal. This function is
 equivalent to [compare r r' = 0]. *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt r] prints [r] using the [OCaml] module [Format]. *)
 val to_string : t -> string
 (** [to_string r] returns a formatted string that represents [r]. *)
end

(** Right-Hand Sides
@author Martin Korp
@since  Thu Sep  3 11:03:31 CEST 2009 *)

(** This module provides an implementation of the container that is used to
store the right-hand sides of tree automata transitions. *)
module Rhs : RHS

(** {2 Module Automaton} *)

module type AUTOMATON = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type 'a p
 type lhs
 type rhs
 type substitution
 type term
 type trs
 type t

 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val add : lhs -> rhs -> t -> t m
 (** [add l r a] adds a new transition with left-hand side [l] and right-hand
 side [r] to [a]. If [a] contains allready a transition with left-hand side
 [l] then [a] remains unchanged. Note that as a side effect [a] is changed
 too. *)
 val add_final : State.t -> t -> t
 (** [add_final p a] adds [p] as a final state to [a]. Note that as a side
 effect [a] is changed too. *)
 val create : int -> t
 (** [create n] creates an empty tree automaton which will eventually consists
 of [n] states. *)
 val extend : lhs -> State.t list -> t -> t m
 (** [extend l ps a] adds the new transition [l -> ps] to [a]. If [a] contains
 allready a transition with left-hand side [l] then the states [ps] are added
 to the current right-hand side. Note that as a side effect [a] is changed
 too. *)
 val remove : lhs -> t -> t m
 (** [remove l a] removes the transition with left-hand side [l]. Note that as
 a side effect [a] is changed too. *)
 val remove_final : State.t -> t -> t
 (** [remove_final p a] removes the final state [p] from [a]. Note that as a
 side effect [a] is changed too. *)
 val replace : lhs -> rhs -> t -> t m
 (** [replace l r a] adds a new transition with left-hand side [l] and
 right-hand side [r] to [a]. If [a] contains allready a transition with
 left-hand side [l] then it is replaced. Note that as a side effect [a]
 is changed too. *)
 val set_finals : State.t list -> t -> t
 (** [set_finals ps a] defines the final states of [a]. As a side effect [a]
 is changed too. *)
 val update : lhs -> State.t -> t -> t m
 (** [update l p a] adds the new transition [l -> p] to [a]. If [a] contains
 allready a transition with left-hand side [l] then [p] is added to the current
 right-hand side. Note that as a side effect [a] is changed too. *)

 (** {3 States, Transitions, and Function Symbols} *)

 val category : Rewriting.Function.t -> t -> Rewriting.Function.t list m
 (** [category f a] returns all function symbols that are in the same category
 as [f]. *)
 val finals : ?i:bool -> t -> State.t list
 (** [finals ~i:i a] returns all finals states of the automaton [a]. If [i]
 is set to true all final states of [a], including inaccessible states, are
 returned. By default, [i] is set to false. *)
 val funs : ?p:(Rewriting.Function.t -> bool) -> t -> Rewriting.Function.t list
 (** [funs ~p:p a] returns all function symbols which are contained in
 transitions of the automaton [a] and satisfy the predicate [p]. By
 default, [p] is configurated such that all transitions are returned. *)
 val states : ?i:bool -> t -> State.t list
 (** [states ~i:i a] returns the states of the automaton [a]. If [i] is set
 to true all states of [a], including inaccessible states, are returned. By
 default, [i] is set to false. *)
 val trans : ?p:(lhs -> rhs -> bool) -> t -> (lhs * rhs) list
 (** [trans ~p:p a] returns the transitions of the automaton [a] that satisfy
 the predicate [p]. By default, [p] is configurated such that all transitions
 are returned. *)

 (** {3 Iterators} *)

 val fold : (lhs -> rhs -> 'a -> 'a) -> t -> 'a -> 'a
 (** [fold f a d] applies function [f] to all transitions of [a]. As default
 value [d] is used. *)
 val fold_eps : (lhs -> rhs -> 'a -> 'a) -> t -> 'a -> 'a
 (** [fold_eps f a d] applies function [f] to all epsilon transitions of [a].
 As default value [d] is used. *)
 val fold_trans : (lhs -> rhs -> 'a -> 'a) -> t -> 'a -> 'a
 (** [fold_trans f a d] applies function [f] to all non-epsilon transitions of
 [a]. As default value [d] is used. *)
 val foldc_trans : Rewriting.Function.t -> (lhs -> rhs -> 'a -> 'a) -> t -> 'a
  -> 'a m
 (** [foldc_trans c f a d] applies function [f] to all non-epsilon transitions
 of [a] contained in the categorization [c]. As default value [d] is used. *)
 val foldf_trans : Rewriting.Function.t -> (lhs -> rhs -> 'a -> 'a) -> t -> 'a
  -> 'a
 (** [foldf_trans g f a d] applies function [f] to all non-epsilon transitions
 of [a] with root symbol [g]. As default value [d] is used. *)
 val iter : (lhs -> rhs -> unit) -> t -> unit
 (** [iter f a] applies function [f] to all transitions of [a]. *)
 val iter_eps : (lhs -> rhs -> unit) -> t -> unit
 (** [iter_eps f a] applies function [f] to all epsilon transitions of [a]. *)
 val iter_trans : (lhs -> rhs -> unit) -> t -> unit
 (** [iter_trans f a] applies function [f] to all non-epsilon transitions of
 [a]. *)
 val iterc_trans : Rewriting.Function.t -> (lhs -> rhs -> unit) -> t -> unit m
 (** [iterc_trans c f a] applies function [f] to all non-epsilon transitions
 of [a] contained in the categorization [c]. *)
 val iterf_trans : Rewriting.Function.t -> (lhs -> rhs -> unit) -> t -> unit
 (** [iterf_trans g f a] applies function [f] to all non-epsilon transitions
 of [a] with root symbol [g]. *)
 val map : (rhs -> rhs) -> t -> t
 (** [map f a] applies the function [f] to the right-hand sides of [a]. Note
 that as a side effect [a] is changed too. *)
 val map_eps : (rhs -> rhs) -> t -> t
 (** [map_eps f a] applies the function [f] to the right-hand sides of epsilon
 transitions in [a]. Note that as a side effect [a] is changed too. *)
 val map_trans : (rhs -> rhs) -> t -> t
 (** [map_trans f a] applies the function [f] to the right-hand sides of
 non-epsilon transitions in [a]. Note that as a side effect [a] is changed
 too. *)
 val mapc_trans : Rewriting.Function.t -> (rhs -> rhs) -> t -> t m
 (** [mapc_trans c f a] applies the function [f] to the right-hand sides of
 non-epsilon transitions contained in the categorization [c]. Note that as a
 side effect [a] is changed too. *)
 val mapf_trans : Rewriting.Function.t -> (rhs -> rhs) -> t -> t
 (** [mapf_trans g f a] applies the function [f] to the right-hand sides of
 non-epsilon transitions with root symbol [g]. Note that as a side effect [a]
 is changed too. *)

 (** {3 Epsilon Closures} *)

 val ec_states : State.t list -> t -> State.t list
 (** [ec_states ps a] closes the states [ps] under epsilon transitions. *)
 val ec_rhs : rhs -> t -> rhs
 (** [ec_rhs r a] closes the right-hand side [r] under epsilon transitions. *)
 val ec : ?r:bool -> t -> t
 (** [ec ~r:r a] closes [a] under epsilon transitions. If [r] is set to true,
 all epsilon transitions are removed aftwerwards. Per default [r] is set to
 false. Note that as a side effect [a] is changed too. *)

 (** {3 Reachability Analysis} *)

 val compatible : ?c:bool -> ?h:t option -> ?t:(term -> term -> term m)
  -> trs -> t -> (term * State.t) list m
 (** [compatible ~c:c ~h:h ~t:t r a] computes all compatibility violations
 with respect to the TRS [r] and the tree automaton [a]. If [c] is set to true,
 the internal categorization is used. In that case it might happen that less
 violations are obtained because transitions are applied which would be
 in general not applicable. If [h] is specified, it is guaranteed that in each
 derivation at least one transition of [h], that is also contained in [a], is
 used. Via the function [t] it is possible to modify the right-hand sides
 involved in compatibility violations. As input the function [t] gets the left-
 and right-hand side of the considered rewrite rule (not that in case that [c]
 is true it might happen that the left-hand side has been changed). Note that
 as a side effect the old signature is changed too. Not tail-recursive. *)
 val paths : ?c:bool -> ?h:t option -> ?f:bool -> term -> t
  -> ((term * substitution) list * State.t) list m
 (** [paths ~c:c ~h:h ~f:f t a] returns all possible terms [u] (in general
 [u = t]), substitutions [s], and states [p] such that [us] rewrites to [p].
 If [c] is set to true, the internal categorization is used. In that case it
 might happen that [u] is not the same as [t]. If [h] is specified, it is
 guaranteed that in each derivation at least one transition of [h], that is
 also contained in [a], is used. Finaly, if [f] is set to true, only accepting
 sequences are taken into consideration. By default [c] and [f] are disabled
 and [h = None]. Not tail-recursive. *)

 (** {3 Rewrite Terms} *)

 val rewrite : ?c:bool -> term -> t -> term list m
 (** [rewrite ~c:c t a] returns all normal forms of [t] that have a minimal
 size. If [c] is set to true, the internal categorization is used. In that
 case it might happen that transitions are applied which would be in general
 not applicable (for instance because the root symbol of the left-hand side
 is not the same as the one of the redex). Not tail-recursive. *)
 val context : ?c:bool -> ?n:int -> term -> State.t -> t
  -> (term * State.t) list m
 (** [context ~c:c t p a] tries to rewrite the term [t] in a top-down manner
 to the state [p]. During each step [n] mismatches can occur. Per default
 [n = 1]. As soon as [p] is reached, a minimal list of mismatches is returned
 (minimal in the sense that the sum of the size of the Terms involved in the
 mismatches is minimal). If [c] is set to true, the internal categorization is
 used. In that case it might happen that transitions are applied which would be
 in general not applicable (for instance because the root symbol of the
 left-hand side is not the same as the one of the redex). Not tail-recursive. *)

 (** {3 Determinization} *)

 val quasi_det : ?c:bool -> t -> t m
 (** [quasi_det ~c:c a] makes [a] quasi-deterministic. If the flag [c] is
 set to true, a completely defined tree automaton is returned. Per default
 the [c] is set to false. Not tail-recursive. Note that as a side effect
 [a] as well as the old signature are changed. *)
 val det : ?c:bool -> ?q:bool -> t -> t m
 (** [det ?c:c a] makes [a] deterministic. If the flag [c] is set to true,
 a completely defined tree automaton is returned. The flag [q] specifies
 if the determinisation should be done by constructing an intermediate
 quasi-deterministic tree automaton or not. Per default both optional
 arguments are set to false. Not tail-recursive. Note that as a side effect
 [a] and the old signature are changed. *)

 (** {3 Predicates} *)

 val is_accepted : ?c:bool -> term -> t -> bool m
 (** [is_accepted ~c:c t a] checks if there is a term [u] (in general [u = t])
 and a substitution [s] such that [a] accepts [us]. If [c] is set to true, the
 internal categorization is used. In that case it might happen that [u] is not
 the same as [t]. By default [c = false]. Not tail-recursive. *)
 val is_empty : t -> bool m
 (** [is_empty t a] checks if the language of the automaton [a] is empty, i.e.,
 if the reduced version of [a] does not contain any accepting state (see
 function {!val: Automata.AUTOMATON.reduce}. Not tail-recursive. *)
 val is_reachable : ?c:bool -> ?h:t option -> term -> State.t -> t -> bool m
 (** [is_reachable ~c:c ~h:h t p a] checks if there is a term [u] (in general
 [u = t]) and a substitution [s] such that [us] rewrites to [p]. If [c] is set
 to true, the internal categorization is used. In that case it might happen
 that [u] is not the same as [t]. If [h] is specified, it is guaranteed that in
 each derivation at least one transition of [h], that is also contained in [a],
 is used. By default [c] is disabled and [h = None]. Not tail-recursive. *)

 (** {3 Search Functions} *)

 val find : ?c:bool -> lhs -> t -> rhs option m
 (** [find ~c:c l a] returns the right-hand side associated with [l]. If [c]
 is set to true, the internal categorization is used to find an appropriate
 right-hand side. In that case the first match is returned. By default this
 behaviour is disabled. If no transition with left-hand side [l] has been
 found, [None] is returned. *)

 (** {3 Scan Functions} *)

 val exists : (lhs -> rhs -> bool) -> t -> bool
 (** [add p a] checks if [a] contains a transition that satisfies [p]. *)
 val for_all : (lhs -> rhs -> bool) -> t -> bool
 (** [for_all p a] checks if all transitions of [a] satisfy [p]. *)
 val mem : ?c:bool -> lhs -> t -> bool m
 (** [mem ~c:c l a] checks if [a] contains a transitions with left-hand side
 [l]. If [c] is set to true, the internal categorization is used. Note that
 in that case the found left-hand side might be differ from [l]. By default
 [c = false].*)

 (** {3 Miscellaneous} *)

 val adapt : t -> t -> t m
 (** [adapt a b] removes all transitons of [a] that are not contained in [b].
 Note that as a side effect [a] is changed too. *)
 val analogies : t -> t -> (State.t * State.t) list m
 (** [analogies a b] computes all state pairs [(p,q)] such that there is a
 ground term [t] which can be rewritten to state [p] via [a] and to state
 [q] via [b]. *)
 val combine : t -> t -> t m
 (** [combine a b] adds all transitions of [b] to [a]. Note that as a side
 effect [a] is changed too. *)
 val copy : t -> t
 (** [copy a] returns a copy of [a]. *)
 val inter : t -> t -> t m
 (** [inter a b] computes the intersection of [a] and [b]. This function is
 not tail-recursive. *)
 val minus : t -> t -> t m
 (** [minus a b] removes all transitons of [a] that are contained in [b]. Note
 that as a side effect [a] is changed too. *)
 val restrict : Rewriting.Function.t list -> t -> t m
 (** [restrict fs a] restricts [a] to the signature given by the function
 symbols [fs]. Note that as a side effect [a] is changed too. *)
 val reduce : t -> t m
 (** [reduce a] removes all inaccessible states (and by that transitions) of
 [a]. Note that as a side effect [a] is changed too. *)
 val size : t -> int
 (** [size a] returns the number of transitions of [a]. *)
 val union : t -> t -> t m
 (** [union a b] computes the union of [a] and [b]. To ensure that result is
 correct it must be guranteed that the states of [a] and [b] are disjoint.
 Note that as a side effect [a] is changed too. *)

 (** {3 Parsers} *)

 val of_string : string list -> string -> t m
 (** [of_string ps s] applies {!val: Automata.AUTOMATON.parse} to the input
 string [s] and lifts the result into a state monad. Note that as a side effect
 the old signature is changed too. *)
 val parse : string list -> t p
 (** [parse ps] takes a list of strings (the identifiers that should be parsed
 as states) and returns an automaton-parser (having a signature as well as some
 status as internal state). *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt a] prints [a] using the [OCaml] module [Format]. *)
 val fprintfm : Format.formatter -> t -> unit m
 (** [fprintfm fmt a] prints [a] using the [OCaml] module [Format]. This
 function uses the information stored in the underlying signature to print
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is printed. Note that as a side
 effect copies of the underlying signature are changed too. This function is
 not tail-recursive. *)
 val to_string : t -> string
 (** [to_string a] returns a formatted string that represents [a]. *)
 val to_stringm : t -> string m
 (** [to_stringm a] returns a formatted string that represents [a]. This
 function uses the information stored in the underlying signature to represent
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is used. Note that as a side effect
 copies of the underlying signature are changed too. This function is not
 tail-recursive. *)
end

(** Tree Automata
@author Martin Korp
@since  Thu Sep  3 11:03:31 CEST 2009 *)

(** This module defines tree automata. In order to obtain an efficient
representation of tree automata we group transitions according to their
left-hand side. So, a right-hand side of a transition stored in the
automaton defines all states to which the corresponding left-hand side
can be reduced. *)
module Automaton : AUTOMATON with
 type 'a m = 'a Monad.t and type 'a p = 'a Parser.t and type lhs = Lhs.t and
 type rhs = Rhs.t and type substitution = Substitution.t and
 type term = Term.t and type trs = Rewriting.Trs.t

(** {2 Module Transducer} *)

module type TRANSDUCER = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type automaton
 type t

 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val fst : t -> automaton
 (** [fst t] returns the first automaton of the transducer [t]. *)
 val invert : t -> t
 (** [invert t] computes the inverse of the relation induced by the transducer
 [t]. *)
 val make : automaton -> automaton -> t
 (** [make a b] constructs a tree transducer consisting of the tree automata
 [a] and [b]. *)
 val snd : t -> automaton
 (** [snd t] returns the second automaton of the transducer [t]. *)
 val transitive_closure : t -> t m
 (** [transitive_closure t] computes the transitive closure of the transducer
 [t]. Not tail-recursive. Note that as a side effect [t] is changed too. *)

 (** {3 Iterators} *)

 val apply : (automaton -> 'a) -> (automaton -> 'b) -> t -> 'a * 'b
 (** [apply f g t] applies the function [f] to the first and [g] to the
 second automaton of [t], i.e., if [t = (a,b)] then the result [(f a,g b)]
 is returned. *)
 val fold : (automaton -> 'a -> 'a) -> 'a -> t -> 'a
 (** [fold f d t] combines the first and second automaton of [t] using the
 function [f]. *)
 val map : (automaton -> 'a) -> t -> 'a * 'a
 (** [map f t] is equivalent to [apply f f t]. *)
 val project : (automaton -> automaton) -> t -> t
 (** [project f t] applies the function [f] to both tree automata of [t]. *)
 val uncurry : (automaton -> automaton -> 'a) -> t -> 'a
 (** [uncurry f t] applies [f] to both tree automata of the transducer [t].
 I.e., if [t = (a,b)] then the result [f a b] is returned. *)

 (** {3 Miscellaneous} *)

 val copy : t -> t
 (** [copy t] returns a copy of [t]. *)
 val size : t -> int
 (** [size t] returns the number of transitions of [t]. *)
 val successors : automaton -> t -> automaton m
 (** [successors a t] closes the language of the tree automaton [a] under
 the relation induced by the transducer [t]. So the resulting automaton
 accepts all terms that are either accepted by [a] or obtained from terms
 in the language of [a] by applying a [t]-steps. To ensure that result is
 correct it must be guranteed that the states of [a] and [t] are disjoint.
 Note that as a side effect [a] is changed too. This function is not
 tail-recursive. *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt t] prints [t] using the [OCaml] module [Format]. *)
 val fprintfm : Format.formatter -> t -> unit m
 (** [fprintfm fmt t] prints [t] using the [OCaml] module [Format]. This
 function uses the information stored in the underlying signature to print
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is printed. Note that as a side
 effect copies of the underlying signature are changed too. This function is
 not tail-recursive. *)
 val to_string : t -> string
 (** [to_string t] returns a formatted string that represents [t]. *)
 val to_stringm : t -> string m
 (** [to_stringm t] returns a formatted string that represents [t]. This
 function uses the information stored in the underlying signature to represent
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is used. Note that as a side effect
 copies of the underlying signature are changed too. This function is not
 tail-recursive. *)
end

(** Tree Transducer
@author Martin Korp
@since  Thu Apr 29 11:03:31 CEST 2010 *)

(** This module defines tree transducers. The module provides all functions
needed to construct the set of all terms reachable from the terms in a given
regular language, defined via a tree automaton, with respect to the relation
induced by the given tree transducer. *)
module Transducer : TRANSDUCER with
 type 'a m = 'a Monad.t and type automaton = Automaton.t

(** {2 Module Path} *)

module type PATH = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type automaton
 type lhs
 type term

 (*** VALUES ******************************************************************)
 (** {3 Establish Fresh Paths} *)

 val fresh_min : ?l:bool -> term -> State.t -> automaton -> automaton m
 (** [fresh_min ~l:l t p a] establish a path from [t] to [p] by using one
 fresh state. For instance for [t = f(g(1),g(a))] and [p = 2] the transitions
 [a -> 3], [g(1) -> 3], [g(3) -> 3], and [f(3,3) -> 2] are added. If [l] is
 set to true the whole path construction is lifted which means that each state
 in [t] as well as [p] is replaced by a fresh state. Afterwards epsilon
 transitions are added to ensure that [t] rewrites to [p]. Note that as a side
 effect the old signature is changed too. *)
 val fresh_funs : ?l:bool -> (Rewriting.Function.t -> bool)
  -> term -> State.t -> automaton -> automaton m
 (** [fresh_funs ~l:l p t q a] establish a path from [t] to [q] by introducing
 a fresh state for all functions symbols in [t] that satisfy [p]. For instance
 for [t = f(g(1),g(a))] and [q = 2] the transitions [a -> 3], [g(1) -> 4],
 [g(3) -> 4], and [f(4,4) -> 2] are added in case that [a] satisfies [p]. If
 [l] is set to true the whole path construction is lifted which means that each
 state in [t] as well as [q] is replaced by a fresh state. Afterwards epsilon
 transitions are added to ensure that [t] rewrites to [q]. Note that as a side
 effect the old signature is changed too. *)
 val fresh_reuse : ?f:(lhs -> automaton -> State.t m) -> ?l:bool
  -> term -> State.t -> automaton -> automaton m
 (** [fresh_reuse ~f:f ~l:l s p a] establish a path from [s] to [q] by reusing
 states from the current automaton using the function [f]. For instance for
 [s = f(g(1),g(a))], [q = 2], and [a = {g(1) -> 3}] the transitions [a -> 4],
 [g(4) -> 5], and [f(3,5) -> 2] are added if [f] defines that for the term
 [g(1)] the transition [g(1) -> 3] is reused. If [l] is set to true the whole
 path construction is lifted which means that each state in [s] as well as [p]
 is replaced by a fresh state. Afterwards epsilon transitions are added to
 ensure that [s] rewrites to [q]. Note that as a side effect the old signature
 is changed too. *)
 val fresh_max : ?l:bool -> term -> State.t -> automaton -> automaton m
 (** [fresh_max ~l:l t p a] establish a path from [t] to [q] by introducing
 a fresh state for all functions symbols in [t]. For instance for
 [t = f(g(1),g(a))] and [q = 2] the transitions [a -> 3], [g(1) -> 4],
 [g(3) -> 5], and [f(4,5) -> 2] are added. If [l] is set to true the whole
 path construction is lifted which means that each state in [t] as well as [p]
 is replaced by a fresh state. Afterwards epsilon transitions are added to
 ensure that [t] rewrites to [q]. Note that as a side effect the old signature
 is changed too. *)

 (** {3 Path Construction Strategies} *)

 val fresh : (term -> State.t -> automaton -> automaton m)
  -> term -> State.t -> automaton -> automaton m
 (** [fresh f t p a] establish a path from [t] to [p] by calling the function
 [f]. Note that as a side effect the old signature is changed too. *)
 val torpa : ?c:bool -> (term -> State.t -> automaton -> automaton m)
  -> term -> State.t -> automaton -> automaton m
 (** [torpa ~c:c f t p a] establish a path from [t] to [p] according to the
 strategy of Torpa. I.e., if [t] can be rewritten to some state [q] then the
 epsilon transition [q -> p] is added. Otherwise [f] is used to establish the
 path. With [c] it can be specified whether the internal categorization of [a]
 should be used to rewrite [t] or not. Per default [c = false]. Note that as a
 side effect the old signature is changed too. *)
 val suffix : ?p:bool -> ?c:bool
  -> (term -> State.t -> automaton -> automaton m)
  -> term -> State.t -> automaton -> automaton m
 (** [suffix ~p:p ~c:c f t p a] establish a path from [t] to [p] as follows:
 At first [t] is rewriting to some normal form [u]. Afterwards [f] is used to
 establish a path from [u] to [p]. With [p] it can be specified that only the
 subterms of [t] should be rewritten and with [c] it can be specified whether
 the internal categorization of [a] should be used to rewrite [t] or not. Per
 default both, [p] and [c] are disabled. Note that as a side effect the old
 signature is changed too. *)
 val tttbox : ?p:bool -> ?n:int -> ?c:bool
  -> (term -> State.t -> automaton -> automaton m)
  -> term -> State.t -> automaton -> automaton m
 (** [tttbox ~p:p ~n:n ~c:c f t p a]  establish a path from [t] to [p] as
 follows: At first [t] is rewriting to some normal form [u]. Afterwards
 the term [u] is rewritten in a top-down manner to the state [p] using
 the function {!val: Automata.AUTOMATON.context}. Afterwards, the function
 [f] is used to establish the returned mismatches. With [n] and [c] the
 behaviour of first two steps is controlled. Per default [n = 1] and [c] is
 set to false. With [p] it can be specified that only the subterms of [t]
 should be rewritten. Per default [p] is disabled. Note that as a side effect
 the old signature is changed too. *)
 val ttt2 : ?p:bool -> ?n:int -> ?c:bool
  -> (term -> State.t -> automaton -> automaton m)
  -> term -> State.t -> automaton -> automaton m
 (** [ttt2 ~p:p ~n:n ~c:c f t p a] behaves similar as
 {!val: Automata.PATH.tttbox} except that the first two steps are switched.
 Note that as a side effect the old signature is changed too. *)
end

(** Construct Paths
@author Martin Korp
@since  Thu Sep  3 11:03:31 CEST 2009 *)

(** This module provides several functions that are used to establish new
paths. There are several ways to do this, ranging from establishing a
completely new path to adding as few as possible new transitions by reusing
transitions from the current automaton. The functions starting with the
prefix fresh establish a completely new path whereas the latter ones try
to reuse transitions according to some heuristics. *)
module Path : PATH with
 type 'a m = 'a Monad.t and type automaton = Automaton.t and
 type lhs = Lhs.t and type term = Term.t

(** {2 Module Initial} *)

module type INITIAL = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type automaton
 type term
 type trs

 (*** VALUES ******************************************************************)
 val custom : Rewriting.Function.t list -> Rewriting.Function.t list
  -> Rewriting.Function.t list -> automaton m
 (** [custom fs gs hs] creates a tree automaton that accepts all ground
 terms over the signature defined by [gs] and [hs] that have as root symbols
 function symbols contained in [fs]. To this end each function symbol in [gs]
 is associated with a fresh state whereas one fresh state is taken for all
 function symbols in [hs]. Note that as a side effect the old signature is
 changed too. *)
 val individual : Rewriting.Function.t list -> Rewriting.Function.t list
  -> automaton m
 (** [individual fs gs] creates a tree automaton that accepts all ground terms
 over the signature defined by [fs] and [gs]. To this end each function symbol
 in [fs] is associated with a fresh state whereas one fresh state is taken for
 all function symbols in [gs]. Note that as a side effect the old signature is
 changed too. *)
 val instances : (term -> State.t list m)
  -> (term -> State.t -> automaton -> automaton m)
  -> Rewriting.Function.t list -> Rewriting.Function.t list
  -> term list -> automaton m
 (** [instances f ts fs gs] creates a tree automaton that accepts all ground
 instances of terms in [ts] over the signature defined by [fs] and [gs]. To
 this end each function symbol in [fs] is associated with a fresh state whereas
 one fresh state is taken for all function symbols in [gs]. Via the function
 [f] it can be controled how the terms in [ts] are modeled. Note that as a side
 effect the old signature is changed too. *)
 val min : Rewriting.Function.t list -> automaton m
 (** [min fs] is equivalent to [individual [] fs]. Note that as a side effect
 the old signature is changed too. *)
 val normal : Rewriting.Function.t list -> trs -> automaton m
 (** [normal fs r] creates a tree automaton that over-approximates the normal
 forms over the signature [fs] with respect to the TRS. The over-approximation
 is achieved by ignoring all non-left-linear rewrite rules. Note that as a side
 effect the old signature is changed too. *)
 val specific : (term -> State.t list m)
  -> (term -> State.t -> automaton -> automaton m) -> term list -> automaton m
 (** [specific f g ts] creates a tree automaton that accepts all terms in
 [ts]. Via the function [f] and [g] it can be controled how the terms in [ts]
 are modeled. I.e., for each term [t] in [ts] the function [f] specifies the
 accepting state for the term [t] and the function [g] controles the
 transitions that are added to the underlying automaton to guarantee that [t]
 rewrites to [f t]. If one of the terms in [ts] contains a variable the
 returned monad represents the failure "variable occured". *)
 val split : Rewriting.Function.t list -> automaton m
 (** [split fs] is equivalent to [individual fs []]. Note that as a side effect
 the old signature is changed too. *)
end

(** Initial Automata
@author Martin Korp
@since  Thu Sep  3 11:03:31 CEST 2009 *)

(** This module provides various functions which construct tree automata that
accept either all ground terms over some given signature or a specific set of
terms. *)
module Initial : INITIAL with
 type 'a m = 'a Monad.t and type automaton = Automaton.t and
 type term = Term.t and type trs = Rewriting.Trs.t

(** {2 Module Reachability} *)

module type REACHABILITY = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type automaton
 type trs

 (*** VALUES ******************************************************************)
 val predecessors : Rewriting.Function.t list -> trs -> automaton
  -> automaton m
 (** [predecessors fs r a] closes the tree automaton [a] under backward
 rewriting with respect to the TRS [r]. So the returned tree automaton
 accepts all ground terms that are either accepted by the tree automaton
 [a] or reachable from some term in [a] by performing backward steps using
 the rewrite rules in [r]. Here [fs] defines the signature of the TRS [r].
 If [r] is not a growing TRS the returned monad represents the failure 
 "illegal TRS". Keep in mind that as a side effect [a] is changed if the
 function is successful. Furthermore, this function can be also used to
 compute the successors of a language. Just invert the rules of [r].
 Note that [r] does not have to satify the general variable conditions. *)
 val successors : Rewriting.Function.t list -> trs -> automaton -> automaton m
 (** [successors fs r a] closes the tree automaton [a] under (forward)
 rewriting with respect to the TRS [r]. So the returned tree automaton
 accepts all ground terms that are either accepted by the tree automaton
 [a] or reachable from some term in [a] by performing [r]-steps. Here [fs]
 defines the signature of the TRS [r]. If [r] consists of a rewrite rules
 such that the variables of the left- and right-hand side are not disjoint,
 the returned monad represents the error "illegal TRS". Keep in mind that as
 a side effect [a] is changed. Furthermore, this function can be also used to
 compute the predecessors of a language. Just invert the rules of [r]. Note
 that [r] does not have to satify the general variable conditions. *)
end

(** Reachability Analysis
@author Martin Korp
@since  Thu May  1 17:58:35 CEST 2010 *)

(** This module provides some basic functions to compute the successors
and predecessors of a regular language with respect to a given TRS. Since
thoes languages need not be regular the considered TRSs have to satisfy
certain properties. Details about this requirements are stated in the
description of each method. *)
module Reachability : REACHABILITY with
 type 'a m = 'a Monad.t and type automaton = Automaton.t and
 type trs = Rewriting.Trs.t

(** {2 Functor Make} *)

module type CATEGORIZATION = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type t

 (*** VALUES ******************************************************************)
 val add : Rewriting.Function.t -> t -> t m
 (** [add f c] adds the function symbol [f] to the categorization [c]. *)
 val create : int -> t
 (** [create n] creates a categorization with approximately [n] entries. *)
 val remove : Rewriting.Function.t -> t -> t m
 (** [remove f c] removes the function symbol [f] from the categorization
 [c]. *)
 val category : Rewriting.Function.t -> t -> Rewriting.Function.t list m
 (** [category f c] returns all function symbols that are in the same
 category as [f]. *)
 val find : Rewriting.Function.t -> t -> Rewriting.Function.t list m
 (** [find f c] returns all function symbols with respect to the
 categorization [c] that can be used instead of [f]. *)
 val funs : t -> Rewriting.Function.t list
 (** [funs c] returns all function symbols that have been added to the
 categorization [c]. *)
 val copy : t -> t
 (** [copy c] returns a copy of the categorization [c]. *)
 val compare : t -> t -> int
 (** [compare c d] compares [c] and [d]. This function defines a total
 ordering on categorizations. *)
 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt c] prints [c] using the [OCaml] module [Format]. *)
end

module type REWRITING = sig
 (*** MODULES *****************************************************************)
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

module type SIGNATURE = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type signature
 type term
 type trs

 (*** MODULES *****************************************************************)
 module State : STATE with type t = State.t
 module Status : STATUS with type t = Status.t

 module Monad : MONAD with
  type 'a m = 'a m and type state = signature * Status.t

 module Parser : Parsec.STATEFUL_CHAR_PARSER with
  type input = Parsec.StringInput.t and type state = signature * Status.t

 module Term : TERM with
  type 'a m = 'a Monad.t and type 'a p = 'a Parser.t and type term = term

 module Substitution : SUBSTITUTION with type term = Term.t

 module Lhs : LHS with
  type 'a m = 'a Monad.t and type 'a p = 'a Parser.t and type term = Term.t

 module Rhs : RHS

 module Automaton : AUTOMATON with
  type 'a m = 'a Monad.t and type 'a p = 'a Parser.t and
  type lhs = Lhs.t and type rhs = Rhs.t and
  type substitution = Substitution.t and type term = Term.t and type trs = trs

 module Transducer : TRANSDUCER with
  type 'a m = 'a Monad.t and type automaton = Automaton.t

 module Path : PATH with
  type 'a m = 'a Monad.t and type automaton = Automaton.t and
  type lhs = Lhs.t and type term = Term.t

 module Initial : INITIAL with
  type 'a m = 'a Monad.t and type automaton = Automaton.t and
  type term = Term.t and type trs = trs

 module Reachability : REACHABILITY with
  type 'a m = 'a Monad.t and type automaton = Automaton.t and type trs = trs
end

(** Generate Automata Library
@author Martin Korp
@since  Thu Sep  3 11:03:31 CEST 2009 *)

(** This functor builds a automata module based on a rewriting module [R] of
module type {!modtype: Automata.REWRITING}. *)
module Default (R : REWRITING) : SIGNATURE with
 type 'a m = 'a R.Monad.t and type signature = R.Signature.t and
 type term = R.Term.t and type trs = R.Trs.t

(** This functor builds a automata module based on a categorization module [C]
of module type {!modtype: Automata.CATEGORIZATION} and a rewriting module [R]
of module type {!modtype: Automata.REWRITING}. *)
module Make (R : REWRITING)
            (C : CATEGORIZATION with type 'a m = 'a R.Monad.t): SIGNATURE with
 type 'a m = 'a R.Monad.t and type signature = R.Signature.t and
 type term = R.Term.t and type trs = R.Trs.t
