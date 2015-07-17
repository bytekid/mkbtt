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

(** Rewriting Library
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Mon Dec  1 12:58:07 CET 2008 *)

(** Provides some basic functions that deal with the standard notions of
term rewriting. *)

(*** MODULES ******************************************************************)
(** {2 Module Function} *)

module type FUNCTION = sig
 (*** TYPES *******************************************************************)
 type t = int

 (*** VALUES ******************************************************************)
 (** {3 Miscellaneous} *)

 val copy : t -> t
 (** [copy f] returns a copy of [f]. *)
 val hash : t -> int
 (** [hash f] returns a hash value for [f]. It is guaranteed that
 [hash f = hash g] if and only if [compare f g = 0]. *)
 val next : t -> t
 (** [next f] returns the successor of [f] with respect to the compare
 function [compare]. *)
 val to_int : t -> int
 (** [to_int f] is equivalent to {!val: Rewriting.FUNCTION.hash}. *)
 val zero : t
 (** [zero] returns the smallest function with respect to the compare
 function [compare]. *)

 (** {3 Compare Functions} *)

 val compare : t -> t -> int
 (** [compare f g] compares [f] and [g]. This function defines a total
 ordering on function symbols. *)
 val equal : t -> t -> bool
 (** [equal f g] checks if [f] and [g] are equal. This function is equivalent
 to [compare f g = 0]. *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt f] prints [f] using the [OCaml] module [Format]. *)
 val to_string : t -> string
 (** [to_string f] returns a formatted string that represents [f]. *)
end

(** Function Symbols
@author Martin Korp
@since  Mon Sep  1 12:22:31 CEST 2008 *)

(** This module provides basic functions that deal with function symbols. *)
module Function : FUNCTION

(** {2 Module Variable} *)

module type VARIABLE = sig
 (*** TYPES *******************************************************************)
 type t

 (*** VALUES ******************************************************************)
 (** {3 Miscellaneous} *)

 val copy : t -> t
 (** [copy x] returns a copy of [x]. *)
 val hash : t -> int
 (** [hash x] returns a hash value for [x]. It is guaranteed that
 [hash x = hash y] if and only if [compare x y = 0]. *)
 val next : t -> t
 (** [next x] returns the successor of [x] with respect to the compare
 function [compare]. *)
 val to_int : t -> int
 (** [to_int x] is equivalent to {!val: Rewriting.VARIABLE.hash}. *)
 val zero : t
 (** [zero] returns the smallest variable with respect to the compare
 function [compare]. *)

 (** {3 Compare Functions} *)

 val compare : t -> t -> int
 (** [compare x y] compares [x] and [y]. This function defines a total
 ordering on variables. *)
 val equal : t -> t -> bool
 (** [equal x y] checks if [x] and [y] are equal. This function is equivalent
 to [compare x y = 0]. *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt x] prints [x] using the [OCaml] module [Format]. *)
 val to_string : t -> string
 (** [to_string x] returns a string that represents [x]. *)
end

(** Variables
@author Martin Korp
@since  Mon Sep  1 12:22:20 CEST 2008 *)

(** This module deals with variables. *)
module Variable : VARIABLE

(** {2 Module Signature} *)

module type STATE = sig
 (*** TYPES *******************************************************************)
 type t

 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val add_ari : Function.t -> int -> t -> t
 (** [add_ari f a s] sets the arity of the function symbol [f] to [a]. If
 the arity of [f] has already been set, the signature remains unchanged. Note
 that as a side effect the old signature is changed too. *)
 val add_fun : Function.t -> int -> string -> t -> t
 (** [add_fun f a n s] adds the function symbol [f] with arity [a] and name
 [n] to the signature [s]. If there exists already a binding for [f] or
 another function symbol with name [n], the signature remains unchanged. Note
 that as a side effect the old signature is changed too. *)
 val add_fun_name : Function.t -> string -> t -> t
 (** [add_fun f n s] sets the name of the function symbol [f] to [n]. If the
 name of [f] has already been specified or [n] is used for an other function
 symbol, the signature remains unchanged. Note that as a side effect the old
 signature is changed too. *)
 val add_var : Variable.t -> string -> t -> t
 (** [add_var x n s] adds the variable [x] with name [n] to the signature [s].
 If there exists already a binding for [x] or [n], then the signature remains
 unchanged. Note that as a side effect the old signature is changed too. *)
 val empty : int -> t
 (** [empty i] creates an empty signature which will store approximately [n]
 variables and functions. *)
 val replace_ari : Function.t -> int -> t -> t
 (** [replace_ari f a s] replaces the arity of the function symbol [f] by [a].
 Note that as a side effect the old signature is changed too. *)
 val replace_fun : Function.t -> int -> string -> t -> t
 (** [replace_fun f a n s] adds the function symbol [f] with arity [a] and
 name [n] to the signature [s]. If [s] contains already a binding for [f] or
 [n], it is replaced. Note that as a side effect the old signature is changed
 too. *)
 val replace_fun_name : Function.t -> string -> t -> t
 (** [replace_fun_name f n s] replaces the name of the function symbol [f] by
 [n]. If [s] contains already a binding for [n] it is replaced. Note that as
 a side effect the old signature is changed too. *)
 val replace_var : Variable.t -> string -> t -> t
 (** [replace_var x n s] adds the variable [x] with name [n] to the signature
 [s]. If [s] contains already a binding for [x] or [n] it is replaced. Note
 that as a side effect the old signature is changed too. *)

 (** {3 Fresh Symbols} *)

 val create_fun : int -> string -> t -> Function.t * t
 (** [create_fun a n s] returns a fresh [a]-ary function with name [n]. Note
 that this function symbol has been added to the signature. If [s] contains
 already a function symbol with name [n] and arity [a], this function symbol
 is returned. Note that as a side effect the old signature is changed too.
 @raise Failure "incorrect arity" if [s] contains already a function symbol
 with name [n] but different arity. *)
 val create_fun_name : Function.t -> t -> string * t
 (** [create_fun_name f s] returns the name of the function symbol [f]. If
 the name of [f] has not been defined so far, a randomly generated name is
 created and registered in the signature. Note that as a side effect the old
 signature is changed too. *)
 val create_var : string -> t -> Variable.t * t
 (** [create_var n s] returns a fresh variable with name [n]. Note that this
 symbol has been added to the signature. If [s] contains already a variable
 with name [n], this symbol is returned. Note that as a side effect the old
 signature is changed too. *)
 val create_var_name : Variable.t -> t -> string * t
 (** [create_var_name x s] returns the name of the variable [x]. If the name
 of [x] has not been defined so far, a randomly generated name is created and
 registered in the signature. Note that as a side effect the old signature is
 changed too. *)
 val fresh_fun : t -> Function.t * t
 (** [fresh_fun s] returns a fresh function symbol. Note that this function
 symbol has not been added to the signature! So, both, its arity and name is
 undefined. Furthermore, as a side effect the old signature is changed too. *)
 val fresh_var : t -> Variable.t * t
 (** [fresh_var s] returns a fresh variable. Note that the returned variable
 has not been added to the signature! So, its name is undefined. Furthermore,
 as a side effect the old signature is changed too. *)

 (** {3 Search Functions} *)

 val find_ari : Function.t -> t -> int
 (** [find_ari f s] returns the arity of the function symbol [f] with respect
 to the signature [s].
 @raise Not_found if [s] does not contain a binding for [f]. *)
 val find_fun : string -> t -> Function.t
 (** [find_fun n s] returns the function that has name [n] with respect to
 the signature [s].
 @raise Not_found if [s] does not contain a binding for [n]. *)
 val find_fun_name : Function.t -> t -> string
 (** [find_fun_name f s] returns the name of the function symbol [f] with
 respect to the signature [s].
 @raise Not_found if [s] does not contain a binding for [f]. *)
 val find_var : string -> t -> Variable.t
 (** [find_var n s] returns the variable that has name [n] with respect to
 the signature [s].
 @raise Not_found if [s] does not contain a binding for [n]. *)
 val find_var_name : Variable.t -> t -> string
 (** [find_var_name x s] returns the name of the variable [x] with respect
 to the signature [s].
 @raise Not_found if [s] does not contain a binding for [x]. *)
 val var_names : t -> string list
 (** [var_names s] returns the names of all variables with respect
 to signature [s]. *)

 (** {3 Scan Functions} *)

 val is_ari : int -> Function.t -> t -> bool
 (** [is_ari n f s] checks if the arity of [f] is [n]. *)
 val is_defined_fun : Function.t -> t -> bool
 (** [is_defined_fun f s] checks if the arity and name of [f] is defined. *)
 val is_defined_var : Variable.t -> t -> bool
 (** [is_defined_var x s] checks if the name of [x] is defined. *)
 val mem_ari : Function.t -> t -> bool
 (** [mem_ari f s] checks if the arity of [f] is defined. *)
 val mem_fun : Function.t -> t -> bool
 (** [mem_fun f s] checks if the name of [f] is defined. *)
 val mem_fun_name : string -> t -> bool
 (** [mem_var_name n s] checks if there exists a function with name [n]. *)
 val mem_var : Variable.t -> t -> bool
 (** [mem_var x s] is equivalent to
 {!val: Rewriting.STATE.is_defined_var}. *)
 val mem_var_name : string -> t -> bool
 (** [mem_var_name n s] checks if there exists a variable with name [n]. *)

 (** {3 Miscellaneous} *)

 val copy : t -> t
 (** [copy s] returns a copy of [s]. *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt s] prints [s] using the [OCaml] module [Format]. *)
 val fprintf_fun : Format.formatter -> Function.t -> t -> t
 (** [fprintf_fun fmt f s] prints [f] using the [OCaml] module [Format].
 This function uses the information stored in the signature [s] to print
 function symbols. If the name of a function symbol is not defined, a 
 randomly generated name is printed. Note that as a side effect copies
 of [s] are changed too. Not tail-recursive. *)
 val fprintf_var : Format.formatter -> Variable.t -> t -> t
 (** [fprintf_var fmt x s] prints [x] using the [OCaml] module [Format].
 This function uses the information stored in the signature [s] to print
 variables. If the name of a variable is not defined, a randomly generated
 name is printed. Note that as a side effect copies of [s] are changed too. *)
 val to_string : t -> string
 (** [to_string s] returns a formatted string that represents [s]. *)
 val to_string_fun : Function.t -> t -> string * t
 (** [to_string_fun f s] returns a formatted string that represents [f].
 This function uses the information stored in the signature [s] to print
 function symbols. If the name of a function symbol is not defined, a 
 randomly generated name is printed. Note that as a side effect copies
 of [s] are changed too. Not tail-recursive. *)
 val to_string_var : Variable.t -> t -> string * t
 (** [to_string_var x s] returns a formatted string that represents [x].
 This function uses the information stored in the signature [s] to print
 variables. If the name of a variable is not defined, a randomly generated
 name is printed. Note that as a side effect copies of [s] are changed too. *)
end

(** Rewrite Signature
@author Martin Korp
@since  Mon Sep  1 12:22:43 CEST 2008 *)

(** This module describes the signature of Terms, Rules and TRSs. *)
module Signature : STATE

(** {2 Module Monad} *)

module type MONAD = sig
 (*** TYPES *******************************************************************)
 type error = string
 type state

 (*** INCLUDES ****************************************************************)
 include Util.Monad.SIGNATURE

 (*** VALUES ******************************************************************)
 (** {3 Access Functions} *)

 val ap_comb : ((error,'a * state) Util.either
  -> (error,'b * state) Util.either) t -> 'a t -> 'b t
 (** [ap_comb m n] applies the argument of [m] to the return value of [n]. *)
 val map_comb : ((error,'a * state) Util.either
  -> (error,'b * state) Util.either) -> 'a t -> 'b t
 (** [map_comb f m] promotes the function [f] to the return value of [m]
 where [m] has the type ['a t]. In order to obtain map functions with a
 higher arity use {!val: Rewriting.MONAD.ap_comb}. *)

 (** {3 State Modifications} *)

 val adopt : (state -> 'a * state) -> 'a t
 (** [adopt f] is equivalent to
 {!val: Util.Monad.Transformer.STATE_MONAD.adopt}. *)
 val get : state t
 (** [get] is equivalent to {!val: Util.Monad.Transformer.STATE_MONAD.get}. *) 
 val modify : (state -> state) -> state t
 (** [modify f] is equivalent to
 {!val: Util.Monad.Transformer.STATE_MONAD.modify}. *)
 val set : state -> unit t
 (** [set s] is equivalent to {!val: Util.Monad.Transformer.STATE_MONAD.set}. *)
 val update : (state -> state) -> unit t
 (** [update f] is equivalent to
 {!val: Util.Monad.Transformer.STATE_MONAD.update}. *)
 val with_state : (state -> state) -> 'a t -> 'a t
 (** [with_state f m] is equivalent to
 {!val: Util.Monad.Transformer.STATE_MONAD.with_state}. *)

 (** {3 Error Handling} *)

 val catch : (error -> 'a t) -> 'a t -> 'a t
 (** [catch h m] is equivalent to
 {!val: Util.Monad.Transformer.ERROR_MONAD.catch}. *)
 val fail : error -> 'a t
 (** [fail e] is equivalent to
 {!val: Util.Monad.Transformer.ERROR_MONAD.fail}. *)

 (** {3 Evaluation Functions} *)

 val eval : state -> 'a t -> (error,'a * state) Util.either
 (** [eval s m] evaluates [m] under state [s] and returns the resulting
 value and state. If an error occurred during the computation, the corresponding
 error message is returned. *)
 val execute : state -> 'a t -> (error,state) Util.either
 (** [execute s m] evaluates [m] under state [s] and returns the resulting
 state. If an error occurred during the computation, the corresponding error
 message is returned. *)
 val run : state -> 'a t -> (error,'a) Util.either
 (** [run s m] evaluates [m] under state [s] and returns the resulting value.
 If an error occurred during the computation, the corresponding error message is
 returned. *)

 (** {3 Constructors and Destructors} *)

 val add_ari : Function.t -> int -> unit t
 (** [add_ari f a] is equivalent to {!val: Rewriting.STATE.add_ari} except
 that the signature is encapsulated in a monad. Note that as a side effect
 the old signature is changed too. *)
 val add_fun : Function.t -> int -> string -> unit t
 (** [add_fun f a n] is equivalent to {!val: Rewriting.STATE.add_fun} except
 that the signature is encapsulated in a monad. Note that as a side effect
 the old signature is changed too. *)
 val add_fun_name : Function.t -> string -> unit t
 (** [add_fun f n] is equivalent to {!val: Rewriting.STATE.add_fun_name}
 except that the signature is encapsulated in a monad. Note that as a side
 effect the old signature is changed too. *)
 val add_var : Variable.t -> string -> unit t
 (** [add_var x n] is equivalent to {!val: Rewriting.STATE.add_var} except
 that the signature is encapsulated in a monad. Note that as a side effect
 the old signature is changed too.*)
 val replace_ari : Function.t -> int -> unit t
 (** [replace_ari f a] is equivalent to {!val: Rewriting.STATE.replace_ari}
 except that the signature is encapsulated in a monad. Note that as a side
 effect the old signature is changed too. *)
 val replace_fun : Function.t -> int -> string -> unit t
 (** [replace_fun f a n] is equivalent to {!val: Rewriting.STATE.replace_fun}
 except that the signature is encapsulated in a monad. Note that as a side
 effect the old signature is changed too. *)
 val replace_fun_name : Function.t -> string -> unit t
 (** [replace_fun_name f n] is equivalent to
 {!val: Rewriting.STATE.replace_fun_name} except that the signature is
 encapsulated in a monad. Note that as a side effect the old signature
 is changed too. *)
 val replace_var : Variable.t -> string -> unit t
 (** [replace_var x n] is equivalent to {!val: Rewriting.STATE.replace_var}
 except that the signature is encapsulated in a monad. Note that as a side
 effect the old signature is changed too. *)

 (** {3 Fresh Symbols} *)

 val create_fun : int -> string -> Function.t t
 (** [create_fun a n] is equivalent to {!val: Rewriting.STATE.create_fun}
 except that the result is encapsulated in a monad. If there exists
 already a function symbol with name [n] but different arity, the returned
 monad represents the error "incorrect arity". Note that as a side effect
 the old signature is changed too. *)
 val create_fun_name : Function.t -> string t
 (** [create_fun_name f] is equivalent to
 {!val: Rewriting.STATE.create_fun_name} except that the result is
 encapsulated in a monad. Note that as a side effect the old signature
 is changed too. *)
 val create_var : string -> Variable.t t
 (** [create_var n] is equivalent to {!val: Rewriting.STATE.create_var}
 except that the result is encapsulated in a monad. Note that as a side
 effect the old signature is changed too. *)
 val create_var_name : Variable.t -> string t
 (** [create_var_name x] is equivalent to
 {!val: Rewriting.STATE.create_var_name} except that the result is
 encapsulated in a monad. Note that as a side effect the old signature
 is changed too. *)
 val fresh_fun : Function.t t
 (** [fresh_fun] is equivalent to {!val: Rewriting.STATE.fresh_fun}
 except that the result is encapsulated in a monad. Note that as a side
 effect the old signature is changed too. *)
 val fresh_var : Variable.t t
 (** [fresh_var] is equivalent to {!val: Rewriting.STATE.fresh_var}
 except that the result is encapsulated in a monad. Note that as a side
 effect the old signature is changed too. *)

 (** {3 Search Functions} *)

 val find_ari : Function.t -> int t
 (** [find_ari f] is equivalent to {!val: Rewriting.STATE.find_ari}
 except that the result is encapsulated in a monad. If the arity
 of [f] has not been specified, the returned monad represents the error
 "not found". *)
 val find_fun : string -> Function.t t
 (** [find_fun n] is equivalent to {!val: Rewriting.STATE.find_fun}
 except that the result is encapsulated in a monad. If there is no
 function symbol with name [n], the returned monad represents the error
 "not found". *)
 val find_fun_name : Function.t -> string t
 (** [find_fun_name f] is equivalent to {!val: Rewriting.STATE.find_fun_name}
 except that the result is encapsulated in a monad. If the name of [f] has
 not been specified, the returned monad represents the error "not found". *)
 val find_var : string -> Variable.t t
 (** [find_var n] is equivalent to {!val: Rewriting.STATE.find_var}
 except that the result is encapsulated in a monad. If there is
 no variable with name [n], the returned monad represents the error
 "not found". *)
 val find_var_name : Variable.t -> string t
 (** [find_var_name x] is equivalent to {!val: Rewriting.STATE.find_var_name}
 except that the result is encapsulated in a monad. If the name of [x] has
 not been specified, the returned monad represents the error "not found". *)

 (** {3 Scan Functions} *)

 val is_ari : int -> Function.t -> bool t
 (** [is_ari n f]  is equivalent to {!val: Rewriting.STATE.is_ari}
 except that the result is encapsulated in a monad. *)
 val is_defined_fun : Function.t -> bool t
 (** [is_defined_fun f] is equivalent to {!val: Rewriting.STATE.is_defined_fun}
 except that the result is encapsulated in a monad. *)
 val is_defined_var : Variable.t -> bool t
 (** [is_defined_var f] is equivalent to {!val: Rewriting.STATE.is_defined_var}
 except that the result is encapsulated in a monad. *)
 val mem_ari : Function.t -> bool t
 (** [mem_ari f] is equivalent to {!val: Rewriting.STATE.mem_ari} except that
 the result is encapsulated in a monad. *)
 val mem_fun : Function.t -> bool t
 (** [mem_fun f] is equivalent to {!val: Rewriting.STATE.mem_fun}
 except that the result is encapsulated in a monad. *)
 val mem_fun_name : string -> bool t
 (** [mem_fun_name f] is equivalent to {!val: Rewriting.STATE.mem_fun_name}
 except that the result is encapsulated in a monad. *)
 val mem_var : Variable.t -> bool t
 (** [mem_var x] is equivalent to {!val: Rewriting.STATE.mem_var}
 except that the result is encapsulated in a monad. *)
 val mem_var_name : string -> bool t
 (** [mem_var_name n] is equivalent to {!val: Rewriting.STATE.mem_var_name}
 except that the result is encapsulated in a monad. *)

 (** {3 Printers} *)

 val fprintf_fun : Format.formatter -> Function.t -> unit t
 (** [fprintf_fun fmt f] is equivalent to {!val: Rewriting.STATE.fprintf_fun}
 except that the signature is encapsulated in a monad. Note that as a side
 effect the old signature is changed too. Not tail-recursive. *)
 val fprintf_var : Format.formatter -> Variable.t -> unit t
 (** [fprintf_var fmt x] is equivalent to {!val: Rewriting.STATE.fprintf_var}
 except that the signature is encapsulated in a monad. Note that as a side
 effect the old signature is changed too. *)
 val to_string_fun : Function.t -> string t
 (** [to_string_fun f] is equivalent to {!val: Rewriting.STATE.to_string_fun}
 except that the signature is encapsulated in a monad. Note that as a side
 effect the old signature is changed too. Not tail-recursive. *)
 val to_string_var : Variable.t -> string t
 (** [to_string_var x] is equivalent to {!val: Rewriting.STATE.to_string_var}
 except that the signature is encapsulated in a monad. Note that as a side
 effect the old signature is changed too. *)
end

(** State Error Monad
@author Martin Korp
@since  Mon Sep  1 12:22:43 CEST 2008 *)

(** This module defines a state error monad that is used to encapsulate the
signature from the functions provided by the modules within this library. *)
module Monad : MONAD with type state = Signature.t

(** {2 Module Position} *)

module type POSITION = sig
 (*** TYPES *******************************************************************)
 type t

 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)
 
 val add_first : int -> t -> t
 (** [add_first i p] creates the position [ip]. *)
 val add_last : int -> t -> t
 (** [add_last i p] creates the position [pi]. Not tail-recursive. *)
 val root : t
 (** [root] represents the root position. *)
 val head : t -> int
 (** [head p] returns the first number of the position [p], i.e., if [p = iq]
 then [i] is returned.
 @raise Failure "root position" if [p] is the root position. *)
 val init : t -> t
 (** [init p] returns the position [p] without the last number, i.e., if
 [p = qi] then the position [q] is returned.
 @raise Failure "root position" if [p] is the root position. *)
 val last : t -> int
 (** [last p] returns the last number of the position [p], i.e., if [p = qi]
 then [i] is returned.
 @raise Failure "root position" if [p] is the root position. *)
 val make : int -> t
 (** [make i] creates the position [i]. *)
 val of_list : int list -> t
 (** [of_list l] constructs the position [i_1...i_n] where the list
 [l = \[i_1;...;i_n\]]. *)
 val split_first : t -> int * t
 (** [split_first p] returns the pair [(i,q)] where [p = iq].
 @raise Failure "root position" if [p] is the root position. *)
 val split_last : t -> t * int
 (** [split_last p] returns the pair [(q,i)] where [p = qi].
 @raise Failure "root position" if [p] is the root position. *)
 val tail : t -> t
 (** [tail p] returns the position [p] without the first number, i.e., if
 [p = iq] then the position [q] is returned.
 @raise Failure "root position" if [p] is the root position. *)
 val to_list : t -> int list
 (** [to_list p] returns [p] as a list. *)

 (** {3 Properties} *)
 
 val (||) : t -> t -> bool
 (** [p || q] checks if [p] and [q] are parallel positions. *)
 val (<=) : t -> t -> bool
 (** [p <= q] checks if [p] is a prefix of [q]. *)
 val (<) : t -> t -> bool
 (** [p < q] checks if [p] is a proper prefix of [q]. *)
 val (>=) : t -> t -> bool
 (** [p >= q] is equivalent to [q <= p]. *)
 val (>) : t -> t -> bool
 (** [p > q] is equivalent to [q < p]. *)
 val is_root : t -> bool
 (** [is_root p] checks if [p] is the root position. *)
 val are_parallel : t -> t -> bool
 (** [are_parallel p q] is equivalent to {!val: Rewriting.POSITION.(||)}. *)
 val is_prefix : t -> t -> bool
 (** [is_prefix p q] is equivalent to {!val: Rewriting.POSITION.(<=)}. *)
 val is_proper_prefix : t -> t -> bool
 (** [is_proper_prefix p q] is equivalent to {!val: Rewriting.POSITION.(<)}. *)
 val is_proper_suffix : t -> t -> bool
 (** [is_proper_suffix p q] checks if [p] is a proper suffix of [q]. *)
 val is_suffix : t -> t -> bool
 (** [is_suffix p q] checks if [p] is a suffix of [q]. *)

 (** {3 Miscellaneous} *)

 val append : t -> t -> t
 (** [append p q] concatenates the positions [p] and [q]. The returned position
 is [pq]. This function is not tail-recursive. *)
 val copy : t -> t
 (** [copy p] returns a copy of [p]. *)
 val hash : t -> int
 (** [hash p] returns the hash value of [p]. It is guaranteed that
 [hash p = hash q] if [compare p q = 0]. *)
 val length : t -> int
 (** [length p] returns the length of the position [p]. *)

 (** {3 Compare Functions} *)
 
 val compare : t -> t -> int
 (** [compare p q] compares [p] and [q]. This function defines a total
 ordering on positions. *)
 val equal : t -> t -> bool
 (** [equal p q] checks if [p] and [q] are equal. This function is equivalent
 to [compare p q = 0]. *)
 
 (** {3 Printers} *)
 
 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt p] prints [p] using the [OCaml] module [Format]. *)
 val to_string : t -> string
 (** [to_string p] returns a formatted string that represents [p]. *)
end

(** Term Positions
@author Martin Korp
@since  Mon Sep  1 12:21:54 CEST 2008 *)

(** This module provides basic functions that deal with term positions. Note
that the first argument of a term has position [0] and not [1]. *)
module Position : POSITION

(** {2 Module Parser} *)
module type PARSER = sig
  include Parsec.STATEFUL_CHAR_PARSER
  module Xml : Parsec.Xml.TRANSFORMER with type state = state
end
(** Parser
@author Christian Sternagel
@since  Mon Sep  1 12:21:54 CEST 2008 *)

(** This module defines the basic setting needed to parse terms, rules
and TRSs in a functional way. *)
module Parser : PARSER
  with type state = Signature.t and type input = Parsec.StringInput.t

(** {2 Module Term} *)

module type TERM = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type 'a p
 type 'a x
 type state
 type t = Var of Variable.t | Fun of Function.t * t list

 (*** VALUES ******************************************************************)
 (** {3 Constructors} *)

 val make_fun : Function.t -> t list -> t
 (** [make_fun f ts] returns the term [Fun (f,ts)]. *)
 val make_var : Variable.t -> t
 (** [make_var x] returns the term [Var x]. *)
 val reflect : t -> t
 (** [reflect t] reverses the arguments of all functions in [t]. This function
 is not tail-recursive. *)
 val replace : Position.t -> t -> t -> t
 (** [replace p s t] replaces the subterm of [t] at position [p] by [s].
 This function is not tail-recursive.
 @raise Failure "illegal position" if [p] is not a valid position for [t]. *)
 val reverse : t -> t
 (** [reverse t] reverses the string [t]. Note that a string is a term build
 of unary function symbols. This function is not tail-recursive.
 @raise Failure "not a string" if [t] is not a string. *)
 val subterm : Position.t -> t -> t
 (** [subterm p t] returns the subterm of [t] at position [p].
 @raise Failure "illegal position" if [p] is not a valid position for [t]. *)

 (** {3 Monadic Constructors} *)

 val ren : ?p:(Position.t -> Variable.t -> bool) -> t -> t m
 (** [ren ~p:p t] renames all variables of the term [t] that satisfy the
 predicate [p]. Per default, all variables are renamed. This function is
 not tail-recursive. Note that as a side effect the underlying signature
 changes all over. *)
 val rename : t -> t m
 (** [rename t] renames all variables of the term [t] via a renaming. This
 function is not tail-recursive. Note that as a side effect the underlying
 signature changes all over. *)

 (** {3 Term Symbols} *)

 val cons : t -> Function.t list
 (** [cons t] returns all constants of the term [t]. This function is not
 tail-recursive. *)
 val funs : t -> Function.t list
 (** [funs t] returns all function symbols of the term [t]. This function is
 not tail-recursive. *)
 val root : t -> Function.t option
 (** [root t] returns the root symbol of [t]. *)
 val symbols : t -> (Variable.t,Function.t) Util.either list
 (** [symbols t] returns all symbols of the term [t]. This function is not
 tail-recursive. *)
 val vars : t -> Variable.t list
 (** [vars t] returns all variables of the term [t]. This function is not
 tail-recursive. *)

 (** {3 Properties} *)

 val is_build : Function.t list -> t -> bool
 (** [is_build fs t] checks if all function symbols of [t] occur in [fs]. *)
 val is_cons : t -> bool
 (** [is_cons t] checks if [t] is a constant. *)
 val is_dummy : t -> bool
 (** [is_dummy t] checks if [t] is term consisting only of unary function
 symbols and constants. *)
 val is_embedded : t -> t -> bool
 (** [is_embedded s t] checks if [s] is embedded in the term [t]. *)
 val is_flat : t -> bool
 (** [is_flat t] checks if [t] is a flat term, i.e., a term with depth smaller
 or equal [1]. *)
 val is_fun : t -> bool
 (** [is_fun t] checks if [t] is a function. *)
 val is_ground : t -> bool
 (** [is_ground t] checks if [t] is a ground term, i.e., a term that does not
 contain any variables. *)
 val is_linear : t -> bool
 (** [is_linear t] checks if [t] is linear. A term [t] is said to be linear
 if all variables occur at most once. This function is not tail-recursive. *)
 val is_proper_subterm : t -> t -> bool
 (** [is_proper_subterm s t] check if [s] is a proper subterm of [t]. *)
 val is_shallow : t -> bool
 (** [is_shallow t] checks if the term [t] is shallow. Note that a term [t] is
 shallow if each variable occurs at most at depth [1]. *)
 val is_string : t -> bool
 (** [is_string t] checks if all function symbols of [t] have arity [1]. *)
 val is_subterm : t -> t -> bool
 (** [is_subterm s t] check if [s] is a subterm of [t]. *)
 val is_var : t -> bool
 (** [is_var t] checks if [t] is a variable. *)

 (** {3 Search Functions} *)

 val mem_fun : Function.t -> t -> bool
 (** [mem_fun f t] checks if [f] occurs in [t]. *)
 val mem_var : Variable.t -> t -> bool
 (** [mem_var x t] checks if [x] occurs in [t]. *)

 (** {3 Positions} *)
 
 val fun_pos : Function.t -> t -> Position.t list
 (** [fun_pos f t] returns the positions of the function [f] with respect to
 the term [t]. This function is not tail-recursive. *)
 val funs_pos : t -> Position.t list
 (** [funs_pos t] returns the positions of all functions of the term [t]. This
 function is not tail-recursive. *)
 val pos : t -> Position.t list
 (** [pos t] returns all positions of the term [t]. This function is not
 tail-recursive. *)
 val subterm_pos : t -> t -> Position.t list
 (** [subterm_pos s t] returns the positions of the term [s] with respect to
 the term [t]. This function is not tail-recursive. *)
 val var_pos : Variable.t -> t -> Position.t list
 (** [var_pos x t] returns the positions of the variable [x] with respect to
 the term [t]. This function is not tail-recursive. *)
 val vars_pos : t -> Position.t list
 (** [vars_pos t] returns the positions of all variables of the term [t].
 This function is not tail-recursive. *)

 (** {3 Folds} *)
 val count_fun : Function.t -> t -> int
 (** [count_fun f t] returns the number of occurrences of [f] in [t]. *)
 val fold_funs : (Function.t -> 'a -> 'a) -> 'a -> t -> 'a
 (** [fold_funs f d t] combines all function symbols of [t] with the
 default value [d], using the function [f]. *)
 
 (** {3 Iterators} *)

 val map : (Function.t -> Function.t) -> t -> t
 (** [map f t] applies the function [f] to all function symbols of the term
 [t]. This function is not tail-recursive. *)

 (** {3 Miscellaneous} *)

 val args : t -> t list
 (** [args t] returns the immediate arguments of the term [t]. If [t] is a
 variable, the empty list is returned. *)
 val copy : t -> t
 (** [copy t] copies the term [t]. *)
 val depth : t -> int
 (** [depth t] returns the depth of the term [t]. This function is not
 tail-recursive. *)
 val fun_size : t -> int
 (** [fun_size t] returns the total number of function symbols of the term
 [t]. Note that double occurrences of a function symbol are counted twice.
 This function is not tail-recursive. *)
 val hash : t -> int
 (** [hash t] returns a hash value for [t]. It is guaranteed that
 [hash s = hash t] whenever [compare s t = 0]. *)
 val proper_subterms : ?p:(t -> bool) -> t -> t list
 (** [proper_subterms ~p:p t] returns all proper subterms of [t] that satisfy
 the predicate [p]. This function is not tail-recursive. *)
 val size : t -> int
 (** [size t] returns the size of the term [t]. This function is not
 tail-recursive. *)
 val subterms : ?p:(t -> bool) -> t -> t list
 (** [subterms ~p:p t] returns all subterms of [t] that satisfy the predicate
 [p]. This function is not tail-recursive. *)
 val var_size : t -> int
 (** [var_size t] returns the total number of variables of the term [t].
 Note that double occurrences of a variable are counted twice. This function
 is not tail-recursive. *)

 (** {3 Compare Functions} *)

 val compare : t -> t -> int
 (** [compare s t] compares [s] and [t]. This function defines a total
 ordering on terms. *)
 val equal : t -> t -> bool
 (** [equal s t] checks if [s] and [t] are equal. This function is equivalent
 to [compare s t = 0]. *)

 (** {3 Parsers} *)

 val of_string : string list -> string -> t m
 (** [of_string vs s] applies [parse vs] to the input string [s] and lifts the
 result into a monad. *)
 val of_xml_string : string -> t m
 (** [of_xml_string s] applies [xml] to the input string [s] and lifts
 the result into a monad. *)
 val parse : string list -> t p
 (** [parse vs] takes a list of strings (the identifiers that should be
 parsed as variables) and returns a term-parser (having a signature as
 internal state). *)
 val xml : t x
 (** [xml] returns a term-parser (having a signature as internal state). *)

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
 val fprintfs : state -> Format.formatter -> t -> state
 (** [fprintfs s fmt t] prints [t] using the [OCaml] module [Format]. This
 function uses the information stored in the signature [s] to print
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is printed. Note that as a side
 effect [s] is changed too. This function is not tail-recursive. *)
 val to_string : t -> string
 (** [to_string t] returns a formatted string that represents [t]. This
 function is not tail-recursive.*)
 val to_stringm : t -> string m
 (** [to_stringm t] returns a formatted string that represents [t]. This
 function uses the information stored in the underlying signature to represent
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is used. Note that as a side effect
 copies of the underlying signature are changed too. This function is not
 tail-recursive. *)
 val to_strings : state -> t -> (state*string)
 (** [to_strings s t] returns a formatted string that represents [t] using the
 signature [s]. This function is not tail-recursive.*)
end

(** Terms
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Mon Sep  1 12:21:54 CEST 2008 *)

(** This module provides basic functions that deal with terms. *)
module Term : TERM with type 'a m = 'a Monad.t and type 'a p = 'a Parser.t and type state = Signature.t

(** {2 Module Context} *)

module type CONTEXT = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type term
 type t = Hole | More of Function.t * term list * t * term list

 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)
 
 val apply : term -> t -> term
 (** [apply t c] plugs the term [t] into the hole of [c]. This function is
 not tail-recursive. *)
 val compose : t -> t -> t
 (** [compose c d] plugs the context [c] into the hole of [d]. This function
 is not tail-recursive.*)
 val of_term : Position.t -> term -> t
 (** [of_term p t] constructs a context from the term [t], by replacing
 the subterm of [t] at position [p] by a hole. This function is not
 tail-recursive.
 @raise Failure "illegal position" if [p] does not exist in [t]. *)
 val subcontext : Position.t -> t -> t
 (** [subcontext p c] returns the subcontext of [c] at position [p].
 @raise Failure "Context.subcontext: illegal position" if the subterm
 of [c] at position [p] is not a subcontext of [c]. *)
 
 (** {3 Properties} *)
 
 val is_empty : t -> bool
 (** [is_empty c] checks if [c] is the empty context. *)

 (** {3 Miscellaneous} *)

 val hash : t -> int
 (** [hash c] returns the hash value of [c]. It is guaranteed that
 [hash c = hash d] if [compare c d = 0]. *)
 val hole_pos : t -> Position.t
 (** [hole_pos] returns the position of the hole in [c]. This function is
 not tail-recursive. *)
 val pos : t -> Position.t list
 (** [pos] returns all positions in [c]. This function is not
 tail-recursive. *)
 
 (** {3 Compare Functions} *)
 
 val compare : t -> t -> int
 (** [compare c d] compares [c] and [d]. This function defines a total
 ordering on contexts. *)
 val equal : t -> t -> bool
 (** [equal c d] checks if [c] and [d] are equal. This function is equivalent
 to [compare c d = 0]. *)
 
 (** {3 Printers} *)
 
 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt c] prints [c] using the [OCaml] module [Format]. This
 function is not tail-recursive.*)
 val fprintfm : Format.formatter -> t -> unit m
 (** [fprintfm fmt c] prints [c] using the [OCaml] module [Format]. This
 function uses the information stored in the underlying signature to print
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is printed. Note that as a side
 effect copies of the underlying signature are changed too. This function is
 not tail-recursive. *)
 val to_string : t -> string
 (** [to_string c] returns a formatted string that represents [c]. This
 function is not tail-recursive.*)
 val to_stringm : t -> string m
 (** [to_stringm c] returns a formatted string that represents [c]. This
 function uses the information stored in the underlying signature to represent
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is used. Note that as a side effect
 copies of the underlying signature are changed too. This function is not
 tail-recursive. *)
end

(** Contexts
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Mon Sep  1 12:21:54 CEST 2008 *)

(** This module provides basic functions that deal with contexts. *)
module Context : CONTEXT with type 'a m = 'a Monad.t and type term = Term.t

(** {2 Module Substitution} *)

module type SUBSTITUTION = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type context
 type term

 (*** INCLUDES ****************************************************************)
 include Util.Replacement.SIGNATURE with
  type domain = Variable.t and type range = term

 (*** VALUES ******************************************************************)
 (** {3 Apply Substitutions} *)

 val apply_term : t -> term -> term
 (** [apply_term s u] applies the substitution [s] to the term [u]. This
 function is not tail-recursive. *)
 val apply_context : t -> context -> context
 (** [apply_context s c] applies the substitution [s] to the context [c].
 This function is not tail-recursive. *)

 (** {3 Predicates} *)

 val is_renaming : t -> bool
 (** [is_renaming s] checks if [s] is a renaming, i.e., a bijective mapping
 from variables to variables. *)

 (** {3 Printers} *)

 val fprintfm : Format.formatter -> t -> unit m
 (** [fprintfm fmt s] prints [s] using the [OCaml] module [Format]. This
 function uses the information stored in the underlying signature to print
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is printed. Note that as a side
 effect copies of the underlying signature are changed too. This function is
 not tail-recursive. *)
 val to_stringm : t -> string m
 (** [to_stringm s] returns a formatted string that represents [s]. This
 function uses the information stored in the underlying signature to represent
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is used. Note that as a side effect
 copies of the underlying signature are changed too. This function is not
 tail-recursive. *)
end

(** Term Substitutions
@author Martin Korp
@since  Mon Sep  1 12:22:43 CEST 2008 *)

(** This module provides basic functions that deal with substitutions. *)
module Substitution : SUBSTITUTION with
 type 'a m = 'a Monad.t and type term = Term.t and type context = Context.t

(** {2 Module Elogic} *)

module type ELOGIC = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type substitution
 type term

 (*** EXCEPTION ***************************************************************)
 exception Not_matchable
 exception Not_semi_unifiable
 exception Not_unifiable

 (*** VALUES ******************************************************************)
 (** {3 Unification} *)

(*
 val are_semi_unifiable : term -> term -> bool
 (** [are_semi_unifiable u v] checks if [u] and [v] are semi-unifiable, i.e.,
 if there are substitutions [s] and [t] such that [us = vst]. *)
*)
 val are_unifiable : term -> term -> bool
 (** [are_unifiable u v] checks if [u] and [v] are unifiable, i.e., if there is
 a most general unifier [s] such that [us = vs]. *)
 val is_variant : term -> term -> bool
 (** [is_variant u v] checks if [u] is a variant of [v], i.e., if there is
 a renaming [s] such that [us = vs]. *)
 (*
 val semi_unify : term -> term -> substitution * substitution
 (** [semi_unify u v] computes unifiers [s] and [t] such that [us = vst].
 *)
 @raise Not_semi_unifiable if [u] and [v] are not semi-unifiable. *)
(*
 (* TODO implement semi_unify_problem *)
 val semi_unify_problem : (term * term) list -> substitution *
 substitution
 (** [semi_unify_problem p] computes unifiers [s] and [t] such that for all
 pairs [(u,v)] in [p], [us = vst].
*)
 @raise Not_semi_unifiable if [p] is not semi-unifiable. *)
 val unify : term -> term -> substitution
 (** [unify u v] computes a most general unifier for [u] and [v].
 @raise Not_unifiable if [u] and [v] are not unifiable. *)
 val unify_problem : (term * term) list -> substitution
 (** [unify_problem p] computes a most general unifier for the unification
 problem [p].
 @raise Not_unifiable if [p] does not admit a most general unifier. *)

 (** {3 Matching} *)

 val contains : term -> term -> bool
 (** [contains u v] checks if a subterm of [u] matches [v]. This function
 is not tail-recursive. *)
 val ground_matches : term -> term -> bool
 (** [ground_matches u v] checks if [u] ground matches [v], i.e., if
 there is a substitution [s] such that [u = vs] where variables of [u]
 are instantiated by arbitrary ground terms. *)
 val matches : term -> term -> bool
 (** [matches u v] checks if [u] matches [v], i.e., if there is a substitution
 [s] such that [u = vs]. *)
 val match_problem : (term * term) list -> substitution
 (** [match_problem p] computes a substitution [s] such that [u = vs] for all
 pairs [(u,v)] in [p].
 @raise Not_matchable if [p] does not admit such a substitution. *)
 val match_term : term -> term -> substitution
 (** [match_term u v] computes a substitution [s] such that [u = vs].
 @raise Not_matchable if [u] does not match [v]. *)
 val subsumes : term -> term -> bool
 (** [subsumes u v] is equivalent to [matches v u]. *)

 (** {3 Renaming} *)

 val renaming : term -> substitution m
 (** [renaming u] computes a renaming [s] with respect to the term [u]. Note
 that a renaming is a bijective mapping from variables to variables. This
 function is not tail-recursive. Note that as a side effect the underlying
 signature changes all over. *)
end

(** Equational Logic
@author Martin Korp
@since  Sat Dec 13 18:31:20 CEST 2008 *)

(** This module provides some special functions that deal with terms,
contexts and substitutions. *)
module Elogic : ELOGIC with
 type 'a m = 'a Monad.t and type substitution = Substitution.t and
 type term = Term.t

(** {2 Module Rule} *)

module type RULE = sig
 (*** TYPES *******************************************************************)
 type 'a m 
 type 'a p
 type 'a x
 type substitution
 type term
 type t

 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val invert : t -> t
 (** [invert r] flips the left- and right hand side of [r], i.e., if
 [r = (s,t)] then the rule [(t,s)] is returned. Note that the returned
 rules is not longer a rewrite rule if [s] contains variables which do
 not occur in [t]. *)
 val lhs : t -> term
 (** [lhs r] returns the left-hand side of the rule [r]. *)
 val of_terms : term -> term -> t
 (** [of_terms s t] creates a rewrite rule consisting of the left-hand side [s]
 and the right-hand side [t]. *)
 val reflect : t -> t
 (** [reflect r] reflects the left- and right-hand side of [r]. See
 {!val: Rewriting.TERM.reflect}. This function is not tail-recursive. *)
 val reverse : t -> t
 (** [reverse r] reverses the left- and right-hand side of [r]. See
 {!val: Rewriting.TERM.reverse}. This function is not tail-recursive.
 @raise Failure "term is not a string" if either the left- or
 right-hand side of [r] is not a string. *)
 val rhs : t -> term
 (** [rhs r] returns the right-hand side of the rule [r]. *)
 val to_terms : t -> term * term
 (** [to_terms r] return the left- and right-hand side of [r]. *)

 (** {3 Monadic Constructors and Destructors} *)

 val rename : t -> t m
 (** [rename r] renames the variables of the left- and right-hand side of
 [r] via a renaming. Note that the function works only correctly if all
 variables of the right-hand side occur also in the left-hand side. This
 function is not tail-recursive. Note that as a side effect the underlying
 signature changes all over. *)

 (** {3 Iterators} *)

 val apply : (term -> 'a) -> (term -> 'b) -> t -> 'a * 'b
 (** [apply f g r] applies the function [f] to the left- and [g] to the
 right-hand side of [r], i.e., if [r = (s,t)] then the result [(f s,g t)]
 is returned. *)
 val count_fun : Function.t -> t -> int
 (** [count_fun f r] counts the number of occurrences of [f] in the rule
 [r]. *)
 val fold : (term -> 'a -> 'a) -> 'a -> t -> 'a
 (** [fold f d r] combines the left- and right-hand side of [r] using the
 function [f]. *)
 val map : (term -> 'a) -> t -> 'a * 'a
 (** [map f r] is equivalent to [apply f f r]. *)
 val project : (term -> term) -> t -> t
 (** [project f r] applies the function [f] to the left- and right-hand side
 of rule [r]. *)
 val uncurry : (term -> term -> 'a) -> t -> 'a
 (** [uncurry f r] applies [f] to the left- and right-hand side of [r]. I.e.
 if [r = (s,t)] then the result [f s t] is returned. *)

 (** {3 Rule Symbols} *)

 val cons : t -> Function.t list
 (** [cons r] returns all constants of the left- and right-hand side of [r].
 This function is not tail-recursive. *)
 val funs : t -> Function.t list
 (** [funs r] returns all function symbols of the left- and right-hand side
 of [r]. This function is not tail-recursive. *)
 val left_cons : t -> Function.t list
 (** [left_cons r] returns all constants of the left-hand side of [r]. This
 function is not tail-recursive. *)
 val left_funs : t -> Function.t list
 (** [left_funs r] returns all function symbols of the left-hand side of [r].
 This function is not tail-recursive. *)
 val left_symbols : t -> (Variable.t,Function.t) Util.either list
 (** [left_symbols r] returns all symbols of the left-hand side of the rule
 [r]. This function is not tail-recursive. *)
 val left_vars : t -> Variable.t list
 (** [left_vars r] returns all variables of the left-hand side of the rule [r].
 This function is not tail-recursive. *)
 val left_root : t -> Function.t
 (** [left_root r] returns the root symbol of the left-hand side of [r].
 @raise Failure "left-hand side is a variable" if the left-hand side of
 [r] is a variable. *)
 val right_cons : t -> Function.t list
 (** [right_cons r] returns all constants of the right-hand side of [r]. This
 function is not tail-recursive. *)
 val right_funs : t -> Function.t list
 (** [right_funs r] returns all function symbols of the right-hand side of [r].
 This function is not tail-recursive. *)
 val right_symbols : t -> (Variable.t,Function.t) Util.either list
 (** [right_symbols r] returns all symbols of the right-hand side of [r]. This
 function is not tail-recursive. *)
 val right_vars : t -> Variable.t list
 (** [right_vars r] returns all variables of the right-hand side of the rule
 [r]. This function is not tail-recursive. *)
 val right_root : t -> Function.t
 (** [right_root r] returns the root symbol of the right-hand side of [r].
 @raise Failure "right-hand side is a variable" if the right-hand side of
 [r] is a variable. *)
 val roots : t -> Function.t list
 (** [roots r] returns the root symbol of the left- and right-hand side of
 [r]. Note that the returned list is unique. *)
 val symbols : t -> (Variable.t,Function.t) Util.either list
 (** [symbols r] returns all symbols of the left- and right-hand side of the
 rule [r]. This function is not tail-recursive. *)
 val vars : t -> Variable.t list
 (** [vars r] returns all variables of the left- and right-hand side of the
 rule [r]. This function is not tail-recursive. *)

 (** {3 Properties} *)

 val is_build : Function.t list -> t -> bool
 (** [is_build fs r] checks if all function symbols of [r] occur in [fs]. *)
 val is_collapsing : t -> bool
 (** [is_collapsing r] checks if the right-hand side of [r] is a variable. *)
 val is_contained : t -> bool
 (** [is_contained r] checks if the left-hand side of [r] is contained in the
 right-hand side. This function is not tail-recursive. *)
 val is_dummy : t -> bool
 (** [is_dummy r] checks if the left- and right-hand side of [r] consists only
 of unary function symbols and constants. *)
 val is_duplicating : t -> bool
 (** [is_duplicating r] checks if there is a variable which occurs more often
 on the right-hand side than on the left-hand side. Not tail-recursive. *)
 val is_embedded : t -> bool
 (** [is_embedded r] checks if the right-hand side of [r] is embedded in the
 left-hand side. See {!val: Rewriting.TERM.is_embedded}. *)
 val is_erasing : t -> bool
 (** [is_erasing r] checks if [r] is erasing, i.e., if there is a variable
 of the left-hand side which does not occur on the right-hand side. This
 function is not tail-recursive. *)
 val is_flat : t -> bool
 (** [is_flat r] checks if the left- and right-hand side of [r] are flat
 terms. See {!val: Rewriting.TERM.is_flat}. *)
 val is_ground : t -> bool
 (** [is_ground r] checks if the left- and right-hand side of [r] are ground
 terms. *)
 val is_growing : t -> bool
 (** [is_growing r] checks if the rewrite rule [r] is growing. Note that a
 rewrite rule is called growing if all variables that occur in the left- and
 right-hand side of [r] occur at most at depth [1] in the left-hand side of
 [r]. Not tail-recursive. *)
 val is_left_build : Function.t list -> t -> bool
 (** [is_left_build fs r] checks if all function symbols of the left-hand
 side of [r] occur in [fs]. *)
 val is_left_dummy : t -> bool
 (** [is_left_dummy r] checks if the left-hand side of [r] consists only of
 unary function symbols and constants. *)
 val is_left_flat : t -> bool
 (** [is_left_flat r] checks if the left-hand side of [r] is a flat term.
 See {!val: Rewriting.TERM.is_flat}. *)
 val is_left_ground : t -> bool
 (** [is_left_ground r] checks if the left-hand side of [r] is a ground
 term. *)
 val is_left_linear : t -> bool
 (** [is_left_linear r] checks if the left-hand side of [r] is a linear term.
 Not tail-recursive. *)
 val is_left_shallow : t -> bool
 (** [is_left_shallow r] checks if the left-hand side of [r] is a shallow
 term. See {!val: Rewriting.TERM.is_shallow}. *)
 val is_left_string : t -> bool
 (** [is_left_string r] checks if the left-hand side of [r] is a string. *)
 val is_linear : t -> bool
 (** [is_linear r] checks if the left- and right-hand side of [r] are linear
 terms. Not tail-recursive. *)
 val is_normal_form : term -> t -> bool
 (** [is_normal_form t r] checks if [t] is in normal form with respect to
 [r]. This function is not tail-recursive. *)
 val is_proper_embedded : t -> bool
 (** [is_proper_embedded r] checks if the right-hand side of [r] is proper
 embedded in the left-hand side. *)
 val is_redex : term -> t -> bool
 (** [is_redex t r] checks if [t] matches the left-hand side of [r]. *)
 val is_rewrite_rule : t -> bool
 (** [is_rewrite_rule r] checks if [r] is a rewrite rule, i.e., if the
 left-hand side of [r] is not a variable and all variables of the right-hand
 side occur also in the left-hand side. *)
 val is_right_build : Function.t list -> t -> bool
 (** [is_right_build fs r] checks if all function symbols of the right-hand
 side of [r] occur in [fs]. *)
 val is_right_dummy : t -> bool
 (** [is_right_dummy r] checks if the right-hand side of [r] consists only of
 unary function symbols and constants. *)
 val is_right_flat : t -> bool
 (** [is_right_flat r] checks if the right-hand side of [r] is a flat term.
 See {!val: Rewriting.TERM.is_flat}. *)
 val is_right_ground : t -> bool
 (** [is_right_ground r] checks if the right-hand side of [r] is a ground
 term. *)
 val is_right_linear : t -> bool
 (** [is_right_linear r] checks if the right-hand side of [r] is a linear
 term. Not tail-recursive. *)
 val is_right_shallow : t -> bool
 (** [is_right_shallow r] checks if the right-hand side of [r] is a shallow
 term. See {!val: Rewriting.TERM.is_shallow}. *)
 val is_right_string : t -> bool
 (** [is_right_string r] checks if the right-hand side of [r] is a string. *)
 val is_shallow : t -> bool
 (** [is_shallow r] checks if both, the left- and right-hand side of [r] are
 shallow terms. See {!val: Rewriting.TERM.is_shallow}. *)
 val is_size_preserving : t -> bool
 (** [is_size_preserving r] checks if the size of the left-hand side of [r] is
 greater or equal than the size of the right-hand side of [r]. *)
 val is_string : t -> bool
 (** [is_string r] checks if the left- and right-hand side of [r] are
 strings. *)
 val is_variant : t -> t -> bool
 (** [is_variant r r'] checks if there is a renaming [s] such that [us = u's]
 and [vs = v's] where [r = (u,v)] and [r' = (u',v')]. *)
 val matches : t -> t -> bool
 (** [matches r r'] checks if [r] matches [r'], i.e., if there is a
 substitution [s] such that [u = u's] and [v = v's] where [r = (u,v)] and
 [r' = (u',v')]. *)
 val subsumes : t -> t -> bool
 (** [subsumed r r'] is equivalent to [matches r' r]. *)

 (** {3 Monadic Properties} *)

 val are_overlapping : t -> t -> bool m
 (** [are_overlapping r r'] checks if [r] and [r'] cause an overlap. Note that
 two rewrite rules [r] and [r'] cause an overlap if there is a position [p]
 such that the subterm of the left-hand side of [r] at position [p] unifies
 with the left-hand side of [r']. Furthermore if [p] is the root position,
 [r] and [r'] my not be variants. Not tail-recursive. Note that as a side
 effect the underlying signature changes all over. *)
 val is_overlap : Position.t -> t -> t -> bool m
 (** [is_overlap p r r'] checks if [r] and [r'] cause an overlap at position
 [p]. Note that two rewrite rules [r] and [r'] are overlapping at position [p]
 if the subterm of the left-hand side of [r] at position [p] unifies with the
 left-hand side of [r']. Furthermore if [p] is the root position, [r] and [r']
 may not be variants. Not tail-recursive. Note that as a side effect the
 underlying signature changes all over. *)

 (** {3 Rewrite Terms} *)

 val reducts : term -> t -> term list
 (** [reducts t r] computes all reducts of the term [t] with respect to the
 rewrite rule [r]. The order of the reducts is arbitrary. Not tail-recursive.
 @raise Failure "left-hand side is a variable" if the left-hand side of [r]
 is a variable. *)
 val rewrite : term -> Position.t -> t -> term
 (** [rewrite t p r] rewrites the term [t] at position [p] using the rewrite
 rule [r]. Not tail-recursive.
 @raise Elogic.Not_matchable if the subterm of [t] at position [p] does not
 match the left-hand side of [r]. *)
 val rewrites : term -> Position.t -> t -> term -> bool
 (** [rewrites s p r t] checks if [s] rewrites to [t] using rule [r] at
 position [p]. *)
 val redex_pos : term -> t -> Position.t list
 (** [redex_pos t r] computes all redex positions of the term [t] with
 respect to the rewrite rule [r]. Not tail-recursive.
 @raise Failure "left-hand side is a variable" if the left-hand side of
 [r] is a variable. *)

 (** {3 Equational Logic} *)

 val apply_sub : substitution -> t -> t
 (** [apply_sub s r] applies the substitution [s] to the left- and right-hand
 side of [r]. *)

 (** {3 Miscellaneous} *)

 val copy : t -> t
 (** [copy r] returns a copy of [r]. *)
 val depth : t -> int
 (** [depth r] returns the maximal depth of the left- and right-hand side of
 [r]. This function is not tail-recursive. *)
 val hash : t -> int
 (** [hash r] returns the hash value of [r]. *)
 val overlaps : t -> t -> (t * Position.t * t) list m
 (** [overlaps r r'] computes all positions of [r'] that cause an overlap with
 [r]. Rewrite rules [r] and [r'] overlap if there is a
 position [p] such that the subterm of the left-hand side of [r'] at position
 [p] unifies with the left-hand side of [r]. Furthermore if [p] is the root
 position, [r] and [r'] may not be variants. Not tail-recursive. 
 Changes the underlying signature. *)

 (** {3 Compare Functions} *)

 val compare : t -> t -> int
 (** [compare r r'] compares [r] and [r']. This function defines a total
 ordering on rules. *)
 val equal : t -> t -> bool
 (** [equal r r'] checks if [r] and [r'] are equal. This function is
 equivalent to [compare r r' = 0]. *)

 (** {3 Parsers} *)

 val of_string : string list -> string -> t m
 (** [of_string vs s] applies [parse vs] to the input string [s] and lifts
 the result into the signature monad. *)
 val of_xml_string : string -> t m
 (** [of_xml_string s] applies [xml] to the input string [s] and lifts
 the result into a monad. *)
 val parse : string list -> t p
 (** [parse vs] takes a list of strings (the identifiers that should be
 parsed as variables) and returns a rule-parser (having a signature as
 internal state). *)
 val xml : t x
 (** [xml] returns a rule-parser (having a signature as internal state). *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt r] prints [r] using the [OCaml] module [Format]. This
 function is not tail-recursive. *)
 val fprintfm : Format.formatter -> t -> unit m
 (** [fprintfm fmt r] prints [r] using the [OCaml] module [Format]. This
 function uses the information stored in the underlying signature to print
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is printed. Note that as a side
 effect copies of the underlying signature are changed too. This function is
 not tail-recursive. *)
 val to_string : t -> string
 (** [to_string r] returns a formatted string that represents [r]. This
 function is not tail-recursive. *)
 val to_stringm : t -> string m
 (** [to_stringm r] returns a formatted string that represents [r]. This
 function uses the information stored in the underlying signature to represent
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is used. Note that as a side effect
 copies of the underlying signature are changed too. This function is not
 tail-recursive. *)
end

(** Rewrite Rules
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Mon Sep  1 12:21:45 CEST 2008 *)

(** This module deals with rewrite rules. *)
module Rule : RULE with
 type 'a m = 'a Monad.t and type 'a p = 'a Parser.t and
 type substitution = Substitution.t and type term = Term.t

(** {2 Module Trs} *)

module type TRS = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type 'a p
 type 'a x
 type rule
 type term
 type t

 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val add : rule -> t -> t
 (** [add u r] adds the rule [u] to [r]. In case that [u] is already contained
 in [r], it is added again. *)
 val diff : t -> t -> t
 (** [diff r s] is equivalent to {!val: Util.List.diff}. *)
 val empty : t
 (** [empty] represents the empty TRS. *)
 val intersect : t -> t -> t
 (** [intersect r s] computes the intersection of [r] and [s]. *)
 val invert : t -> t
 (** [invert r] switches the left- and right-hand sides of [r]. This function
 is not tail-recursive. *)
 val lhs : t -> term list
 (** [lhs r] returns the left-hand sides of [r]. Note that the returned list
 is unique. Furthermore the order of the terms is arbitrary. *)
 val of_list : rule list -> t
 (** [of_list rs] returns a TRS consisting of the rewrite rules contained in
 [rs]. Note that multiple copies are preserved. *)
 val partition : (rule -> bool) -> t -> t * t
 (** [partition p r] splits the TRS [r] into those rules that satisfy [p]
 and those that do not. *)
 val reflect : t -> t
 (** [reflect r] reflects the left- and right-hand sides of [r]. This
 function is not tail-recursive. See {!val: Rewriting.RULE.reflect}. *)
 val remove : rule -> t -> t
 (** [remove u r] is equivalent to {!val: Util.List.remove_all}. *)
 val reverse : t -> t
 (** [reverse r] reverses the left- and right-hand sides of [r]. This function
 is not tail-recursive.
 @raise Failure "not a srs" if [r] is not a SRS. *)
 val rhs : t -> term list
 (** [rhs r] returns the right-hand sides of [r]. Note that the returned list
 is unique. Furthermore the order of the terms is arbitrary. *)
 val singleton : rule -> t
 (** [singleton u] is equivalent to {!val: Util.List.singleton}. *)
 val to_list : t -> rule list
 (** [to_list r] returns the rules of [r] as a list. *)
 val union : t -> t -> t
 (** [union] is equivalent to {!val: Util.List.union}. *)
 val unique : t -> t
 (** [unique r] is equivalent to {!val: Util.List.unique}. *)
 val terms : t -> term list
 (** [terms r] returns the left- and right-hand sides of [r]. Note that the
 returned list is unique. Furthermore the order of the terms is arbitrary. *)

 (** {3 Monadic Constructors and Destructors} *)

 val rename : t -> t m
 (** [rename r] renames the variables of the left- and right-hand sides of
 [r] via renamings. For each rule, a fresh renaming is used. Note that the
 function works only correctly if all rules in [r] are rewrite rules. This
 function is not tail-recursive. Note that as a side effect the underlying
 signature changes all over. *)

 (** {3 Iterators} *)

 val count_fun : Function.t -> t -> int
 (** [count_fun f trs] counts the number of occurrences of the function
 symbol [f] in the TRS [trs]. *)
 val flat_map : (rule -> 'a list) -> t -> 'a list
 (** [flat_map f r] is equivalent to {!val: Util.List.flat_map}. *)
 val fold : (rule -> 'a -> 'a) -> 'a -> t -> 'a
 (** [fold f d r] is equivalent to {!val: Util.List.foldr}. *)
 val iter : (rule -> unit) -> t -> unit
 (** [iter f r] is equivalent to {!val: Util.List.LIST.iter}. *)
 val map : (rule -> 'a) -> t -> 'a list
 (** [map f r] is equivalent to {!val: Util.List.LIST.map}. Not
 tail-recursive. *)
 val project : (rule -> rule) -> t -> t
 (** [project f r] applies the function [f] to all rules in [r]. Not
 tail-recursive. *)

 (** {3 Scan Functions} *)

 val exists : (rule -> bool) -> t -> bool
 (** [exists p r] is equivalent to {!val: Util.List.LIST.exists}. *)
 val for_all : (rule -> bool) -> t -> bool
 (** [for_all p r] is equivalent to {!val: Util.List.LIST.for_all}. *)
 val mem : rule -> t -> bool
 (** [mem u r] is equivalent to {!val: Util.List.mem}. *)

 (** {3 Search Functions} *)

 val choose : t -> rule
 (** [choose r] is equivalent to {!val: Util.List.LIST.hd}.
 @raise Failure "empty TRS" if [r] is the empty TRS. *)
 val filter : (rule -> bool) -> t -> t
 (** [filter p r] is equivalent to {!val: Util.List.LIST.filter}. *)
 val find : (rule -> bool) -> t -> rule
 (** [find p r] is equivalent to {!val: Util.List.LIST.find}.
 @raise Not_found if no rewrite rule in [r] satisfies the predicate [p]. *)

 (** {3 TRS Symbols} *)

 val cons : t -> Function.t list
 (** [cons r] returns the constants of the TRS [r]. This function is not
 tail-recursive. *)
 val con_symbols : t -> Function.t list
 (** [con_symbols r] returns the constructor symbols of the TRS [r]. This
 function is not tail-recursive. *)
 val def_symbols : t -> Function.t list
 (** [def_symbols r]  returns the root symbols of the left-hand sides (defined
 symbols) of [r]. *)
 val funs : t -> Function.t list
 (** [funs r] returns the function symbols of the TRS [r]. This function is
 not tail-recursive. *)
 val left_cons : t -> Function.t list
 (** [left_cons r] returns the constants of the left-hand sides of [r]. This
 function is not tail-recursive. *)
 val left_con_symbols : t -> Function.t list
 (** [left_con_symbols r] returns the constructor symbols of the left-hand
 sides of [r]. This function is not tail-recursive. *)
 val left_funs : t -> Function.t list
 (** [left_funs r] returns the function symbols of the left-hand sides of
 [r]. This function is not tail-recursive. *)
 val left_roots : t -> Function.t list
 (** [left_roots r] is equivalent to {!val: Rewriting.TRS.def_symbols}. *)
 val left_symbols : t -> (Variable.t,Function.t) Util.either list
 (** [left_symbols r] returns the function symbols and variables of the
 left-hand sides of [r]. This function is not tail-recursive. *)
 val left_vars : t -> Variable.t list
 (** [left_vars r] returns the variables of the left-hand sides of [r]. This
 function is not tail-recursive. *)
 val right_cons : t -> Function.t list
 (** [right_cons r] returns the constants of the right-hand sides of [r].
 This function is not tail-recursive. *)
 val right_con_symbols : t -> Function.t list
 (** [right_con_symbols r] returns the constructor symbols of the right-hand
 sides of [r]. This function is not tail-recursive. *)
 val right_funs : t -> Function.t list
 (** [right_funs r] returns the function symbols of the right-hand sides of
 [r]. This function is not tail-recursive. *)
 val right_roots : t -> Function.t list
 (** [right_roots r] returns the root symbols of the right-hand sides of
 [r]. *)
 val right_symbols : t -> (Variable.t,Function.t) Util.either list
 (** [right_symbols r] returns the function symbols and variables of the
 right-hand sides of [r]. This function is not tail-recursive. *)
 val right_vars : t -> Variable.t list
 (** [right_vars r] returns the variables of the right-hand sides of [r].
 This function is not tail-recursive. *)
 val roots : t -> Function.t list
 (** [roots r] returns all root symbols of the TRS [r]. *)
 val symbols : t -> (Variable.t,Function.t) Util.either list
 (** [symbols r] returns the function symbols and variables of the TRS [r].
 This function is not tail-recursive. *)
 val vars : t -> Variable.t list
 (** [vars r] returns the variables of the TRS [r]. This function is not
 tail-recursive. *)

 (** {3 Properties} *)

 val is_build : Function.t list -> t -> bool
 (** [is_build fs r] checks if all function symbols of [r] occur in [fs]. *)
 val is_collapsing : t -> bool
 (** [is_collapsing r] checks if [r] contains a collapsing rewrite rule. 
 See {!val: Rewriting.RULE.is_collapsing}. *)
 val is_constructor : t -> bool
 (** [is_constructor r] checks if all left-hand sides of [r] are
 constructor terms. Note that a term is a constructor term if all function
 symbols below the root are constructor symbols. Not tail-recursive. *)
 val is_dummy : t -> bool
 (** [is_dummy r] checks if all rewrite rules of [r] are dummy. See
 {!val: Rewriting.RULE.is_dummy}. *)
 val is_duplicating : t -> bool
 (** [is_duplicating r] checks if [r] admits a duplicating rewrite rule. This
 function is not tail-recursive. *)
 val is_erasing : t -> bool
 (** [is_erasing r] checks if [r] contains an erasing rewrite rule. See
 {!val: Rewriting.RULE.is_erasing}. This function is not tail-recursive. *)
 val is_empty : t -> bool
 (** [is_empty r] checks if [r] is empty. *)
 val is_flat : t -> bool
 (** [is_flat r] checks if all rules in [r] are flat. See
 {!val: Rewriting.RULE.is_flat}. *)
 val is_ground : t -> bool
 (** [is_ground r] checks if [r] consists only of ground rewrite rules. *)
 val is_growing : t -> bool
 (** [is_growing r] checks if [r] consists only of growing rewrite rules.
 Not tail-recursive. *)
 val is_left_build : Function.t list -> t -> bool
 (** [is_left_build fs r] checks if all function symbols of the left-hand
 sides of [r] occur in [fs]. *)
 val is_left_constructor : t -> bool
 (** [is_left_constructor r] is equivalent to
 {!val: Rewriting.TRS.is_constructor}. This function is not tail-recursive. *)
 val is_left_dummy : t -> bool
 (** [is_left_dummy r] checks if all left-hand sides of [r] are dummy. *)
 val is_left_flat : t -> bool
 (** [is_left_flat r] checks if all left-hand sides of [r] are flat. *)
 val is_left_ground : t -> bool
 (** [is_left_ground r] checks if all left-hand sides of [r] are ground. *)
 val is_left_linear : t -> bool
 (** [is_left_linear r] checks if all rules in [r] are left-linear. This
 function is not tail-recursive. *)
 val is_left_shallow : t -> bool
 (** [is_left_shallow r] checks if all left-hand sides of [r] are shallow. *)
 val is_linear : t -> bool
 (** [is_linear r] checks if all rules in [r] are linear. This function is
 not tail-recursive. *)
 val is_normal_form : term -> t -> bool
 (** [is_normal_form t r] checks if [t] is a normal form with respect to [r].
 This function is not tail-recursive. *)
 val is_proper_subset : t -> t -> bool
 (** [is_proper_subset r s] is equivalent to
 {!val: Util.List.is_proper_subset}. *)
 val is_redex : term -> t -> bool
 (** [is_redex u r] checks if [u] is a redex with respect to [r]. *)
 val is_right_build : Function.t list -> t -> bool
 (** [is_right_build fs r] checks if all function symbols of the right-hand
 sides of [r] occur in [fs]. *)
 val is_right_constructor : t -> bool
 (** [is_right_constructor r] checks if all right-hand sides of [r] are
 constructor terms. Note that a term is a constructor term if all function
 symbols below the root are constructor symbols. Not tail-recursive. *)
 val is_right_dummy : t -> bool
 (** [is_right_dummy r] checks if all right-hand sides of [r] are dummy. *)
 val is_right_flat : t -> bool
 (** [is_right_flat r] checks if all right-hand sides of [r] are flat. *)
 val is_right_ground : t -> bool
 (** [is_right_ground r] checks if all right-hand sides of [r] are ground. *)
 val is_right_linear : t -> bool
 (** [is_right_linear r] checks if all rules in [r] are right-linear. This
 function is not tail-recursive. *)
 val is_right_shallow : t -> bool
 (** [is_right_shallow r] checks if all right-hand sides of [r] are shallow. *)
 val is_shallow : t -> bool
 (** [is_shallow r] checks if all rewrite rules in [r] are shallow. *)
 val is_size_preserving : t -> bool
 (** [is_size_preserving r] checks if all rewrite rules in [r] are size
 preserving (i.e. if the size of each left-hand side is greater or equal
 than the size of the corresponding right-hand side). *)
 val is_srs : t -> bool
 (** [is_srs r] checks if [r] is a SRS. *)
 val is_subset : t -> t -> bool
 (** [is_subset r s] is equivalent to {!val: Util.List.is_subset}. *)
 val is_trs : t -> bool
 (** [is_trs r] checks if [r] is a TRS, i.e., if each rule in [r] is a
 rewrite rule (left-hand side is not a variable and all variables of the
 right-hand side occur also on the left-hand side). *)
 val is_variant : t -> t -> bool
 (** [is_variant r s] checks if [r] is a variant of [s], i.e., if for all
 rules in [r] there is a variant in [s] and vice versa. *)

 (** {3 Monadic Properties} *)

 val is_applicative : t -> bool m
 (** [is_applicative r] checks if [r] is an applicative system. If [r]
 contains a function symbol with undefined arity, the returned monad
 represents the occurred error. Note that a TRS is applicative if its
 signature consists of one binary symbol and arbitrary many constants.
 Not tail-recursive. *)
 val is_overlapping : t -> bool m
 (** [is_overlapping r] checks if some rules in [r] cause an overlap.
 This function is not tail-recursive. *)
 val is_overlay : t -> bool m
 (** [is_overlay r] checks if [r] is an overlay system. An overlay system is
 a TRS where overlaps take place only at root positions. This function is not
 tail-recursive. *)

 (** {3 Rewrite Terms} *)

 val reducts : term -> t -> term list
 (** [reducts u r] computes all reducts of the term [u] with respect to the
 TRS [r]. The order of the reducts is arbitrary. Not tail-recursive.
 @raise Failure "left-hand side is a variable" if some left-hand side of [r]
 is a variable. *)
 val rewrite : term -> Position.t -> t -> term list
 (** [rewrite u p r] rewrites the term [u] at position [p] using the rewrite
 rules in [r]. Not tail-recursive.
 @raise Failure "illegal position" if [p] is not a function position.
 @raise Elogic.Not_matchable if the left-hand side of [r] does not match the
 subterm of [u] at position [p]. *)
 val rewrites : term -> Position.t -> t -> term -> bool
 (** [rewrites s p trs t] checks if [s] rewrites to [t] using a rule in
 [trs] at position [p]. *)

 (** {3 Miscellaneous} *)

 val copy : t -> t
 (** [copy r] copies the TRS [r]. *)
 val depth : t -> int
 (** [depth r] returns the maximal depth of the TRS [r]. This function is not
 tail-recursive. *)
 val hash : t -> int
 (** [hash r] returns a hash value for [r]. It is guaranteed that
 [hash r = hash s] whenever [compare s t = 0]. *)
 val overlaps : t -> (rule * Position.t * rule) list m
 (** [overlaps r] computes all overlaps between rewrite rules in [r]. 
 Rewrite rules [u] and [v] cause an overlap if there is a position
 [p] such that the subterm of the left-hand side of [v] at position [p]
 unifies with the left-hand side of [u]. Furthermore if [p] is the root
 position, [u] and [v] may not be variants. Not tail-recursive. Note that as
 a side effect the underlying signature changes all over. *)
 val size : t -> int
 (** [size r] returns the number of rewrite rules contained in [r]. *)

 (** {3 Compare Functions} *)

 val compare : t -> t -> int
 (** [compare r s] compares if [r] and [s] contain the same rules
 (duplicate rules are not ignored). This function defines a total
 ordering on TRSs. *)
 val equal : t -> t -> bool
 (** [equal r s] checks if [r] and [s] are equal (duplicates of rules are
 not ignored). This function is equivalent to [compare r s = 0]. *)
 val equivalent : t -> t -> bool
 (** [equivalent r s] checks if [r] and [s] contain the same rules using
 set comparison. I.e., duplicates of rules are ignored. *)

 (** {3 Parsers} *)

 val of_string : string list -> string -> t m
 (** [of_string vs s] applies [parse vs] to the input string [s] and lifts
 the result into the signature monad. *)
 val of_xml_string : string -> t m
 (** [of_xml_string s] applies [xml] to the input string [s] and lifts
 the result into a monad. *)
 val parse : string list -> t p
 (** [parse vs] takes a list of strings (the identifiers that should be
 parsed as variables) and returns a TRS-parser (having a signature as
 internal state). *)
 val xml : t x
 (** [xml] returns a TRS-parser (having a signature as internal state). *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt r] prints [r] using the [OCaml] module [Format]. This
 function is not tail-recursive. *)
 val fprintfm : Format.formatter -> t -> unit m
 (** [fprintfm fmt r] prints [r] using the [OCaml] module [Format]. This
 function uses the information stored in the underlying signature to print
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is printed. Note that as a side
 effect copies of the underlying signature are changed too. This function is
 not tail-recursive. *)
 val to_string : t -> string
 (** [to_string r] returns a formatted string that represents [r]. This
 function is not tail-recursive. *)
 val to_stringm : t -> string m
 (** [to_stringm r] returns a formatted string that represents [r]. This
 function uses the information stored in the underlying signature to represent
 function symbols and variables. If the name of a function symbol or variable
 is not defined, a randomly generated name is used. Note that as a side effect
 copies of the underlying signature are changed too. This function is not
 tail-recursive. *)
end

(** Term Rewrite Systems
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Mon Sep  1 12:21:37 CEST 2008 *)

(** This module deals with term rewrite systems. *)
module Trs : TRS with
 type 'a m = 'a Monad.t and type 'a p = 'a Parser.t and
 type rule = Rule.t and type term = Term.t

(** {2 Module Rewrite} *)

module type REWRITE = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type term
 type rule
 type trs
 type position

 type step

 type t =
  | Full (** full rewriting *)
 (** Specifies the strategies that can be used to rewrite terms. *)

 (*** VALUES ******************************************************************)

 val step_make : term -> position -> rule -> step
 (** [step_make t p r] creates a rewrite step *)

 val step_get_term : step -> term
 (** [step_get_term s] return starting term from rewrite step [s] *)
 val step_get_rule : step -> rule
 (** [step_get_term s] return rule from rewrite step [s] *)
 val step_get_pos : step -> position
 (** [step_get_term s] return position from rewrite step [s] *)

 val step_get_lab : step -> int list
 val step_add_lab_left : step -> int -> step
 val step_add_lab_right: step -> int -> step


(*
 val join_get_sequences : join -> sequence * sequence
 val join_to_join : sequence * sequence -> join
 val sequence_to_steps : sequence -> step list
 val sequence_of_steps : step list -> sequence
 *)

 (** {3 Rewrite Strategies} *)

 val are_joinable : ?s:t -> ?n:int -> term -> term -> trs -> bool
 (** [are_joinable ~s:s ~n:n u v r] checks if the terms [u] and [v] are
 joinable with respect to the TRS [r] within [n] steps using the rewrite
 strategy [s]. If [n] is smaller than [0] this function may not
 terminate. Per default [s = Full] and [n = ~-1]. *)
 val reducts : ?s:t -> ?n:int -> term -> trs -> term list
 (** [reducts ~n:n u r] computes all reducts of the term [u] with respect to
 the TRS [r] that can be reached within [n] steps. If [n] is smaller than [0]
 this function may not terminate. Per default [s = Full] and [n = ~-1].
 The reducts are not ordered.  This function is not tail-recursive. *)
 (*
 val rewrite : ?s:t -> ?n:int -> term -> trs -> term list
 (** [rewrite ~s:s ~n:n u r] rewrites the term [u] according to the strategy
 [s] with respect to the TRS [r], [n] steps. If the value of [n] is smaller
 than [0], then this function may not terminate.
 Per default [s = Full] and [n = ~-1]. *)
 *)

 (** {3 Confluence} *)

 val critical_pairs : trs -> (term * term) list m
 (** [critical_pairs r] computes all critical pairs of [r]. This function
 is not tail-recursive. *)
 val is_wcr : ?s:t -> ?n:int -> trs -> bool m
 (** [is_wcr ~s:s ~n:n r] checks if all critical pairs of [r] are joinable.
 Joinability of two terms is tested via the function
 {!val: Rewriting.REWRITE.are_joinable} where the strategy [s] is applied at
 most [n] steps. Not tail-recursive. *)
 val min_joins : ?s:t -> term -> term -> trs -> (term list * term list) list
 (** [min_joins ~s:s t u trs] computes the joining sequences of minimal 
 length. Default for [s] is [Full].*)
end

(** Rewrite Terms
@author Martin Korp
@since  Mon Dec 23 14:44:35 CEST 2008 *)

(** This module provides several functions that allow to rewrite terms
or that are based on functions which rewrite terms. *)
module Rewrite : REWRITE with
 type 'a m = 'a Monad.t and type term = Term.t and type trs = Trs.t

(** {2 Module Diagram} *)

module type DIAGRAM = sig
 (*** TYPES *******************************************************************)
 type 'a m
 type term
 type trs

 type step

 type t (** full rewriting *)
 (** Specifies the strategies that can be used to rewrite terms. *)

 (*** VALUES ******************************************************************)
 val critical : trs -> t list m
 (** [crtical trs] computes the (minimal) critical diagrams *)
 val peak : t -> step * step
 (** [peak d] returns the (local) peak of [d] *)
 val joins : t -> (step list * step list) list
 (** [joins d] returns the (minimal) peak of [d] *)
 val label : (int -> step -> int * step) -> t -> t
 (** [label f d] labels diagram [d] according to labeling function [f] *)
 val make : (step * step) -> (step list * step list) list -> t
 (* [make p js] returns a diagram with peak [p] and joins [js] *)
  
 (** Predicates **)
 val is_decreasing : t -> bool
 (** [is_decreasing d] tests if [d] is decreasing *)

 (** Printers **)
 val fprintfm : Format.formatter -> t -> unit m
 (** [fprintfm fmt t] pretty prints [t] into [fmt] *)

end

(** Diagrams
@author Harald Zankl
@since  Mon Dec 23 14:44:35 CEST 2010 *)

(** This module provides several functions that allow to rewrite terms
or that are based on functions which rewrite terms. *)
(*
module Diagram: DIAGRAM with 
 type 'a m = 'a Monad.t and type term = Term.t and type trs = Trs.t and
 type step = Rewrite.step and type sequence = Rewrite.sequence and type
 join = Rewrite.join
 *)

(** {2 Functor Make} *)

module type LABEL = sig
 (*** MODULES *****************************************************************)
 module F : Util.Index.ISOMORPHIC with
  type key = Function.t and type element = string

 module V : Util.Index.ISOMORPHIC with
  type key = Variable.t and type element = string

 (*** TYPES *******************************************************************)
 type signature = F.t * V.t
 type t

 (*** VALUES ******************************************************************)
 val compare : t -> t -> int
 (** [compare l k] compares [l] and [k]. It must be guaranteed that
 [compare l k = 0] if [l] and [k] are equal, [compare l k < 0] if [l]
 is smaller than [k], and [compare l k > 0] if [l] is greater than [k]. *)
 val copy : t -> t
 (** [copy l] copies the value [l]. *)
 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt l] prints [l] using the [OCaml] module [Format]. *)
 val fprintfs : (Format.formatter -> Function.t -> signature -> signature)
  -> (Format.formatter -> Variable.t -> signature -> signature)
  -> Format.formatter -> t -> signature -> signature
 (** [fprintfs f g fmt l s] prints [l] using the [OCaml] module [Format].
 In difference to {!val: Rewriting.LABEL.fprintf}, this function uses the
 information stored in the signature [s] to print function symbols and
 variables. *)
 val hash : t -> int
 (** [hash l] returns the hash value of [l]. It must be guaranteed that
 [hash l = hash k] whenever [compare l k = 0]. *)
end

module type STATEX = sig
 (*** TYPES *******************************************************************)
 type label
 type signature

 (*** INCLDUES ****************************************************************)
 include STATE

 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val label : ?arity:int -> Function.t -> label -> t -> Function.t * t
 (** [label ~arity:a f l s] returns a pair consisting of an [a]-ary function
 symbol which represents the function symbol [f] labeled with [l] and the
 new signature. If [s] contains already a binding with respect to [f] and
 [l] then this function symbol is returned. In that case the arity of the
 function symbol is set to [a] if its arity has not been defined so far.
 If no arity is specified, the arity of [f] is taken. Note that as a side
 effect the old signature is changed too. *)
 val replace_lab : Function.t -> label -> t -> t
 (** [replace_lab f l s] replaces the label of [f] by [l]. Note that as a
 side effect the old signature is changed too.
 @raise Failure "unlabeled function symbol" if [f] has no label. *)

 (** {3 Search Functions} *)

 val drop_lab : (label -> bool) -> Function.t -> t -> Function.t
 (** [drop_lab p f s] unlabels [f] as long as [p] is not satisfied. As soon
 as [p] is satisfied, the corresponding function symbol is returned.
 @raise Not_found if no such symbol is found. *)
 val find_lab : Function.t -> t -> label
 (** [find_lab f s] returns the label of the function symbol [f] with respect
 to the signature [s].
 @raise Not_found if [s] does not contain a binding for [f]. *)
 val labs : Function.t -> t -> label list
 (** [labs f s] returns all labels of the function symbol [f]. *)
 val origin : Function.t -> t -> Function.t
 (** [origin f s] returns the origin of [f], i.e., the topmost function symbol
 that cannot be unlabeled any more. *)
 val pick_lab : (label -> bool) -> Function.t -> t -> label
 (** [pick_lab p f s] returns the first label of [f] which satisfies [p].
 @raise Not_found if no such label is found. *)
 val search_lab : (label -> bool) -> Function.t -> t -> Function.t * label
 (** [search_lab p f s] unlabels [f] as long as [p] is not satisfied. As soon
 as [p] is satisfied, the corresponding label and function symbol is returned.
 @raise Not_found if no such symbol is found. *)
 val unlabel : Function.t -> t -> Function.t
 (** [unlabel f s] unlabels the function symbol [f].
 @raise Not_found if [f] cannot be unlabeled. *)

 (** {3 Scan Functions} *)

 val is_lab : (label -> bool) -> Function.t -> t -> bool
 (** [is_lab p f s] checks if [f] admits a label that satisfies [p]. *)
 val is_labeled : Function.t -> t -> bool
 (** [is_labeled f s] checks if [f] is a labeled function symbol. *)
 val is_unlabeled : Function.t -> t -> bool
 (** [is_unlabeled f s] checks if [f] has no labels. *)

 (** {3 Miscellaneous} *)

 val set_signature : signature -> t -> t
 (** [set_signature x s] replaces the indices of [s] which store the names of
 function symbols and variables. *)
 val to_signature : t -> signature
 (** [to_signature s] drops all information except the indices which store the
 names of function symbols and variables. *)
end

module type MONADX = sig
 (*** TYPES *******************************************************************)
 type label

 (*** INCLDUES ****************************************************************)
 include MONAD

 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val label : ?arity:int -> Function.t -> label -> Function.t t
 (** [label ~arity:a f l] is equivalent to {!val: Rewriting.STATEX.label} except
 that the result is encapsulated in a monad. Note that as a side effect the
 old signature is changed too. *)
 val replace_lab : Function.t -> label -> unit t
 (** [replace_lab f l] is equivalent to {!val: Rewriting.STATEX.replace_lab}
 except that the result is encapsulated in a monad. Note that as a side effect
 the old signature is changed too. If the function symbol [f] has no label,
 the returned monad represents the error "unlabeled function symbol". *)

 (** {3 Search Functions} *)

 val drop_lab : (label -> bool) -> Function.t -> Function.t t
 (** [drop_lab p f] is equivalent to {!val: Rewriting.STATEX.drop_lab} except
 that the result is encapsulated in a monad. If the function symbol [f] does
 not admit an appropriate label, the returned monad represents the error
 "not found". *)
 val find_lab : Function.t -> label t
 (** [find_lab f] is equivalent to {!val: Rewriting.STATEX.find_lab} except
 that the result is encapsulated in a monad. If the function symbol [f] does
 not admit an appropriate label, the returned monad represents the error
 "not found". *)
 val labs : Function.t -> label list t
 (** [labs f] is equivalent to {!val: Rewriting.STATEX.labs} except that the
 result is encapsulated in a monad. *)
 val origin : Function.t -> Function.t t
 (** [origin f] is equivalent to {!val: Rewriting.STATEX.origin} except that
 the result is encapsulated in a monad. *)
 val pick_lab : (label -> bool) -> Function.t -> label t
 (** [pick_lab p f] is equivalent to {!val: Rewriting.STATEX.pick_lab} except
 that the result is encapsulated in a monad. If the function symbol [f] does
 not admit an appropriate label, the returned monad represents the error
 "not found". *)
 val search_lab : (label -> bool) -> Function.t -> (Function.t * label) t
 (** [search_lab p f] is equivalent to {!val: Rewriting.STATEX.search_lab}
 except that the result is encapsulated in a monad. If the function symbol [f]
 does not admit an appropriate label, the returned monad represents the error
 "not found". *)
 val unlabel : Function.t -> Function.t t
 (** [unlabel f] is equivalent to {!val: Rewriting.STATEX.unlabel} except that
 the result is encapsulated in a monad. If the function symbol [f] cannot be
 unlabeled, the returned monad represents the error "not found". *)

 (** {3 Scan Functions} *)

 val is_lab : (label -> bool) -> Function.t -> bool t
 (** [is_lab p f] is equivalent to {!val: Rewriting.STATEX.is_lab} except that
 the result is encapsulated in a monad. *)
 val is_labeled : Function.t -> bool t
 (** [is_labeled f] is equivalent to {!val: Rewriting.STATEX.is_labeled}
 except that the result is encapsulated in a monad. *)
 val is_unlabeled : Function.t -> bool t
 (** [is_unlabeled f] is equivalent to {!val: Rewriting.STATEX.is_unlabeled}
 except that the result is encapsulated in a monad. *)
end

module type SIGNATURE = sig
 (*** TYPES *******************************************************************)
 type label
 type signature

 (*** MODULES *****************************************************************)
 module Function : FUNCTION with type t = Function.t
 module Variable : VARIABLE with type t = Variable.t

 module Signature : STATEX with
  type label = label and type signature = signature

 module Monad : MONADX with type label = label and type state = Signature.t
 module Position : POSITION with type t = Position.t

 module Parser : PARSER with
  type state = Signature.t and type input = Parsec.StringInput.t

 module Term : TERM with
  type 'a m = 'a Monad.t and type 'a p = 'a Parser.t and
  type 'a x = 'a Parser.Xml.t and type state = Signature.t

 module Context : CONTEXT with type 'a m = 'a Monad.t and type term = Term.t 

 module Substitution : SUBSTITUTION with
  type 'a m = 'a Monad.t and type term = Term.t and type context = Context.t

 module Elogic : ELOGIC with
  type 'a m = 'a Monad.t and type substitution = Substitution.t and
  type term = Term.t

 module Rule : RULE with
  type 'a m = 'a Monad.t and type 'a p = 'a Parser.t and
  type substitution = Substitution.t and type term = Term.t and
  type 'a x = 'a Parser.Xml.t

 module Trs : TRS with
  type 'a m = 'a Monad.t and type 'a p = 'a Parser.t and
  type rule = Rule.t and type term = Term.t and type 'a x = 'a Parser.Xml.t

 module Rewrite : REWRITE with
  type 'a m = 'a Monad.t and type term = Term.t and type trs = Trs.t and
  type rule = Rule.t and type position = Position.t

 module Diagram : DIAGRAM with 
  type 'a m = 'a Monad.t and type term = Term.t and type trs = Trs.t and
  type step = Rewrite.step 
end

(** Generate Rewriting Library
@author Martin Korp
@since  Mon Dec 23 14:44:35 CEST 2008 *)

(** This functor builds a rewriting module based on labeling module [L] of
module type {!modtype: Rewriting.LABEL}. *)
module Make (L : LABEL) : SIGNATURE with
 type label = L.t and type signature = L.signature
