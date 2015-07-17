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

(** Propositional and Arithmetical Manipulation Library
@author Harald Zankl
@since  Tue Jan 13 13:50:26 CET 2009 *)

(** This library allows to formulate expressions in arithmetic (most binary
operators are available via the module {!Operators} and provides three
solvers (some of which do not support the full format). [Yices] and [MiniSat+]
can just solve linear arithmetical constraints. Furthermore our interface to
[Yices] does currently not support constant non-integer (i.e. rational)
values. *)

(*** MODULES ******************************************************************)
(**/**)
module State : sig type t end
module Monad : Util.Monad.STATE_MONAD with type state = State.t
(**/**)

(** {2 Module Number} *)

(** Numbers
@author Harald Zankl
@since  Tue Jan 13 13:51:21 CET 2009 *)

(** This module deals with rational, real, ... numbers. Data is represented as
a reduced fraction with positive denumerator. *)
module Number: sig
 (*** TYPES *******************************************************************)
 type t
 (** type of a number *)

 (*** VALUES ******************************************************************)
 (** {3 Constructors} *)

 val one : t
 (** [one] returns constant [1]. *)
 val two : t
 (** [two] returns constant [2] *)
 val zero : t
 (** [zero] returns constant zero. *)
 val of_int : int -> t
 (** [of_int n] returns constant [n]. *)
 val of_int_string : string -> t
 (** [of_int_string n] returns constant [n]. *)
 val of_big_int : Big_int.big_int -> t
 (** [of_big_int n] returns constant [n]. *)
 val of_64_int : Int64.t -> t
 (** [of_64_int n] returns constant [n]. *)
 val of_big_rat : Big_int.big_int -> Big_int.big_int -> t
 (** [of_rat n d] returns rational with numerator [n] and denominator [d]. 
 @raise Failure "Rational.denumerator zero" if denumerator [d] is zero. *)
 val of_big_real : ?base:int -> (Big_int.big_int * Big_int.big_int) -> 
  (Big_int.big_int * Big_int.big_int) -> t
 (** [of_real b (c0,d0) (c1,d1)] returns real number represented by
  [c0/d0 + sqrt(b)c1/d1], default b = 2*)
 val get_minf : unit -> t
 (** [get_minf ()] returns a value representing minus infinity *)

 (** {3 Combinators} *)

 val minus : t -> t
 (** unary minus *)
 val add : t -> t -> t
 (** addition *)
 val div : t -> t -> t
 (** division *)
 val mul : t -> t -> t
 (** multiplication *)
 val sub : t -> t -> t
 (** subtraction *)
 val scale : Big_int.big_int -> t -> t
 (** [scale n r] multiplies integer [n] with number [r]. *)
 val min : t -> t -> t
 (** minimum operation *)
 val max : t -> t -> t
 (** maximum operation *)

 (** {3 Comparisons} *)

 val compare : t -> t -> int
 (* compares two numbers. not that for real numbers the comparison is
 not exact but an approximation *)
 val eq : t -> t -> bool
 (** test for equality *)
 val ge : t -> t -> bool
 (** abbreviation for [gt || ge] *)
 val gt : t -> t -> bool
 (** test for greater. note that for real numbers the comparison is not
 exact but an approximation. *)
 val is_zero : t -> bool
 (** [is_zero r] returns [true] iff [r] is zero. *)
 val is_one : t -> bool
 (** [is_one r] returns [true] iff [r] is one. *)
 val is_minf : t -> bool
 (** [is_minf r] returns [true] iff [r] is minus infinity. *)

 (** {3 Destructors} *)

 val to_string : t -> string
 (** [to_string r] returns [r] as string. 
 *)
 val to_int : t -> int
 (** [to_int r] returns [r] as integer. 
 @raise Failure "to_int: not an integer" if [r] is not an integer.
 *)
 val to_rat : t -> Big_int.big_int * Big_int.big_int
 (** [to_rat r] returns pair [(n,d)] of nominator and denumerator. 
 @raise Failure "to_rat: not a rational" if [r] contains real part
 @raise Failure "to_rat: not a rational" if [r] is minus infinity.
 *)
 val to_real : t -> (Big_int.big_int * Big_int.big_int) *
 (Big_int.big_int * Big_int.big_int) * int
 (** [to_real r] returns triple [((c0,d0),(c1,d1),b] where
 [r = c0/d0+sqrt(b)c1/d1].
 @raise Failure "to_real: not a real" if [r] is minus infinity.
 *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf ppf r] writes pretty output of [r] into buffer [ppf]. *)
 val fprintfx : Format.formatter -> t -> unit
 (** [fprintfx ppf r] writes certified output of [r] into buffer [ppf]. *)
 val to_string : t -> string
 (** [to_string r] returns pretty output of [r] as string. *)

 val fprintf_intp : Format.formatter -> (string * int * t list * t) -> unit
 (** [fprintf ppf r] writes pretty output of [r] into buffer [ppf]. *)
 val fprintfx_intp : int -> Format.formatter -> (string * int * t list * t) -> unit
 (** [fprintf ppf r] writes pretty output of [r] into buffer [ppf]. *)

end

(*** TYPES ********************************************************************)
(** {2 Types} *)

type a 
(** arithmetic expressions *)

type arith = {
 min : Int64.t;
 (** minimal value that must be representable 
  - [MiniSat] uses as many bits as needed to represent [min]
  - [MiniSat+] uses as many bits as needed to represent [min]
  - [Yices] ignores this *)
 neg : bool;
 (** allow negative values 
  - [MiniSat] allows negative numbers (two complement) if [neg] is set
  - [MiniSat+] ignores this 
  - [Yices] allows negative numbers if [neg] is set *)
 rat : int;
 (** denumerator for rational values 
  - [MiniSat] uses denumerator [rat]
  - [MiniSat+] ignores this 
  - [Yices] uses any denumerator if [rat <> 1] *)
 real : bool;
 (** use reals
  - [MiniSat] represents reals as [c+d*sqrt(2)] if [real] is set
  - [MiniSat+] ignores this 
  - [Yices] ignores this *)
 minf : bool;
 (** reals augmented by minus infinity
  - [MiniSat] allows minus infinity if [minf] is set
  - [MiniSat+] ignores this 
  - [Yices] ignores this *)
};;
(** arithmetical specification *)

type p
(** propositional logic formulas *)

type solver =
 | MiniSat  (** use [MiniSat] as back-end *)
 | MiniSatP (** use [MiniSat+] as back-end *)
 | MiniSmt of string list  (** use [MiniSmt] as back-end (experimental *)
 | Yices    (** use [Yices] as back-end *)
(** type for solver (back-end) *)

type assignment
(** type for assignments *)

(*** VALUES *******************************************************************)
(** {2 Monadic Operations} *)

val (>>=) : 'a Monad.t -> ('a -> 'b Monad.t) -> 'b Monad.t
(** bind operator *)
val return : 'a -> 'a Monad.t
(** [return a] lifts [a] to monadic value. *)
val run : ?dbits:int -> ?obits:int -> 'a Monad.t -> 'a
(** [run ~dbits:db ~obits:ob m] evaluates [m]. 
 - [dbits] indicates how many bits are used after decimal point for intermediate results
  only used by [MiniSat]
 - [obits] indicates how many bits are used for intermediate results. only
 used by [MiniSat] 
 
 *)

(** {2 Constructors} *)

val bot : p
(** constant false *)
val top : p
(** constant true *)

val zero : a
(** constant zero *)
val one : a
(** constant one *)
val minus_one : a
(** constant minus one. *)
val minf : a
(** minus infinity *)
val constant : Number.t -> a
(** [constant r] lifts [r] to type [a] *)

(** {2 Combinators} *)

val neg : p -> p 
(** [neg] represents negation *)
val xor : p -> p -> p
(** [xor] represents exclusive or *)
val min : a -> a -> a
(** [min a b] represents minimum of [a] and [b] *)
val max : a -> a -> a
(** [max a b] represents maximum of [a] and [b] *)
val ite : p -> a -> a -> a
(** [ite x a b] returns [a] if [x] evaluates to [true] and [b] otherwise. *)
val scale : Number.t -> a -> a
(** [scale c a] returns product of [c] and [a]. *)

val big_and : p list -> p
(** [big_and ls] is conjunction of all elements in [ls]. *)
val big_or : p list -> p
(** [big_or ls] is disjunction of all elements in [ls]. *)
val big_sum : a list -> a
(** [big_sum ls] is sum of all elements in [ls]. *)

val obits : int -> p -> p
(** [obits n p] uses at most [n] bits to (represent intermediate results to) evaluate [p] *)

(** {2 Printers} *)

val fprintf_a : Format.formatter -> a -> unit
(** [fprintf_a ppf a] writes pretty output of [a] into [ppf]. *)
val fprintf_p : Format.formatter -> p -> unit
(** [fprintf_p ppf p] writes pretty output of [p] into [ppf]. *)
val fprintf_smt : ?logic:string -> Format.formatter -> p -> unit
(** [fprintf_smt ppf p] writes [p] into [ppf] in SMT-LIB format. *)

(** {2 Getting Variables} *)

(** For 
 - [Yices] variables of different type may not be mixed
 - [MiniSatP] no boolean variables are allowed *)

val fresh_bool : p Monad.t
(** [fresh_bool] returns fresh propositional variable *)
val fresh_arith : arith -> a Monad.t
(** [fresh_arith spec] returns fresh arithmetical variable according to
specification [spec]. *)
val fresh : a -> a
(** [fresh a] associates a fresh variable with [a] *)
val cache_bool : ('a,p) Hashtbl.t -> 'a -> p Monad.t
(** [cache_bool tbl k] associates a propositional variable to [k] *)
val cache_arith : ('a,a) Hashtbl.t -> arith -> 'a -> a Monad.t
(** [cache_arith tbl spec k] associates an arithmetical variable with
    [k]. The type and possible values of the variable are specified by
    [spec]. *)

(** {2 Solving and Solution Extraction} *)

val solve : ?solver:solver -> ?goal:a option -> p -> assignment option Monad.t
(** [solve ~solver:s ~goal:g f] 
 - tries to solve formula [f] (using solver [s] (default is [MiniSat]))
 - goal function (which is minimized) is only supported by [MiniSatP]
 - returns [None] if [f] is not satisfiable and [Some assign] otherwise *)
val eval_a : a -> assignment -> Number.t Monad.t
(** [eval_a a ass] returns value of [a] wrt [ass]. *)
val eval_p : p -> assignment -> bool Monad.t
(** [eval_p f ass] returns value of [f] wrt [ass]. *)

val negate_assignment : assignment -> p

(** {2 Conveniences } *)

val nat : int -> arith
(** [nat n] returns arithmetical specification that can represent
numbers from [0] to [n]. *)
val int : int -> arith
(** [int n] returns arithmetical specification that can represent 
numbers from [-n] to [n-1]. *)

val set_print_formula : unit -> unit
(** [set_print_formula ()] for internal use *)

(**/**)
val fprintf_assignment : Format.formatter -> assignment -> unit
(** just for testing *)
(**/**)

(*** MODULES ******************************************************************)
(** {2 Module Operators} *)

(** Logic operators
@author Harald Zankl
@since  Tue Jan 13 13:51:21 CET 2009 *)

(** This module contains the binary logic connectives (operators). Usually one
will use [open Operators] to have them available also in infix notation.
Note that operators have no specified binding priority, so please use
parenthesis! *)
module Operators : sig
 (*** VALUES ******************************************************************)
 (** {3 Operators} *)

 val (<&>) : p -> p -> p 
 (** logical and *)
 val (<|>) : p -> p -> p
 (** logical or *)
 val (<<->>) : p -> p -> p
 (** logical if and only if *)
 val (<->>) : p -> p -> p
 (** logical implication *)
 val (<<->) : p -> p -> p
 (** logical left-implication *)
 val (~!) : p -> p 
 (** logical not *)
 val (<+>) : a -> a -> a
 (** arithmetical addition *)
 val (<->) : a -> a -> a
 (** arithmetical subtraction *)
 val (<*>) : a -> a -> a
 (** arithmetical multiplication. only scalar multiplication supported for
 [MiniSatP] and [Yices] backend *)
 val (<=>) : a -> a -> p
 (** arithmetical equality *)
 val (<>>) : a -> a -> p
 (** arithmetical greater *)
 val (<>=>) : a -> a -> p
 (** arithmetical greater-equal *)
 val (<<>) : a -> a -> p
 (** arithmetical smaller *)
 val (<<=>) : a -> a -> p
 (** arithmetical smaller-equal *)
 val (<?>) : p -> a -> a -> a
 (** [x <?> a <:> b] is alias for [ite x a b]. *)
 val (<:>) : (a -> a) -> a -> a
 (** [x <?> a <:> b] is alias for [ite x a b]. *)
end
