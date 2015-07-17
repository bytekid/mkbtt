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

(** TTT2 Library
@author Martin Korp
@since  Mon Mar 16 10:48:58 CET 2009 *)

(** This library provides some functions which can be used to prove
(non)termination of a given system. In order to be able to combine
different techniques, a strategy language is provided. *)

(*** MODULES ******************************************************************)
(** {2 Module Strategy} *)

(** Strategy
@author Martin Korp
@since  Mon Sep  1 12:22:31 CEST 2008 *)

(** This module contains all constructs which are used to define the strategy
of TTT2. *)
module Strategy : sig
 (*** MODULES *****************************************************************)
 (** {3 Module Syntax} *)
 
 (** Abstract Syntax Tree
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the abstract syntax tree used to represent strategies. *)
 module Syntax : sig
  (*** TYPES ******************************************************************)
  type condition =
   | Atom of string * string list
   | Not of condition
   | And of condition * condition
   | Or of condition * condition
  
  type modifier = string * string list
  
  type t =
   | Strategy of string * string list
   (* combinators *)
   | Combine of t * t
   | Choose of t * t
   | Parallel of t * t
   | Condition of condition * t * t
   (* iterators *)
   | Optional of t
   | Iterate of t
   | Repeat of t
   | Replicate of int * t
   | Iterate_timed of float * t
   (* specifiers *)
   | Modify of t * modifier
   | Stop of t
   | Strict of t
   | Timed of float * t
 
  (*** VALUES *****************************************************************)
  (** {4 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt s] prints [s] using the [OCaml] module [Format]. Not
  tail-recursive. *)
  val to_string : t -> string
  (** [to_string s] returns a string that represents [s]. Not tail-recursive. *)
 end

 (** {3 Module Parser} *)
 
 (** Strategy Parser
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the parser used to parse strategies. *)
 module Parser : sig
  (*** TYPES ******************************************************************)
  type token =
   | AND
   | COLON
   | COMMA
   | ELSE
   | EOF
   | EXCLAMATION_MARK
   | GREATER
   | IF
   | LEFT_BRACE
   | LEFT_BRACKET
   | LEFT_PAREN
   | PERCENT
   | PIPE
   | PLUS
   | QUESTION_MARK
   | RIGHT_BRACE
   | RIGHT_BRACKET
   | RIGHT_PAREN
   | SEMICOLON
   | SMALLER
   | STAR
   | THEN
   | FLOAT of float
   | ID of string
   | INT of int

  (*** VALUES *****************************************************************)
  val strategy : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Syntax.t
  (** [strategy l s] parser the strategy [s] by using the lexer [l].
  @raise Parsing.Parse_error if the given strategy is not proper defined. *)
 end

 (** {3 Module Lexer} *)
 
 (** Strategy Lexer
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the lexer used to tokenize strategies. *)
 module Lexer : sig
  (*** VALUES *****************************************************************)
  val token : Lexing.lexbuf -> Parser.token
  (** [token s] returns the current token of the strategy [s]. *)
 end

 (** {3 Module Status} *)
 
 (** Processor Status
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** This module defines the possible states of an applied processor. We differ
 between successful and not successful proofs. A processor is successful if
 it could simplifies the given problem or prove termination or nontermination
 of it. Otherwise the applied processor failed. *)
 module Status : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Constructors} *)
  
  val fail : t
  (** [fail] specifies that the used strategy failed. *)
  val nonterminating : t
  (** [nonterminating] specifies that the used strategy proved
  nontermination. *)
  val terminating : t
  (** [terminating] specifies that the used strategy proved termination. *)
  val unfinished : t
  (** [unfinished] specifies that the used strategy was not able to finish
  the proof so far. *)
  
  (** {4 Properties} *)

  val is_complete : t -> bool
  (** [is_complete s] checks if the given problem has been proved termination
  or nonterminating by the given strategy. *)
  val is_fail : t -> bool
  (** [is_fail s] checks if the given strategy failed to prove termination
  or nontermination. *)
  val is_nonterminating : t -> bool
  (** [is_nonterminating s] checks if the given problem has been proved
  nonterminating by the given strategy. *)
  val is_unfinished : t -> bool
  (** [is_unfinished s] checks if the termination proof of the given problem
  is unfinished. *)
  val is_success : t -> bool
  (** [complete s] checks if the given problem has been proved termination
  or nonterminating by the given strategy. *)
  val is_terminating : t -> bool
  (** [complete s] checks if the given problem has been proved termination
  or nonterminating by the given strategy. *)

  (** {4 Miscellaneous} *)

  val collect : t -> t -> t
  (** [collect s s'] combines the status [s] and [s'] in a conjunctive way.
  That means if both [s] and [s'] have status terminating, then the resulting
  status is set to terminating. If [s] or [s'] have status nonterminating then
  the resulting status is set to nonterminating. Otherwise either status fail
  or open are returned. *)
  val combine : t -> t -> t
  (** [combine s s'] combines the status [s] and [s'] in a disjunctive
  way. That means if [s'] has status terminating (nonterminating, fail) then
  the resulting status is set to terminating (nonterminating, fail). Otherwise
  [s] is returned. Note that the order of the arguments is important, i.e., [s]
  defines some new status that should be combined with some old status [s']. *)

  (** {4 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt s] prints [s] using the [OCaml] module [Format]. *)
  val to_string : t -> string
  (** [to_string s] returns a string that represents [s]. *)
 end

 (** {3 Module Processor} *)
 
 (** Register Processors
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the processors that can be used to prove or disprove termination
 of a given problem. *)
 module Processor : sig
  (*** TYPES ******************************************************************)
  type t
  
  (*** VALUES *****************************************************************)
  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the processor [p] and [q] are equal. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t
   -> Util.Complexity.t option
  (** [complexity c p] returns information regarding the complexity of the new
  termination problem obtained by applying processor [p]. Note that [None] is
  returned if [p] proves nontermination. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Processors.Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Processors.Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] and the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
  val fprintfx : Status.t
   -> (Format.formatter -> unit Processors.Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Processors.Rewritingx.Monad.t
  (** [fprintfx s fs fmt p] prints [p] and the corresponding proofs in XML
  using the [OCaml] module [Format]. Function symbols and variables are
  represented by their names registered in the underlying signature. If the
  name of a function symbol or variable is not defined, a randomly generated
  name is printed. Note that as a side effect copies of the underlying
  signature are changed too. This function is not tail-recursive.
  @raise Failure "not supported" if some proccessor is used which does not
  support XML output. *)
 end

 (** {3 Module Proof} *)
 
 (** Proof Objects
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the internal format of the generated proof objects. Furthermore
 it provides various functions to create or modify proof objects. *)
 module Proof : sig
  (*** TYPES ******************************************************************)
  type t
  
  (*** VALUES *****************************************************************)
  (** {4 Constructors} *)

  val append : t -> t -> t
  (** [append p q] appends the proof [p] to [q], i.e., a proof of the form
  [pq] is returned. Not tail-recursive.
  @raise Failure "finished proof object" if [q] is a complete proof. *)
  val finished : t
  (** [finished] defines the end of a proof. *)
  val make : Processor.t -> t list -> t
  (** [make pr ps] creates a new proof consisting of the processor [pr] and
  the proofs [ps]. *)
  val merge : t list -> t
  (** [merge ps] combines the proofs in [ps]. Not tail-recursive.
  @raise Failure "empty list" if [ps] is empty.
  @raise Failure "conflicting proof structures" if two proofs in [ps] cannot
  be merged. *)
  val optimize : Processor.t -> t -> t -> t
  (** [optimize pr p q] optimizes the proof [p] by replacing the
  processor [pr] in [p] by the proof [q]. Not tail-recursive.
  @raise Failure "illegal input" if [pr] cannot be optimized. Such a
  situation occurs if [pr] is not a complexity problem or [pr] is not
  a termination processor. *)
  val unfinished : t
  (** [unfinished] defines an open proof. *)

  (** {4 Complexity Bounds} *)

  val complexity : t -> Util.Complexity.t option
  (** [complexity p] returns information regarding the complexity of the
  initial termination problem of proof [p]. Note that [None] is returned
  either if the proof is not complete or nontermination has been proved.
  Not tail-recursive. *)
  val critical : t -> (Util.Complexity.t * Processor.t list) option
  (** [critical p] returns the critical processors of the proof [p]
  which contribute the highest (intermediate) complexity bounds.
  Not tail-recursive. *)
 
  (** {4 Printers} *)

  val fprintf : Format.formatter -> t -> unit Processors.Rewritingx.Monad.t
  (** [fprintf fmt p] prints [p] using the [OCaml] module [Format].
  Function symbols and variables are represented by their names registered
  in the underlying signature. If the name of a function symbol or variable
  is not defined, a randomly generated name is printed. Note that as a side
  effect copies of the underlying signature are changed too. This function
  is not tail-recursive. *)
  val fprintfx : Status.t -> Format.formatter -> t
   -> unit Processors.Rewritingx.Monad.t
  (** [fprintfx fmt p] prints [p] in XML using the [OCaml] module [Format].
  Function symbols and variables are represented by their names registered
  in the underlying signature. If the name of a function symbol or variable
  is not defined, a randomly generated name is printed. Note that as a side
  effect copies of the underlying signature are changed too. This function
  is not tail-recursive. *)
  val to_string : t -> string Processors.Rewritingx.Monad.t
  (** [to_string p] returns a formatted string that represents [p].
  Function symbols and variables are represented by their names registered
  in the underlying signature. If the name of a function symbol or variable
  is not defined, a randomly generated name is used. Note that as a side
  effect copies of the underlying signature are changed too. This function
  is not tail-recursive. *)
  val to_stringx : Status.t -> t -> string Processors.Rewritingx.Monad.t
  (** [to_stringx p] returns a formatted string that represents [p] in XML.
  Function symbols and variables are represented by their names registered
  in the underlying signature. If the name of a function symbol or variable
  is not defined, a randomly generated name is used. Note that as a side
  effect copies of the underlying signature are changed too. This function
  is not tail-recursive. *)
 end

 (** {3 Module State} *)
 
 (** Proof Data
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the intermediate proof states constructed by TTT2. *)
 module State : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Constructors} *)

  val make : Processors.Problem.t -> Proof.t -> Status.t -> t
  (** [make problem proof s] creates a new state consisting of the problem
  [problem], the proof [proof], and the status [s]. *)
  val set_problem : Processors.Problem.t -> t -> t
  (** [set_problem p s] replaces the problem of [s] by [p]. *)
  val set_proof : Proof.t -> t -> t
  (** [set_proof p s] replaces the proof of [s] by [p]. *)
  val set_status : Status.t -> t -> t
  (** [set_status status s] replaces the status of [s] by [status]. *)
  val update_status : Status.t -> t -> t
  (** [update_status status s] combines the old status of [s] with the new
  status by using the function {!val: Ttt2.Strategy.Status.combine}. *)

  (** {4 Access Functions} *)

  val get_problem : t -> Processors.Problem.t
  (** [get_problem s] returns the problem of [s]. *)
  val get_proof : t -> Proof.t
  (** [get_proof s] returns the proof of [s]. *)
  val get_status : t -> Status.t
  (** [get_status s] returns the status of [s]. *)

  (** {4 Printers} *)

  val fprintf : Format.formatter -> t -> unit Processors.Rewritingx.Monad.t
  (** [fprintf fmt p] prints [p] using the [OCaml] module [Format].
  Function symbols and variables are represented by their names registered
  in the underlying signature. If the name of a function symbol or variable
  is not defined, a randomly generated name is printed. Note that as a side
  effect copies of the underlying signature are changed too. This function
  is not tail-recursive. *)
  val to_string : t -> string Processors.Rewritingx.Monad.t
  (** [to_string p] returns a formatted string that represents [p].
  Function symbols and variables are represented by their names registered
  in the underlying signature. If the name of a function symbol or variable
  is not defined, a randomly generated name is used. Note that as a side
  effect copies of the underlying signature are changed too. This function
  is not tail-recursive. *)
 end

 (** {3 Module Monad} *)
 
 (** Combinator Monad
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** This module defines a combinator monad that is used to evaluate a given
 strategy. *)
 module Monad : sig
  (*** INCLUDES ***************************************************************)
  include Util.Monad.SIGNATURE

  (*** TYPES ******************************************************************)
  type state = State.t

  (*** VALUES *****************************************************************)
  (** {4 Constructors} *)

  val liftr : 'a Processors.Rewritingx.Monad.t -> 'a t
  (** [liftr m] lifts monad [m] to an instance of type [t]. *)
  val result : ('a * State.t) list -> 'a t
  (** [result r] constructs an instance of type [t] with result [r]. *)

  (** {4 State Modifications} *)

  val get : state t
  (** [get] is equivalent to [modify id]. *) 
  val modify : (state -> state) -> state t
  (** [modify f] modifies the current state via the function [f] and returns
  the new state as result. It is equivalent to [update f >> get]. *)
  val set : state -> unit t
  (** [set s] is equivalent to [update (const s)]. *)
  val update : (state -> state) -> unit t
  (** [update f] updates the current state by applying [f] to it. *)
  val with_state : (state -> state) -> 'a t -> 'a t
  (** [with_state f m] modifies the state of [m] via the function [f]. *)

  (** {4 Evaluation Functions} *)
  
  val run : state -> Processors.Rewritingx.Signature.t -> 'a t
   -> Proof.t * Processors.Rewritingx.Signature.t * Status.t
  (** [run s s' m] evaluates [m] under [s] and [s'] and returns the resulting
  proof, status and signature.
  @raise Failure "conflicting proof structures" if the resulting proofs could
  not be merged.
  @raise Failure "error '...' occured" if an error occured during the
  unfolding of the internal monadic structure. Detailed information about
  the occured error are given within the ticks. *)

  (** {4 Combinators} *)

  val choose : ('a -> 'b t) -> ('a -> 'b t) -> ('a -> 'b t)
  (** [choose f g] applies [f] to the given problem. If this succeeds, its
  result is returned. Otherwise [g] is applied to the given problem. *)
  val combine : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
  (** [combine f g] first applies [f] to the given problem. If this fails,
  then [combine f g] fails. Otherwise [g] is applied to the resulting
  problems. *)
  val condition : bool t -> ('a -> 'b t) -> ('a -> 'b t) -> ('a -> 'b t)
  (** [condition p f g] applies [f] to the given problem if [p] evaluates to
  [true]. Otherwise [g] is applied. *)
  val parallel : ('a -> 'a t) -> ('a -> 'a t) -> ('a -> 'a t)
  (** [parallel f g] runs [f] and [g] in parallel on the given problem. As
  soon as at least one of [f] and [g] suceeds, the resulting problem is
  returned. If both [f] and [g] fail, [parallel f g] fails. *)

  (** {4 Iterators} *)

  val duplicate : int -> ('a -> 'a t) -> ('a -> 'a t)
  (** [duplicate n f] applies [f] recursively to the given problem until it
  cannot be modified any more or [f] has been applied [n]-times. Note that
  [duplicate t f] is always successful. *)
  val iterate : ('a -> 'a t) -> ('a -> 'a t)
  (** [iterate f] applies [f] recursively to the given problem until it cannot
  be modified any more. Note that [iterate f] is always successful. *)
  val iterate_timed : float -> ('a -> 'a t) -> ('a -> 'a t)
  (** [iterate_timed t f] applies [f] recursively to the given problem until
  it cannot be modified any more or [t] seconds are elapsed. Note that
  [iterate_timed t f] is always successful. *)
  val optional : ('a -> 'a t) -> ('a -> 'a t)
  (** [optional f] applies [f] to the given problem. On success its result is
  returned. Otherwise the original problem is returned unmodified. *)
  val repeat : ('a -> 'a t) -> ('a -> 'a t)
  (** [repeat f] applies [f] recursively to the given problem until it cannot
  be modified any more. I.e., [repeat f] is successful if it can prove or
  disprove termination of the given problem. Otherwise it fails. Note that
  [repeat f = combine (iterate f) f]. *)

  (** {4 Specifiers} *)

  val alter : ('a -> 'b t) -> (Processors.Problem.t -> 'b -> 'c t)
   -> ('a -> 'c t)
  (** [alter f g] first applies [f] to the given problem. If this fails,
  then [alter f g] fails. Otherwise the modifier [g] is applied to the
  resulting problems. *)
  val stop : ('a -> 'b t) -> ('a -> 'b t)
  (** [stop f] applies [f] to the given problem. If [f] fails, the computation
  is aborted and [stop f] fails. Otherwise it is sucessful. *)
  val strict : ('a -> 'a t) -> ('a -> 'a t)
  (** [strict f] applies [f] to the given problem. If [f] proves or disproves
  termination of the given problem, [strict f] is successful. Otherwise it
  fails. *)
  val timed : float -> ('a -> 'a t) -> ('a -> 'a t)
  (** [timed t f] tries to modify a given problem via [f] for at most [t]
  seconds. If [f] does not succeed or fail within [t] seconds, [timed t f]
  fails. Otherwise [timed t f] returns the resulting problem. Hence it succeeds
  (fails) if [f] succeeds (fails). *)
 end

 (** {3 Module Main} *)
 
 (** Run Strategies
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** This module contains all important functions that are needed to run a
 given strategy. In order to register a new processor follow the instructions
 in the file [strategy/main.ml]. *)
 module Main : sig
  (*** VALUES *****************************************************************)
  (** {4 Execute Strategies} *)

  val run : Processors.Problem.t -> Processors.Rewritingx.Signature.t
   -> Syntax.t -> Proof.t * Processors.Rewritingx.Signature.t * Status.t
  (** [run p s t] runs the strategy [t] on input [p] with respect to the
  information [s].
  @raise Failure "unknown processor" if the strategy is not properly
  defined. *)

  (** {4 Printers} *)

  val help : ?fs:bool -> unit -> unit
  (** [help ~fs:fs ()] prints all processors of TTT2. If [fs] is set to [true],
  also the flags of the processors are printed. Per default, [fs] is set to
  [false]. Not tail-recursive. *)
  val locate : ?fs:bool -> Str.regexp -> unit
  (** [help ~fs:fs e] prints all processors of TTT2. If [fs] is set to [true],
  also the flags of the processors are printed. Per default, [fs] is set to
  [false]. Not tail-recursive. *)
 end
end

(** {2 Module Input} *)

(** WST Input Formats
@author Martin Korp
@since  Mon Sep  1 12:22:31 CEST 2008 *)

(** This module provides all functionalities necessary to abstract a
termination problem from an input defined according to the WST input format. *)
module Input : sig
 (*** MODULES *****************************************************************)
 (** {3 Module SrsSyntax} *)
 
 (** Abstract Syntax Tree for SRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the abstract syntax tree used to represent SRSs. *)
 module SrsSyntax : sig
  (*** TYPES ******************************************************************)
  type term = Var of string | Fun of string * term list
  type rule = Strict of term * term | Weak of term * term
  type strategy = Full | Leftmost | Rightmost
  type t
 
  (*** VALUES *****************************************************************)
  (** {4 Constructors and Destructors} *)

  val add_rules : rule list -> t -> t
  (** [add_rules rs r] adds the rewrite rules [rs] to the SRS [r]. *)
  val empty : t
  (** [empty] returns an empty SRS. *)
  val set_rules : rule list -> t -> t
  (** [set_rules rs r] sets the rewrite rules of [r] to [rs]. *)
  val set_strategy : strategy -> t -> t
  (** [set_strategy s r] sets the strategy of [r] to [s]. *)
  val to_problem : t -> Processors.Problem.t * Processors.Rewritingx.Signature.t
  (** [to_problem r] returns a termination problem that represents the SRS [r].
  @raise Failure "strategies are not supported" if the [r] contains some
  strategy information. *)

  (** {4 Access Functions} *)

  val get_rules : t -> rule list
  (** [get_rules r] returns the rules of the SRS [r]. *)
  val get_strategy : t -> strategy
  (** [get_strategy r] returns the strategy of [r]. *)

  (** {4 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt r] prints [r] using the [OCaml] module [Format]. This
  function is not tail-recursive. *)
  val to_string : t -> string
  (** [to_string r] returns a string that represents [r]. This function is
  not tail-recursive. *)
 end

 (** {3 Module SrsParser} *)
 
 (** SRS Parser
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the parser used to parse SRSs. *)
 module SrsParser : sig
  (*** TYPES ******************************************************************)
  type token =
   | ARROW
   | ARROW_EQUAL
   | COMMA
   | EOF
   | LEFTMOST
   | LEFT_PAREN
   | RIGHTMOST
   | RIGHT_PAREN
   | RULES
   | STRATEGY
   | ID of string
   | STRING of string

  (*** VALUES *****************************************************************)
  val srs : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> SrsSyntax.t
  (** [srs l r] parses the SRS [r] by using the lexer [l].
  @raise Parsing.Parse_error if the given SRS is not proper defined. *)
 end

 (** {3 Module SrsLexer} *)
 
 (** SRS Lexer
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the lexer used to tokenize SRSs. *)
 module SrsLexer : sig
  (*** VALUES *****************************************************************)
  val token : Lexing.lexbuf -> SrsParser.token
  (** [token r] returns the current token of the SRS [r]. *)
 end

 (** {3 Module TrsSyntax} *)
 
 (** Abstract Syntax Tree for TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the abstract syntax tree used to represent TRSs. *)
 module TrsSyntax : sig
  (*** TYPES ******************************************************************)
  type term = Var of string | Fun of string * term list
  type condition = Strong of term * term | Low of term * term
  type rule =
   | Strict of term * term * condition list
   | Weak of term * term * condition list
  
  type theory =
   | Theory of string * string list
   | Equations of (term * term) list
  
  type strategy =
   | Full
   | Innermost
   | Outermost
   | Contextsensitive of (string * int list) list
  
  type t
 
  (*** VALUES *****************************************************************)
  (** {4 Constructors and Destructors} *)

  val add_theories : theory list -> t -> t
  (** [add_theories ts r] adds the theories [ts] to the TRS [r]. *)
  val add_rules : rule list -> t -> t
  (** [add_rules rs r] adds the rules [rs] to [r]. *)
  val empty : t
  (** [empty] returns an empty TRS. *)
  val set_strategy : strategy -> t -> t
  (** [set_strategy s r] sets the strategy of [r] to [s]. *)
  val set_theories : theory list -> t -> t
  (** [set_theories ts r] sets the theories of [r] to [ts]. *)
  val set_rules : rule list -> t -> t
  (** [set_rules rs r] sets the rewrite rules of [r] to [rs]. *)
  val to_problem : t -> Processors.Problem.t * Processors.Rewritingx.Signature.t
  (** [to_problem r] returns a termination problem that represents [r].
  @raise Failure "theories and strategies are not supported" if the [r]
  contains some theory or strategy information.
  @raise Failure "conditions are not supported" if the TRSs in [r] come
  equiped with some conditions. *)

  val to_problem_with : t -> Processors.Rewritingx.Signature.t -> Processors.Problem.t * Processors.Rewritingx.Signature.t

  (** {4 Access Functions} *)

  val get_strategy : t -> strategy
  (** [get_strategy r] returns the strategy of [r]. *)
  val get_theories : t -> theory list
  (** [get_theories r] returns the theories of [r]. *)
  val get_rules : t -> rule list
  (** [get_rules r] returns the rules of [r]. *)

  (** {4 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt r] prints [r] using the [OCaml] module [Format]. This
  function is not tail-recursive. *)
  val to_string : t -> string
  (** [to_string r] returns a string that represents [r]. This function is
  not tail-recursive. *)
 end

 (** {3 Module TrsParser} *)
 
 (** TRS Parser
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the parser used to parse TRSs. *)
 module TrsParser : sig
  (*** TYPES ******************************************************************)
  type token =
   | ARROW
   | ARROW_EQUAL
   | ARROW_LR
   | COMMA
   | CONTEXTSENSITIVE
   | EOF
   | EQUAL
   | EQUATIONS
   | INNERMOST
   | LEFT_PAREN
   | OUTERMOST
   | PIPE
   | RIGHT_PAREN
   | RULES
   | STRATEGY
   | THEORY
   | VAR
   | ID of string
   | STRING of string

  (*** VALUES *****************************************************************)
  val trs : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> TrsSyntax.t
  (** [trs l r] parses the TRS [r] by using the lexer [l].
  @raise Parsing.Parse_error if the given TRS is not proper defined. *)
 end

 (** {3 Module TrsLexer} *)
 
 (** TRS Lexer
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the lexer used to tokenize TRSs. *)
 module TrsLexer : sig
  (*** VALUES *****************************************************************)
  val token : Lexing.lexbuf -> TrsParser.token
  (** [token r] returns the current token of the TRS [r]. *)
 end

 (** {3 Module ConfSyntax} *)
 
 (** Abstract Syntax Tree for Configuration Files
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the abstract syntax tree used to represent the information of
 configuration files. *)
 module ConfSyntax : sig
  (*** TYPES ******************************************************************)
  type abbreviation = string * string
  type t
 
  (*** VALUES *****************************************************************)
  (** {4 Constructors and Destructors} *)

  val add_abbreviation : abbreviation -> t -> t
  (** [add_abbreviation a t] adds the abbreviation [a] to [t]. *)
  val empty : t
  (** [empty] returns an empty list of abbreviations. *)
  val set_abbreviations : abbreviation list -> t -> t
  (** [set_abbreviations as t] sets the abbreviations of [as] to [t]. *)
  val singleton : abbreviation -> t
  (** [singleton a] returns the singleton abbreviation [a]. *)

  (** {4 Access Functions} *)

  val get_abbreviations : t -> abbreviation list
  (** [get_abbreviations t] returns the abbreviations of [t]. *)

  (** {4 Miscellaneous} *)

  val expand : string -> t -> string
  (** [expand s t] expands the abbrevations in [s] using [t]. Note that this
  function does not terminate if there is a recursive connection between
  abbreviations. *)

  (** {4 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt t] prints [t] using the [OCaml] module [Format]. *)
  val to_string : t -> string
  (** [to_string t] returns a string that represents [t]. *)
 end

 (** {3 Module ConfParser} *)
 
 (** Configuration File Parser
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the parser used to parse configuration files. *)
 module ConfParser : sig
  (*** TYPES ******************************************************************)
  type token =
   | BACKSLASH
   | EOF
   | EQUAL
   | NEWLINE
   | SHARP
   | ID of string

  (*** VALUES *****************************************************************)
  val conf : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ConfSyntax.t
  (** [conf l t] parses the configuration file [t] by using the lexer [l].
  @raise Parsing.Parse_error if the given configuration file is not proper
  defined. *)
 end

 (** {3 Module ConfLexer} *)
 
 (** Configuration File Lexer
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2008 *)
 
 (** Defines the lexer used to tokenize configuration files. *)
 module ConfLexer : sig
  (*** VALUES *****************************************************************)
  val token : Lexing.lexbuf -> ConfParser.token
  (** [token t] returns the current token of the configuration file [t]. *)
 end

 module Xml : sig
  val of_channel : in_channel -> Processors.Problem.t * Processors.Rewritingx.Signature.t
 end

end

(** {2 Module Answer} *)

(** Answer
@author Martin Korp
@since  Mon Sep  1 12:22:31 CEST 2008 *)

(** This module defines the possible outcomes of TTT2. *)
module Answer : sig
 (*** TYPES ******************************************************************)
 type t

 (*** VALUES *****************************************************************)
 (** {3 Constructors} *)
 
 val maybe : t
 (** [maybe] specifies the termination status of the given problem is
 unknown. This means the underlying problem could be termiantion but also
 nonterminating. *)
 val no : t
 (** [no] specifies that the given problem is nonterminating. *)
 val yes : t
 (** [yes] specifies that the given problem is terminating. *)
 val of_status : Strategy.Status.t -> t
 (** [of_status s] computes the answer of the underlying termination proof
 with respect to the status [s]. *)
 
 (** {3 Properties} *)

 val is_maybe : t -> bool
 (** [is_maybe a] checks if the given answer is MAYBE. *)
 val is_no : t -> bool
 (** [is_no a] checks if the given answer is NO. *)
 val is_yes : t -> bool
 (** [is_yes a] checks if the given asnwer is YES. *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt a] prints [a] using the [OCaml] module [Format]. *)
 val to_string : t -> string
 (** [to_string a] returns a string that represents [a]. *)
end

(** {2 Module Kernel} *)

(** Executing TTT2
@author Martin Korp
@since  Mon Sep  1 12:22:31 CEST 2008 *)

(** This module allows to execute TTT2 on a given termination problem with a
certain strategy. The input has to specified according to the WST format. The
strategy has to defined according to grammar mentioned in the description of
the module [Main] contained in the module [Strategy]. In order to integrate
new processors follow the instructions in the file [strategy/main.ml]. *)
module Kernel : sig
 (*** VALUES *****************************************************************)
 val execute : unit -> unit
 (** [execute ()] executes TTT2. In order to configurate TTT2 please use
 the command line options. A complete list of options (including explanations)
 can be obtained by executing `./ttt2 -h'. *)
 val parse_problem : ?stdin:bool -> string ->
  (Processors.Problem.t * Processors.Rewritingx.Parser.Xml.state)

 val run : Processors.Problem.t -> Processors.Rewritingx.Signature.t
  -> string -> Answer.t * Strategy.Proof.t
 (** [run p s strategy] executes TTT2 on the problem [p] with signature [s]
 using the strategy [strategy]. As result an answer and a proof object are
 returned. Note that the answer [YES] means termination, [NO] indicates
 nontermination and any other value indicates a nonsuccessful terminaton
 proof. *)
end
