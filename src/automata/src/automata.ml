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

(*** INCLUDES *****************************************************************)
include Automatax.Prelude;;

(*** MODULES ******************************************************************)
module Fun = Rewriting.Function;;
module Var = Rewriting.Variable;;

(*** OPEN *********************************************************************)
open Util;;
open Parsec;;

(*** MODULE TYPES *************************************************************)
module type STATE = sig
 type t

 val combinations : int -> t list -> (int * t list list) list
 val copy : t -> t
 val hash : t -> int
 val max : t -> t -> int
 val min : t -> t -> int
 val next : t -> t
 val to_int : t -> int
 val zero : t
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val fprintf : Format.formatter -> t -> unit
 val to_string : t -> string
end

module type STATUS = sig
 type t

 val init : t
 val add_state : State.t -> t -> t
 val create_state : int -> t -> State.t * t
 val fresh_state : t -> State.t * t
 val find_state : int -> t -> State.t
 val find_state_name : State.t -> t -> int
 val copy : t -> t
 val fprintf : Format.formatter -> t -> unit
 val to_string : t -> string
end

module type MONAD = sig
 type 'a m

 include Rewriting.MONAD

 val liftm : 'a m -> 'a t
 val add_state : State.t -> unit t
 val create_state : int -> State.t t
 val fresh_state : State.t t
 val find_state : int -> State.t t
 val find_state_name : State.t -> int t
end

module type TERM = sig
 type 'a m
 type 'a p
 type term
 type t = Var of Var.t | State of State.t | Fun of Fun.t * t list

(* TODO remove function *) val min : t list -> t list
(* TODO remove function *) val choose : t list -> t
 val make_fun : Fun.t -> t list -> t
 val make_state : State.t -> t
 val make_var : Var.t -> t
 val of_term : term -> t
 val to_term : t -> term
 val cons : t -> Fun.t list
 val funs : t -> Fun.t list
 val root : t -> Fun.t option
 val states : t -> State.t list
 val vars : t -> Var.t list
 val is_fun : t -> bool
 val is_ground : t -> bool
 val is_state : t -> bool
 val is_var : t -> bool
 val args : t -> t list
 val fun_size : t -> int
 val size : t -> int
 val state_size : t -> int
 val var_size : t -> int
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val of_string : string list -> string list -> string -> t m
 val parse : string list -> string list -> t p
 val fprintf : Format.formatter -> t -> unit
 val fprintfm : Format.formatter -> t -> unit m
 val to_string : t -> string
 val to_stringm : t -> string m
end

module type SUBSTITUTION = sig
 type term

 include Replacement.SIGNATURE with
  type domain = Var.t and type range = State.t

 val apply_term : t -> term -> term
end

module type LHS = sig
 type 'a m
 type 'a p
 type term
 type t = State of State.t | Fun of Fun.t * State.t list

 val make_fun : Fun.t -> State.t list -> t
 val make_state : State.t -> t
 val of_term : term -> t
 val to_term : t -> term
 val states : t -> State.t list
 val root : t -> Fun.t option
 val is_fun : t -> bool
 val is_state : t -> bool
 val args : t -> State.t list
 val hash : t -> int
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val of_string : string list -> string -> t m
 val parse : string list -> t p
 val fprintf : Format.formatter -> t -> unit
 val fprintfm : Format.formatter -> t -> unit m
 val to_string : t -> string
 val to_stringm : t -> string m
end

module type RHS = sig
 type t

(* TODO remove function *) val make : State.t option -> State.t list -> State.t list -> t
(* TODO remove function *) val set_agent : State.t -> t -> t
(* TODO remove function *) val agent : t -> State.t option
(* TODO remove function *) val violations : t -> State.t list
(* TODO remove function *) val det : t -> t
 val add : State.t -> t -> t
 val create : State.t list -> t
 val extend : State.t list -> t -> t
 val singleton : State.t -> t
 val states : t -> State.t list
 val fold : (State.t -> 'a -> 'a) -> t -> 'a -> 'a
 val iter : (State.t -> unit) -> t -> unit
 val map : (State.t -> State.t) -> t -> t
 val mem : State.t -> t -> bool
 val filter : (State.t -> bool) -> t -> t option
 val remove : State.t -> t -> t option
 val diff : t -> t -> t option
 val inter : t -> t -> t option
 val max : t -> State.t
 val size : t -> int
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val fprintf : Format.formatter -> t -> unit
 val to_string : t -> string
end

module type AUTOMATON = sig
 type 'a m
 type 'a p
 type lhs
 type rhs
 type substitution
 type term
 type trs
 type t
 
 val add : lhs -> rhs -> t -> t m
 val add_final : State.t -> t -> t
 val create : int -> t
 val extend : lhs -> State.t list -> t -> t m
 val remove : lhs -> t -> t m
 val remove_final : State.t -> t -> t
 val replace : lhs -> rhs -> t -> t m
 val set_finals : State.t list -> t -> t
 val update : lhs -> State.t -> t -> t m
 val category : Fun.t -> t -> Fun.t list m
 val finals : ?i:bool -> t -> State.t list
 val funs : ?p:(Fun.t -> bool) -> t -> Fun.t list
 val states : ?i:bool -> t -> State.t list
 val trans : ?p:(lhs -> rhs -> bool) -> t -> (lhs * rhs) list
 val fold : (lhs -> rhs -> 'a -> 'a) -> t -> 'a -> 'a
 val fold_eps : (lhs -> rhs -> 'a -> 'a) -> t -> 'a -> 'a
 val fold_trans : (lhs -> rhs -> 'a -> 'a) -> t -> 'a -> 'a
 val foldc_trans : Fun.t -> (lhs -> rhs -> 'a -> 'a) -> t -> 'a -> 'a m
 val foldf_trans : Fun.t -> (lhs -> rhs -> 'a -> 'a) -> t -> 'a -> 'a
 val iter : (lhs -> rhs -> unit) -> t -> unit
 val iter_eps : (lhs -> rhs -> unit) -> t -> unit
 val iter_trans : (lhs -> rhs -> unit) -> t -> unit
 val iterc_trans : Fun.t -> (lhs -> rhs -> unit) -> t -> unit m
 val iterf_trans : Fun.t -> (lhs -> rhs -> unit) -> t -> unit
 val map : (rhs -> rhs) -> t -> t
 val map_eps : (rhs -> rhs) -> t -> t
 val map_trans : (rhs -> rhs) -> t -> t
 val mapc_trans : Fun.t -> (rhs -> rhs) -> t -> t m
 val mapf_trans : Fun.t -> (rhs -> rhs) -> t -> t
 val ec_states : State.t list -> t -> State.t list
 val ec_rhs : rhs -> t -> rhs
 val ec : ?r:bool -> t -> t
 val compatible : ?c:bool -> ?h:t option -> ?t:(term -> term -> term m)
  -> trs -> t -> (term * State.t) list m
 val paths : ?c:bool -> ?h:t option -> ?f:bool -> term -> t
  -> ((term * substitution) list * State.t) list m
 val rewrite : ?c:bool -> term -> t -> term list m
 val context : ?c:bool -> ?n:int -> term -> State.t -> t
  -> (term * State.t) list m
 val quasi_det : ?c:bool -> t -> t m
 val det : ?c:bool -> ?q:bool -> t -> t m
 val is_accepted : ?c:bool -> term -> t -> bool m
 val is_empty : t -> bool m
 val is_reachable : ?c:bool -> ?h:t option -> term -> State.t -> t -> bool m
 val find : ?c:bool -> lhs -> t -> rhs option m
 val exists : (lhs -> rhs -> bool) -> t -> bool
 val for_all : (lhs -> rhs -> bool) -> t -> bool
 val mem : ?c:bool -> lhs -> t -> bool m
 val adapt : t -> t -> t m
 val analogies : t -> t -> (State.t * State.t) list m
 val combine : t -> t -> t m
 val copy : t -> t
 val inter : t -> t -> t m
 val minus : t -> t -> t m
 val restrict : Fun.t list -> t -> t m
 val reduce : t -> t m
 val size : t -> int
 val union : t -> t -> t m
 val of_string : string list -> string -> t m
 val parse : string list -> t p
 val fprintf : Format.formatter -> t -> unit
 val fprintfm : Format.formatter -> t -> unit m
 val to_string : t -> string
 val to_stringm : t -> string m
end

module type TRANSDUCER = sig
 type 'a m
 type automaton
 type t

 val fst : t -> automaton
 val invert : t -> t
 val make : automaton -> automaton -> t
 val snd : t -> automaton
 val transitive_closure : t -> t m
 val apply : (automaton -> 'a) -> (automaton -> 'b) -> t -> 'a * 'b
 val fold : (automaton -> 'a -> 'a) -> 'a -> t -> 'a
 val map : (automaton -> 'a) -> t -> 'a * 'a
 val project : (automaton -> automaton) -> t -> t
 val uncurry : (automaton -> automaton -> 'a) -> t -> 'a
 val copy : t -> t
 val size : t -> int
 val successors : automaton -> t -> automaton m
 val fprintf : Format.formatter -> t -> unit
 val fprintfm : Format.formatter -> t -> unit m
 val to_string : t -> string
 val to_stringm : t -> string m
end

module type PATH = sig
 type 'a m
 type automaton
 type lhs
 type term

 val fresh_min : ?l:bool -> term -> State.t -> automaton -> automaton m
 val fresh_funs : ?l:bool -> (Fun.t -> bool)
  -> term -> State.t -> automaton -> automaton m
 val fresh_reuse : ?f:(lhs -> automaton -> State.t m) -> ?l:bool
  -> term -> State.t -> automaton -> automaton m
 val fresh_max : ?l:bool -> term -> State.t -> automaton -> automaton m
 val fresh : (term -> State.t -> automaton -> automaton m)
  -> term -> State.t -> automaton -> automaton m
 val torpa : ?c:bool -> (term -> State.t -> automaton -> automaton m)
  -> term -> State.t -> automaton -> automaton m
 val suffix : ?p:bool -> ?c:bool
  -> (term -> State.t -> automaton -> automaton m)
  -> term -> State.t -> automaton -> automaton m
 val tttbox : ?p:bool -> ?n:int -> ?c:bool
  -> (term -> State.t -> automaton -> automaton m)
  -> term -> State.t -> automaton -> automaton m
 val ttt2 : ?p:bool -> ?n:int -> ?c:bool
  -> (term -> State.t -> automaton -> automaton m)
  -> term -> State.t -> automaton -> automaton m
end

module type INITIAL = sig
 type 'a m
 type automaton
 type term
 type trs

 val custom : Fun.t list -> Fun.t list -> Fun.t list -> automaton m
 val individual : Fun.t list -> Fun.t list -> automaton m
 val instances : (term -> State.t list m)
  -> (term -> State.t -> automaton -> automaton m)
  -> Fun.t list -> Fun.t list -> term list -> automaton m
 val min : Fun.t list -> automaton m
 val normal : Fun.t list -> trs -> automaton m
 val specific : (term -> State.t list m)
  -> (term -> State.t -> automaton -> automaton m) -> term list -> automaton m
 val split : Fun.t list -> automaton m
end

module type REACHABILITY = sig
 type 'a m
 type automaton
 type trs

 val predecessors : Fun.t list -> trs -> automaton -> automaton m
 val successors : Fun.t list -> trs -> automaton -> automaton m
end

module type SIGNATURE = sig
 type 'a m
 type signature
 type term
 type trs

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

(*** MODULES ******************************************************************)
module Make (R : REWRITING)
            (C : CATEGORIZATION with type 'a m = 'a R.Monad.t) = struct
 (*** TYPES *******************************************************************)
 type 'a m = 'a R.Monad.t;;
 type signature = R.Signature.t;;
 type term = R.Term.t;;
 type trs = R.Trs.t;;

 (*** MODULES *****************************************************************)
 module State = Automatax.State;;
 module Status = Automatax.Status;;
 module Monad = Automatax.Monad.Make (R);;
 module Parser = Automatax.Parser.Make (R);;
 module Term = Automatax.Term.Make (R);;
 module Substitution = Automatax.Substitution.Make (R);;
 module Lhs = Automatax.Lhs.Make (R);;
 module Rhs = Automatax.Rhs;;
 module Automaton = Automatax.Automaton.Make (R) (C);;
 module Transducer = Automatax.Transducer.Make (R) (C);;
 module Path = Automatax.Path.Make (R) (C);;
 module Initial = Automatax.Initial.Make (R) (C);;
 module Reachability = Automatax.Reachability.Make (R) (C);;
end

module Categorization (R : REWRITING) = struct
 (*** MODULES *****************************************************************)
 module M = R.Monad;;

 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type t = Fun.t list;;

 (*** FUNCTIONS ***************************************************************)
 let add f c = M.return (if List.mem f c then c else f::c);;
 let create = const [];;
 let remove f c = M.return (List.remove f c);;
 let category f c = M.return (if List.mem f c then [f] else []);;
 let find = category;;
 let funs = id;;
 let copy = id;;
 let compare c = compare (List.sort compare c) <.> List.sort compare;;

 let fprintf =
  List.fprintf (fun fmt -> Format.fprintf fmt "[%a]" Fun.fprintf) ","
 ;;
end

module Default (R : REWRITING) = Make (R) (Categorization (R));;

(*** INCLUDES *****************************************************************)
include Make (Rewriting) (Categorization (Rewriting));;
