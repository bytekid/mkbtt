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

(** Standard Library
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Tue Oct 28 11:54:35 CET 2008 *)

(** Provides some useful standard functions. *)

(*** TYPES ********************************************************************)
type ('a,'b) either = Left of 'a | Right of 'b

(*** VALUES *******************************************************************)
(** {2 Standard Functions} *)

val (|>) : 'a -> ('a -> 'b) -> 'b
(** [x |> f] applies the function [f] to the argument [x]. This facilitates
a 'waterfall notation' where the order of statements in the source code
reflects the order in which functions are applied. But still prevents the
typical naming errors in sequences of [let ... = ... in ...]s. *)
val (<.>) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
(** [f <.> g] is the function composition of [f] and [g], i.e., it is
equivalent to [fun x -> f (g x)]. *)
val (<?>) : ('a -> 'b) -> string -> ('a -> 'b)
(** [f <?> s] redefines the function [f] such that if [f] raises an exception
[Failure m] where [m] is an arbitrary error message, the exception [Failure s]
is thrown. I.e., it is equivalent to the function
[fun x -> try f x with Failure _ -> failwith s]. *)
val catch : exn -> ('a -> 'b) -> 'b -> 'a -> 'b
(** [catch e f d x] evaluates [f] on input [x] and returns its result. If [f]
raises the exception [e], [d] is returned. *)
val cons : 'a -> 'a list -> 'a list
(** [cons e l] is equivalent to {!val: Util.List.cons}. *)
val either : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) either -> 'c 
(** [either f g e] is equivalent to {!val: Util.Either.either}. *)
val even : int -> bool
(** [even n] checks if [n] is even. *)
val fix : ?c:('a -> 'a -> int) -> ('a -> 'a) -> 'a -> 'a
(** [fix ~c:c f x] computes a fixed point for [f] using the compare function [c].
Per default [c] is the standard compare function [compare]. *)
val id : 'a -> 'a
(** [id x] represents the identity function, i.e, given an input [x] it
returns [x]. *)
val odd : int -> bool
(** [odd n] checks if [n] is odd. *)
val option : ('a -> 'b) -> 'b -> 'a option -> 'b
(** [option f d o] is equivalent to {!val: Util.Option.option}. *)
val pair : 'a -> 'b -> 'a * 'b
(** [pair x y] is equivalent to {!val: Util.Pair.make}. *)
val throw : exn -> ('a -> 'b) -> ('b -> bool) -> 'a -> 'b
(** [throw e f p x] evaluates [f] on input [x]. If [p (f x)] yields [true],
the exception [e] is raised. Otherwise [f x] is returned. *)

(** {2 Input Transformations} *)

val const : 'a -> 'b -> 'a
(** [const x] is the constant function with value [x]. That means
given any input, it returns [x]. *)
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c 
(** [curry f x y] computes [f (x,y)]. *)
val curry3 : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
(** [curry3 f x y z] computes [f (x,y,z)]. *)
val drop : ('a -> 'b) -> 'a -> 'c -> 'b
(** [drop f x y] returns [f x]. The value [y] is ignored. *)
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c 
(** [flip f] returns a function that is equivalent to [f] with flipped
arguments, i.e., [flip f x y = f y x]. *)
val nop : ('a -> 'b) -> 'a -> 'b
(** [nop f x] returns [f x]. *)
val swap : 'a -> ('a -> 'b) -> 'b
(** [swap x f] returns [f x]. *)
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
(** [uncurry f p] is equivalent to {!val: Util.Pair.uncurry}. *)
val uncurry3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd
(** [uncurry3 f p] is equivalent to {!val: Util.Triple.uncurry}. *)

(** {2 Convenient Functions} *)

val read_channel : ?buff_size:int -> in_channel -> string
(** [read_channel ch] returns the contents of the channel [ch]
as a string. If [buff_size] is provided it is used as the
initial value of the internal read buffer. *)
val read_file : string -> string
(** [read_file f] returns the contents of file [f] as a string. *)

(*** MODULES ******************************************************************)
(** {2 Module Arg} *)

(** Extended Arg
@author Christian Sternagel 
@since  Sat Nov 29 15:28:49 CET 2008 *)

(** This is an extension of the standard [OCaml] module [Arg]. *)
module Arg : sig
 (*** MODULE TYPES ************************************************************)
 module type ARG = sig
  type anon_fun = string -> unit
  type doc = string
  type key = string
  type spec =
   | Unit of (unit -> unit)
   | Bool of (bool -> unit)
   | Set of bool ref
   | Clear of bool ref
   | String of (string -> unit)
   | Set_string of string ref
   | Int of (int -> unit)
   | Set_int of int ref
   | Float of (float -> unit)
   | Set_float of float ref
   | Tuple of spec list
   | Symbol of string list * (string -> unit)
   | Rest of (string -> unit)
  type usage_msg = string
  
  exception Bad of string
  exception Help of string
  
  val align: (key * spec * doc) list -> (key * spec * doc) list
  val current : int ref
  val parse : (key * spec * doc) list -> anon_fun -> usage_msg -> unit
  val parse_argv : ?current: int ref -> string array -> (key * spec * doc) list
   -> anon_fun -> usage_msg -> unit
  val usage : (key * spec * doc) list -> usage_msg -> unit
 end
 (** This module type contains all standard [OCaml] functions provided by the
 module [Arg]. *)
 
 (*** INCLUDES ****************************************************************)
 include ARG
 
 (*** VALUES ******************************************************************)
 val alignx : int -> (key * spec * doc) list -> (key * spec * doc) list
 (** [alignx w ks] behaves similar as [align] but with multiline support. The
 parameter [w] specifies the total width of the resulting text. *)
 val parsex : string -> (key * spec * doc) list -> string list -> unit
 (** [parsex s ks ss] is similar to [parse] but working on a list of strings
 and without any user messages. Note that this function does not support the
 option types [Tuple], [Symbol], and [Rest].
 @raise Bad [m] if [ss] contains a key which is not specified in [ks].
 @raise Failure "not supported" if an unsupported option type is used. *)
 val usagex: (key * spec * doc) list -> usage_msg -> unit
 (** [usagex spec error_mesg] behaves similar as [usage] but without
 adding entries for [-help] and [--help] if not already present. *)
end

(** {2 Module Bool} *)

(** Boolean Values
@author Martin Korp
@since  Thu Dec  4 16:53:44 CET 2008 *)

(** This module collects some frequently used operations on Boolean values. *)
module Bool : sig
 (*** TYPES *******************************************************************)
 type t = bool

 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val of_int : int -> t
 (** [of_int n] returns [true] if [n > 0]. *)
 val to_int : t -> int
 (** [to_int b] returns [1] if [b] is [true] and [0] otherwise. *)

 (** {3 Compare Functions} *)

 val compare: t -> t -> int
 (** [compare b b'] compares [b] and [b']. If [b] and [b'] are equal, [0] is
 returned. If [b] is smaller than [b'], [-1], and if [b'] is greater than [b],
 [1] is returned. *)
 val equal : t -> t -> bool
 (** [equal b b'] checks if [b] and [b'] are equal. Note that this function is
 equivalent to [compare b b' = 0]. *)

 (** {3 Miscellaneous} *)

 val copy : t -> t
 (** [copy b] returns a copy of [b]. *)
 val hash : t -> int
 (** [hash b] returns a hash value for [b]. *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt b] prints [b] using the [OCaml] module [Format]. *)
 val to_string : t -> string
 (** [to_string b] transforms [b] into a formatted string. *)
end

(** {2 Module Char} *)

(** Extended Char
@author Christian Sternagel 
@since  Thu Dec  4 02:12:12 CET 2008 *)

(** This is an extension of the standard [OCaml] char module. *)
module Char : sig
 (*** MODULE TYPES ************************************************************)
 type t = char

 module type CHAR = sig
  val chr : int -> char
  val code : char -> int
  val compare : t -> t -> int
  val escaped : char -> string
  val lowercase : char -> char
  val uppercase : char -> char
 end
 (** This module type contains all standard [OCaml] functions provided by the
 module [Char]. *)
 
 (*** INCLUDES ****************************************************************)
 include CHAR
 
 (*** VALUES ******************************************************************)
 (** {3 Properties} *)

 val is_alnum : char -> bool
 (** [is_alnum c] checks whether [c] is a digit or a letter. *)
 val is_alpha : char -> bool
 (** [is_alpha c] checks whether [c] is a letter. *)
 val is_digit : char -> bool
 (** [is_digit c] checks whether [c] is a digit between [0] and [9]. *)
 val is_lower : char -> bool
 (** [is_lower c] checks whether [c] is a lowercase letter. *)
 val is_space : char -> bool
 (** [is_space c] checks whether [c] is a white-space character ([' '], ['\n'],
 ['\t'] or ['\r']). *)
 val is_upper : char -> bool
 (** [is_upper c] checks whether [c] is an uppercase letter. *)

 (** {3 Compare Functions} *)

 val equal : t -> t -> bool
 (** [equal c d] checks if [c] and [d] are equal. Note that this function is
 equivalent to [compare c d = 0]. *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt c] prints [c] using the [OCaml] module [Format]. *)
 val to_string : t -> string
 (** [fprintf c] transforms [c] into a formatted string. *)
end

(** {2 Module Complexity} *)

(** Complexity
@author Martin Korp, Harald Zankl
@since  Tue Jul 21 12:22:31 CEST 2009 *)

(** This module defines the different (polynomial) complexity classes. *)
module Complexity : sig
 (*** TYPES *******************************************************************)
 type t

 (*** EXCEPTIONS **************************************************************)
 exception Undefined;;

 (*** VALUES ******************************************************************)
 (** {3 Constructors} *)

 val constant : t
 (** [constant] returns a value representing the complexity class O(1). *)
 val linear : t
 (** [linear] returns the complexity class O(n). *)
 val other : t
 (** [other] returns a value representing a non-polynomial complexity class. *)
 val poly : int option -> t
 (** [poly m] returns the polynomial complexity class O(n^m) if [m = Some m].
 Otherwise a value representing the complexity class P is returned. *)
 
 (** {3 Predicates} *)

 val is_constant : t -> bool
 (** [is_constant c] checks if [c] represents the constant complexity class. *)
 val is_linear : t -> bool
 (** [is_linear c] checks if [c] represents the linear complexity class. *)
 val is_other : t -> bool
 (** [is_other c] checks if [c] represents some non-polynomial complexity
 class. *)
 val is_poly : t -> bool
 (** [is_poly c] checks if [c] represents some polynomial complexity class. *)

 (** {3 Combinators} *)
 val max : t -> t -> t
 (** [max c c'] returns the maximum complexity class of [c] and [c']. *)
 val add : t -> t -> t
 (** [add c c'] sums up the complexity classes [c] and [c']. Note that this
 function is equivalent to {!val: Util.Complexity.max}. *)
 val mul : t -> t -> t
 (** [mul c c'] multiplies the complexity classes [c] and [c']. *)
 val sub : t -> t -> t
 (** [sub c c'] subtracts the complexity classes [c'] from [c].
 @raise Undefined if [c] is smaller than [c']. *)

 (** {3 Miscellaneous} *)
 val decrease : t -> t
 (** [decrease c] decreases the degree of the complexity classes [c] by 1.
 @raise Undefined if the degree of the complexity class [c] is unknown. *)
 val increase : t -> t
 (** [increase c] increase the degree of the complexity classes [c] by 1.
 @raise Undefined if the degree of the complexity class [c] is unknown. *)

 (** {3 Compare Functions} *)

 val (<=) : t -> t -> bool
 (** [c <= c'] checks if [c] is smaller or equal than [c']. Note that this
 function is equivalent to [compare c c' <= 0]. *)
 val (<) : t -> t -> bool
 (** [c < c'] checks if [c] is smaller than [c']. Note that this function is
 equivalent to [compare c c' < 0]. *)
 val (>=) : t -> t -> bool
 (** [c >= c'] checks if [c] is greater or equal than [c']. Note that this
 function is equivalent to [compare c c' >= 0]. *)
 val (>) : t -> t -> bool
 (** [c > c'] checks if [c] is greater than [c']. Note that this function is
 equivalent to [compare c c' > 0]. *)
 val compare: t -> t -> int
 (** [compare c c'] compares [c] and [c']. If [c] and [c'] are equal, [0] is
 returned. If [c] is smaller than [c'], [-1], and if [c'] is greater than [c],
 [1] is returned. *)
 val equal : t -> t -> bool
 (** [equal c c'] checks if [c] and [c'] are equal. Note that this function is
 equivalent to [compare c c' = 0]. *)

 (** {3 Printers} *)

 val fprintf : ?short:bool -> Format.formatter -> t -> unit
 (** [fprintf ~s:s fmt c] prints [c] using the [OCaml] module [Format].
 If the flag [s] is set to true and [c] corresponds to a polynomial of
 degree [n] then only [n] is printed. *)
 val to_string : ?short:bool -> t -> string
 (** [to_string ~s:s c] returns a formatted string that represents [c].
 If the flag [s] is set to true and [c] corresponds to a polynomial of
 degree [n] then only [n] is printed. *)

 (** {3 Parser} *)

 val of_answer : string -> (t option * t option)
 (** [of_answer a] returns [(l,u)] where [l] is the lower and [u] the
 upper bound. *)
end


(** {2 Module Either} *)

(** Alternatives
@author Christian Sternagel, Harald Zankl
@since  Tue Oct 28 11:54:35 CET 2008 *)

(** This module provides useful functions for the type [either]. It is used
to combine two different types in a way such that each instance represents
either a value of the first or the second type. *)
module Either : sig
 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val either : ('a -> 'c) -> ('b -> 'c) -> ('a,'b) either -> 'c
 (** [either f g e] performs pattern matching on [e]. Returns [f x] if
 [e = Left x] and [g x] if [e = Right x]. *)
 val left : ('a,'b) either -> 'a
 (** [left e] returns the left part of [e]. 
 @raise Failure "no content" if there is no left part. *)
 val make_left : 'a -> ('a,'b) either
 (** [make_left x] creates an instance of type [t] with left part [x]. *)
 val make_right : 'b -> ('a,'b) either
 (** [make_right x] creates an instance of type [t] with right part [x]. *)
 val right : ('a,'b) either -> 'b
 (** [right e] returns the right part of [e]. 
 @raise Failure "no content" if there is no right part. *)
 val strip : ('a,'a) either -> 'a
 (** [strip e] returns the argument of [e]. *)

 (** {3 Iterators} *)

 val map : ('a -> 'c) -> ('b -> 'd) -> ('a,'b) either -> ('c,'d) either
 (** [map f g e] performs pattern matching on [e]. Returns [Left (f x)] if
 [e = Left x] and [Right (g x)] if [e = Right x]. *)

 (* Properties *)
 val is_left : ('a,'b) either -> bool
 (** [is_left e] returns [true] if [e] is of form [Left x]. *)
 val is_right : ('a,'b) either -> bool
 (** [is_right e] returns [true] if [e] is of form [Right x]. *)
 
 (** {3 Printers} *)

 val fprintf : (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit) -> Format.formatter
  -> ('a,'b) either -> unit
 (** [fprintf f g fmt e] writes pretty printed output of [e]. *)
 val to_string : ('a -> string) -> ('b -> string) -> ('a,'b) either -> string
 (** [to_string f g e] transforms [e] into a formatted string. *)
end

(** {2 Module Filename} *)

(** Extended Filename
@author Martin Korp
@since  Thu Dec  4 12:30:02 CET 2008 *)
  
(** This is an extension of the standard [OCaml] module [Filename]. *)
module Filename : sig
 (*** MODULE TYPES ************************************************************)
 module type FILENAME = sig
  val current_dir_name : string
  val parent_dir_name : string
  val concat : string -> string -> string
  val is_relative : string -> bool
  val is_implicit : string -> bool
  val check_suffix : string -> string -> bool
  val chop_suffix : string -> string -> string
  val chop_extension : string -> string
  val basename : string -> string
  val dirname : string -> string
  val temp_file : string -> string -> string
  val open_temp_file : ?mode:open_flag list -> string -> string
   -> string * out_channel
  val temp_dir_name : string
  val quote : string -> string
 end
 (** This module type contains all standard [OCaml] functions provided by the
 module [Filename]. *)
 
 (*** INCLUDES ****************************************************************)
 include FILENAME
 
 (*** VALUES ******************************************************************)
 (** {3 Simplifications} *)

 val clean : string -> string
 (** [clean f] removes multiple occurrences of [/]. *)
 val expand : string -> string
 (** [expand f] replaces the symbol [~] by the path of the home directory. *)
 val extension : string -> string
 (** [extension f] returns the extension (string after last period) of [f]. 
 @raise Invalid_argument if [f] constains no period. *)
end

(** {2 Module Graph} *)

(** Labeled and Unlabeled Graphs
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Thu Dec  4 12:30:02 CET 2008 *)
  
(** This module implements (labeled) graphs. Multiple edges are not
supported. *)
module Graph : sig
 (*** MODULE TYPES ************************************************************)
 module type LABEL = sig
  type t
 
  val combine : t -> t -> t
  (** [combine l l'] combines the labels [l] and [l']. *)
  val compare : t -> t -> int
  (** [compare l l'] compares [l] and [l']. It must be guaranteed that
  [compare l l' = 0] if [l] and [l'] are equal, [compare l l' < 0] if [l]
  is smaller than [l'], and [compare l l' > 0] if [l] is greater than [l']. *)
  val copy : t -> t 
  (** [copy l] copies the value [l]. *)
  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt l] prints [l] using the [OCaml] module [Format]. *)
 end
 
 module type NODE = sig
  type t
 
  val compare : t -> t -> int
  (** [compare n m] compares [n] and [m]. It must be guaranteed that
  [compare n m = 0] if [n] and [m] are equal, [compare n m < 0] if [n]
  is smaller than [m], and [compare n m > 0] if [n] is greater than [m]. *)
  val copy : t -> t 
  (** [copy n] copies the value [n]. *)
  val hash : t -> int
  (** [hash n] returns the hash value of [n]. It must be guaranteed that
  [hash n = hash m] whenever [compare n m = 0]. *)
  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt n] prints [n] using the [OCaml] module [Format]. *)
 end
 
 module type SIGNATURE = sig
  (*** TYPES ******************************************************************)
  type node
  type edge
  type t
 
  (*** VALUES *****************************************************************)
  (** {3 Constructors and Destructors} *)

  val make : node list -> edge list -> t
  (** [make ns es] constructs a graph with nodes ns and edges es *)
  val add_edge : edge -> t -> t
  (** [add_edge e g] adds the edge [e] to the graph [g]. *)
  val add_node : node -> t -> t
  (** [add_node n g] adds the node [n] to the graph [g]. *)
  val edges : t -> edge list
  (** [edges g] returns the edges of the graph [g]. *)
  val empty : t
  (** [empty] returns an empty graph. *)
  val generate : (node -> node -> edge list) -> node list -> t
  (** [generate f ns] generates a graph consisting of the edges induced by
  function [f]. Note that the function [f] receives all possible combinations
  of nodes in [ns]. *)
  val in_nodes : t -> node list
  (** [in_nodes g] returns all nodes of [g] that have at least one incoming
  edge. *)
  val nodes : t -> node list
  (** [nodes g] returns all nodes of the graph [g]. *)
  val of_list : edge list -> t
  (** [of_list es] constructs a new graph which consists of the edges [es].
  Note that multiples of one and the same edge are added only once. *)
  val out_nodes : t -> node list
  (** [out_nodes g] returns all nodes of [g] that have at least one outgoing
  edge. *)
  val remove_edge : edge -> t -> t
  (** [remove_edge e g] removes the edge [e] from [g]. *)
  val remove_node : node -> t -> t
  (** [remove_node n g] removes the node [n] from [g]. *)
  val restrict : node list -> t -> t
  (** [restrict ns g] returns a new graph consisting of all edges that
  remain after removing all nodes that are not contained in [ns]. *)
  val successors : node -> t -> node list
  (** [successors n g] returns the successors of the node [n]. *)
  val to_list : t -> edge list
  (** [to_list g] is equivalent to {!val: Util.Graph.SIGNATURE.edges}. *)

  (** {3 Iterators} *)

  val fold_edges : (edge -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_edges f g d] combines all edges of [g] using the function [f]. *)
  val foldi_edges : (int -> edge -> 'a -> 'a) -> t -> 'a -> 'a
  (** [foldi_edges f g d] combines all edges of [g] using the function [f].
  As first argument, [f] receives the index of the current edge (starting
  at [0]). *)
  val fold_nodes : (node -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_nodes f g d] combines all nodes of [g] using the function [f]. *)
  val foldi_nodes : (int -> node -> 'a -> 'a) -> t -> 'a -> 'a
  (** [foldi_nodes f g d] combines all nodes of [g] using the function [f].
  As first argument, [f] receives the index of the current node (starting
  at [0]). *)
  val iter_edges : (edge -> unit) -> t -> unit
  (** [iter_edges f g] applies [f] to all edges of [g]. *)
  val iteri_edges : (int -> edge -> unit) -> t -> unit
  (** [iteri_edges f g] applies [f] to all edges of [g]. Note that [f] receives
  as first argument the index of the current edge (starting at [0]). *)
  val iter_nodes : (node -> unit) -> t -> unit
  (** [iter_nodes f g] applies [f] to all nodes of [g]. *)
  val iteri_nodes : (int -> node -> unit) -> t -> unit
  (** [iteri_nodes f g] applies [f] to all nodes of [g]. Note that [f] receives
  as first argument the index of the current node (starting at [0]). *)

  (** {3 Graph Analysis} *)

  val cycles : t -> node list list
  (** [cycles g] computes all simple cycles of the graph [g]. Note that a
  simple cycle is a cycle where all intermediate nodes are pairwise distinct.
  This function is not tail-recursive. *)
  val paths : node -> node -> t -> node list list
  (** [paths n m g] computes all non-empty paths in [g] starting at node [n]
  and ending at node [m]. Note that all intermediate nodes (without the start
  node [n] and the final node [m]) are pairwise distinct. This function is
  not tail-recursive. *)
  val sccs : ?trivial:bool -> t -> node list list
  (** [sccs ~trivial:b g] returns all sccs of [g]. If the optional argument
  [trivial] is set to [true] then all SCCs, including trivial ones, are
  returned. Otherwise only non-trivial SCCs are returned. Per default,
  [trivial] is set to [true]. *)

  (** {3 Search Functions} *)

  val filter_edges : (edge -> bool) -> t -> t
  (** [filter_edges p g] returns a new graph consisting of all edges that
  satisfies the predicate [p]. *)
  val filter_nodes : (node -> bool) -> t -> t
  (** [filter_nodes p g] returns a new graph consisting of all edges that
  remain after removing all nodes that do not satisfy the predicate [p]. *)
  val find_edge : (edge -> bool) -> t -> edge
  (** [find_edge p g] returns the first edge that satisfies the predicate [p].
  @raise Not_found if no such edge exists. *)
  val find_node : (node -> bool) -> t -> node
  (** [find_node p g] returns the first node that satisfies the predicate [p].
  @raise Not_found if no such node exists. *)

  (** {3 Scan Functions} *)

  val mem_edge : edge -> t -> bool
  (** [mem_edge e g] checks if [e] is contained in [g]. *)
  val mem_node : node -> t -> bool
  (** [mem_node n g] checks if [n] is contained in [g]. *)
  val for_all_edges : (edge -> bool) -> t -> bool
  (** [for_all_edges p g] checks if [p] holds for all edges in [g]. *)
  val for_all_nodes : (node -> bool) -> t -> bool
  (** [for_all_nodes p g] checks if [p] holds for all nodes in [g]. *)
  val exists_edge : (edge -> bool) -> t -> bool
  (** [exists_edge p g] checks if there is an edge that satisfies [p]. *)
  val exists_node : (node -> bool) -> t -> bool
  (** [exists_node p g] checks if there is a node that satisfies [p]. *)

  (** {3 Properties} *)

  val is_empty : t -> bool
  (** [is_empty g] returns [true] if [g] does not contain any edges. *)
  val is_trivial_scc : node list -> t -> bool
  (** [is_trivial_scc scc g] checks if the given scc is trivial. Not that a
  SCC is not trivial if it admits a non-empty cycle. *)

  (** {3 Miscellaneous} *)

  val bypass : node -> t -> t
  (** [bypass n g] returns a graph without node [n]. Node [n] is bypassed.
  @raise Failure "looping edge" if [g] contains an edge from [n] to itself. *)
  val partition : (edge -> bool) -> t -> t * t
  (** [partition p g] returns two graphs where the first one is build from
  edges that satisfy [p] and the second one from edges that do not satisfy
  [p]. Note that both graphs consist of all nodes from [g]. *)
  val size_edges : t -> int
  (** [size_edges g] returns the number of edges of [g]. *)
  val size_nodes : t -> int
  (** [size_nodes g] returns the number of nodes of [g]. *)

  (** {3 Compare Functions} *)

  val compare: t -> t -> int
  (** [compare g g'] compares [g] and [g']. If [g] and [g'] are equal, [0] is
  returned. If [g] is smaller than [g'], [-1], and if [g] is greater than [g'],
  [1] is returned. *)
  val equal : t -> t -> bool
  (** [equal g g'] checks if [g] and [g'] are equal. Note that this function is
  equivalent to [compare g g' = 0]. *)

  (** {3 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt g] prints [g] using the [OCaml] module [Format]. *)
  val to_string : t -> string
  (** [to_string g] transforms [g] into a formatted string. *)
 end

 module type UNLABELED = sig
  (*** INCLUDES ***************************************************************)
  include SIGNATURE

  (*** VALUES *****************************************************************)
  (** {3 Constructors and Destructors} *)

  val add : node -> node list -> t -> t
  (** [add n ns g] adds new edges from node [n] to [ns] to [g]. If there exists
  already a binding for [n] in [g], the graph remains unchanged. *)
  val replace : node -> node list -> t -> t
  (** [replace n ns g] adds new edges from node [n] to [ns] to [g]. If there
  exists already a binding for [n] in [g] it is replaced. *)

  (** {3  Iterators} *)

  val fold : (node -> node list -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f g d] combines all edges of [g] using the function [f]. *)
  val foldi : (int -> node -> node list -> 'a -> 'a) -> t -> 'a -> 'a
  (** [foldi f g d] combines all edges of [g] using the function [f]. As first
  argument, [f] receives the index of the current node (starting at [0]). *)
  val iter : (node -> node list -> unit) -> t -> unit
  (** [iter f g] applies [f] to all edges of [g]. *)
  val iteri : (int -> node -> node list -> unit) -> t -> unit
  (** [iteri f g] applies [f] to all edges of [g]. Note that [f] receives as
  first argument the index of the current node (starting at [0]). *)

  (** {3 Miscellaneous} *)

  val is_acyclic : t -> bool
  (** [is_acyclic g] checks whether the graph [g] has cycles. *)
  val floyd_warshall : t -> ((node * int)list * int option array array)
  (** [floyd_warshall g] computes the adjacency matrix of shortest paths
  in [g]. Additionally an association list which maps nodes to indices in
  the matrix is returned. *)
  val transitive_closure : t -> t
  (** [transitive_closure g] computes the transitive closure of [g]. *)
 end
 
 module type LABELED = sig
  (*** TYPES ******************************************************************)
  type label
 
  (*** INCLUDES ***************************************************************)
  include SIGNATURE

  (*** VALUES *****************************************************************)
  (** {3 Constructors and Destructors} *)

  val add : node -> (label option * node) list -> t -> t
  (** [add n ns g] adds new edges from node [n] to [ns] to [g]. If there exists
  already a binding for [n] in [g], the graph remains unchanged. *)
  val replace : node -> (label option * node) list -> t -> t
  (** [replace n ns g] adds new edges from node [n] to [ns] to [g]. If there
  exists already a binding for [n] in [g] it is replaced. *)

  (** {3  Iterators} *)

  val fold : (node -> (label option * node) list -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f g d] combines all edges of [g] using the function [f]. *)
  val foldi : (int -> node -> (label option * node) list -> 'a -> 'a) -> t
   -> 'a -> 'a
  (** [foldi f g d] combines all edges of [g] using the function [f]. As first
  argument, [f] receives the index of the current node (starting at [0]). *)
  val iter : (node -> (label option * node) list -> unit) -> t -> unit
  (** [iter f g] applies [f] to all edges of [g]. *)
  val iteri : (int -> node -> (label option * node) list -> unit) -> t -> unit
  (** [iteri f g] applies [f] to all edges of [g]. Note that [f] receives as
  first argument the index of the current node (starting at [0]). *)

  (** {3 Search Functions} *)

  val labels : node -> node -> t -> label list
  (** [labels n m] returns the labels of all edges going from node [n] to node
  [m]. *)

  (** {3 Miscellaneous} *)

  val combine_edges : t -> t
  (** [combine_edges g] returns a graph with at most one edge between two
  nodes. To combine two edges the function {!val: Util.Graph.LABEL.combine} is
  used. *)
  val transitive_closure : ?n:int -> t -> t
  (** [transitive_closure ~n:n g] computes the transitive closure of [g] for
  at most [n] steps. Note that if [n] is smaller than [0] the computation can
  loop if [g] contains self-loops! Per default [n = -1]. *)
 end

 (*** MODULES *****************************************************************)
 module Labeled (L : LABEL) (N : NODE) : LABELED with
  type node = N.t and type edge = N.t * L.t option * N.t and type label = L.t
 (** Functor building a directed labeled graph. The labels are of
 module type {!modtype: Util.Graph.LABEL} whereas the nodes are of
 module type {!modtype: Util.Graph.NODE}. *)
 module Make (N : NODE) : UNLABELED with
  type node = N.t and type edge = N.t * N.t
 (** Functor building a directed unlabeled graph. The nodes are of module
 type {!modtype: Util.Graph.NODE}. *)
end

(** {2 Module Hashtbl} *)

(** Extended Hashtable
@author Martin Korp
@since  Sat Nov 29 11:16:35 CEST 2008 *)

(** This is an extension of the standard [OCaml] module [Hashtbl]. *)
module Hashtbl : sig
 (*** MODULE TYPES ************************************************************)
 module type DOMAIN = sig
  type t
 
  val compare : t -> t -> int
  (** [compare k t] compares [k] and [k']. It must be guaranteed that
  [compare k k' = 0] if [k] and [k'] are equal, [compare k k' < 0] if [k]
  is smaller than [k'], and [compare k k' > 0] if [k] is greater than [k']. *)
  val hash : t -> int
  (** [hash k] returns a hash value for key [k]. It must be guaranteed that
  if two keys are equal according to function [compare], then they have
  identical hash values. *)
  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt k] prints [k] using the [OCaml] module [Format]. *)
 end
 
 module type RANGE = sig
  type t
 
  val compare : t -> t -> int
  (** [compare e e'] compares [e] and [e']. It must be guaranteed that
  [compare e e' = 0] if [e] and [e'] are equal, [compare e e' < 0] if [e]
  is smaller than [e'], and [compare e e' > 0] if [e] is greater than [e']. *)
  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt e] prints [e] using the [OCaml] module [Format]. *)
 end

 module type HASHTBL = sig
  type ('a,'b) t
 
  val add : ('a,'b) t -> 'a -> 'b -> unit
  val clear : ('a,'b) t -> unit
  val copy : ('a,'b) t -> ('a,'b) t
  val create : ?random:bool -> int -> ('a,'b) t
  val remove : ('a,'b) t -> 'a -> unit
  val replace : ('a,'b) t -> 'a -> 'b -> unit
  val find : ('a,'b) t -> 'a -> 'b
  val find_all : ('a,'b) t -> 'a -> 'b list
  val fold : ('a -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c
  val iter : ('a -> 'b -> unit) -> ('a,'b) t -> unit
  val mem : ('a,'b) t -> 'a -> bool
  val length : ('a,'b) t -> int
  val hash : 'a -> int
  val hash_param : int -> int -> 'a -> int
 end
 (** This module contains all functions of the standard [OCaml] module
 [Hashtbl]. *)
 
 module type PARTIAL = sig
  type domain
  type 'a t

  val add : 'a t -> domain -> 'a -> unit
  val clear : 'a t -> unit
  val copy : 'a t -> 'a t
  val create : int -> 'a t
  val remove : 'a t -> domain -> unit
  val replace : 'a t -> domain -> 'a -> unit
  val find : 'a t -> domain -> 'a
  val find_all : 'a t -> domain -> 'a list
  val fold : (domain -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (domain -> 'a -> unit) -> 'a t -> unit
  val mem : 'a t -> domain -> bool
  val length : 'a t -> int
 end
 (** This module is equivalent to the standard [OCaml] module type [Hashtbl.S],
 except that type [key] is now called [domain]. *)

 module type SIGNATURE = sig
  (*** TYPES ******************************************************************)
  type domain
  type range
  type t
  
  (*** VALUES *****************************************************************)
  (** {3 Constructors} *)

  val add : domain -> range -> t -> t
  (** [add k e h] adds a new binding of [k] to [e] to [h]. If there exists
  already a binding for [k] in [h], the hash table remains unchanged. Note
  that as a side effect [h] is changed too. *)
  val clear : t -> t
  (** [clear h] is equivalent to [Hashtbl.S.clear]. *)
  val copy : t -> t
  (** [copy h] is equivalent to [Hashtbl.S.copy]. *)
  val create : int -> t
  (** [create n] is equivalent to [Hashtbl.S.create]. *)
  val empty : t
  (** [empty] is equivalent to [create 0]. *)
  val of_list : (domain * range) list -> t
  (** [of_list xs] creates a hash table consisting of all bindings in [xs].
  Note that multiples of one and the same binding in [xs] are added only
  once. *)
  val remove : domain -> t -> t
  (** [remove k h] is equivalent to [Hashtbl.S.remove]. Note that as a side
  effect [h] is changed too. *)
  val replace : domain -> range -> t -> t
  (** [replace k e h] is equivalent to [Hashtbl.S.replace]. Note that as a
  side effect [h] is changed too. *)
  val singleton : domain -> range -> t
  (** [singleton k e] returns a hash table containing a singleton binding of
  [k] to [e]. *)
  val to_list : t -> (domain * range) list
  (** [to_list h] creates a list consisting of all bindings in [h]. *)

  (** {3 Search Functions} *)

  val find : domain -> t -> range
  (** [find k h] is equivalent to [Hashtbl.S.find].
  @raise Not_found if [h] does not contain a binding for [k]. *)
  val find_all : domain -> t -> range list
  (** [find_all k h] is equivalent to [Hashtbl.S.find_all]. *)
  val search : (domain -> range -> bool) -> t -> domain * range
  (** [search p h] returns the first binding that satisfies the predicate [p].
  @raise Not_found if [h] does not contain a binding that satisfies [p]. *)

  (** {3 Iterators} *)

  val fold : (domain -> range -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f h d] is equivalent to [Hashtbl.S.fold]. *)
  val foldi : (int -> domain -> range -> 'a -> 'a) -> t -> 'a -> 'a
  (** [foldi f h d] combines all elements of [h] using the function [f]. Note
  that as first argument the function [f] gets the index of the considered
  element, where the first element has index [0]. *)
  val iter : (domain -> range -> unit) -> t -> unit
  (** [iter f h] is equivalent to [Hashtbl.S.iter]. *)
  val iteri : (int -> domain -> range -> unit) -> t -> unit
  (** [iteri f h] applies the function [f] to all elements of [h]. Note that
  as first argument the function [f] gets the index of the considered element,
  where the first element has index [0]. *)
  val map : (range -> range) -> t -> t
  (** [map f h] applies the function [f] to the values of the bindings in [h].
  Note that as a side effect [h] is changed too. *)
  val mapi : (domain -> range -> range) -> t -> t
  (** [mapi f h] applies the function [f] to the bindings in [h]. Note that as
  a side effect [h] is changed too. *)

  (** {3 Scan Functions} *)

  val exists : (domain -> range -> bool) -> t -> bool
  (** [exists p h] checks whether the predicate [p] holds for at least one
  binding in [h]. *)
  val for_all : (domain -> range -> bool) -> t -> bool
  (** [for_all p h] checks whether the predicate [p] holds for all bindings in
  [h]. *)
  val mem : domain -> t -> bool
  (** [mem k h] is equivalent to [Hashtbl.S.mem]. *)

  (** {3 Properties} *)

  val is_empty : t -> bool
  (** [is_empty h] checks if [h] is empty, i.e., if it does not contain a
  binding. *)

  (** {3 Miscellaneous} *)

  val domain : t -> domain list
  (** [domain h] returns the domain of [h]. *)
  val length : t -> int
  (** [length h] is equivalent to [Hashtbl.S.length]. *)
  val range : t -> range list
  (** [range h] returns the range of [h]. *)
  val size : t -> int
  (** [size h] is equivalent to {!val: Util.Hashtbl.SIGNATURE.length}. *)

  (** {3 Compare Functions} *)

  val compare : t -> t -> int
  (** [compare h h'] compares [h] and [h']. This function defines a total
  ordering on hash tables. *)
  val equal : t -> t -> bool
  (** [equal h h'] tests whether [h] contains the same bindings as [h'] and
  vice versa. This function is equivalent to [compare h h' = 0]. *)

  (** {3 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt h] prints the replacement map [h] nicely. *)
  val to_string : t -> string
  (** [to_string h] transforms [h] into a formatted string. *)
 end

 (*** INCLUDES ****************************************************************)
 include HASHTBL

 (*** MODULES *****************************************************************)
 module Make (D : DOMAIN) (R : RANGE) : SIGNATURE with
  type domain = D.t and type range = R.t
 (** Functor building a hash table. The elements of the range are of module
 type {!modtype: Util.Hashtbl.RANGE} whereas the elements of the domain are of
 module type {!modtype: Util.Hashtbl.DOMAIN}. *)
 module Partial (D : DOMAIN) : PARTIAL with type domain = D.t
 (** Functor building a partial hash table. The elements of the domain are
 of module type {!modtype: Util.Hashtbl.DOMAIN}. The range can be chosen
 arbitrarily. Note that this functor is equivalent to the standard [OCaml]
 functor [Hashtbl.Make]. *)
end

(** {2 Module Index} *)

(** Index
@author Martin Korp
@since  Wed Sep  3 09:11:32 CEST 2008 *)

(** This module provides some index implementations. An index is a mapping
which maps indices of type [KEY] to elements of type [ELEMENT]. A bijective
index is an isomorphism, i.e., it index elements of type [ELEMENT] with
indices of type [KEY] and vice versa. *)
module Index : sig
 (*** MODULE TYPES ************************************************************)
 module type KEY = sig
  type t

  val compare : t -> t -> int
  (** [compare s t] compares [s] and [t]. It must be guaranteed that
  [compare s t = 0] if [s] and [t] are equal, [compare s t < 0] if [s]
  is smaller than [t], and [compare s t > 0] if [s] is greater than [t]. *)
  val copy : t -> t
  (** [copy t] copies the value [t]. *)
  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt t] prints [t] using the [OCaml] module [Format]. *)
  val hash : t -> int
  (** [hash t] returns the hash value of [t]. It must be guaranteed that
  [hash s = hash t] whenever [compare s t = 0]. *)
  val next : t -> t
  (** [next t] returns the successor of [t]. It must be guaranteed that
  [next s = next t] if and only if [compare s t = 0]. *)
  val zero : t
  (** [zero] is the smallest value of type [t]. It must be guaranteed that
  there is no [s] such that [compare s t < 0]. *)
 end
 
 module type ELEMENT = sig
  type t

  val compare : t -> t -> int
  (** [compare s t] compares [s] and [t]. It must be guaranteed that
  [compare s t = 0] if [s] and [t] are equal, [compare s t < 0] if [s]
  is smaller than [t], and [compare s t > 0] if [s] is greater than [t]. *)
  val copy : t -> t
  (** [copy t] copies the value [t]. *)
  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt t] prints [t] using the [OCaml] module [Format]. *)
  val hash : t -> int
  (** [hash t] returns the hash value of [t]. It must be guaranteed that
  [hash s = hash t] whenever [compare s t = 0]. *)
 end

 module type SIGNATURE = sig
  (*** TYPES ******************************************************************)
  type key
  type element
  type t
  
  (*** VALUES *****************************************************************)
  (** {3 Constructors} *)

  val add : key -> element -> t -> t
  (** [add k e i] adds a binding from [k] to the element [e]. If there exists
  already a binding for [k], the index remains unchanged. *)
  val empty : t
  (** [empty] creates an empty index. *)
  val index : element -> t -> t
  (** [index e i] adds a binding from a fresh key [k] to the element [e]. *)
  val of_list : (key * element) list -> t
  (** [of_list xs] creates an index consisting of all bindings in [xs]. If
  [xs] contains bindings with the same key, the first is added to the index.
  All others are ignored. *)
  val replace : key -> element -> t -> t
  (** [replace k e i] adds a binding from [k] to the element [e]. If there
  exists already a binding for [k], it is replaced. *)
  val to_list : t -> (key * element) list
  (** [to_list i] creates a list consisting of all bindings in [i]. *)

  (** {3 Iterators} *)

  val fold : (key -> element -> 'a -> 'a) -> 'a -> t -> 'a
  (** [fold f d i] computes [f k_n e_n (... (f k_1 e_1 d) ...)], where
  [k_1, ..., k_n] are the keys of all bindings in [i], and [e_1, ..., e_n]
  are the associated values. Each binding is presented exactly once to [f].
  The order in which the bindings are passed to [f] is unspecified. *)
  val iter : (key -> element -> unit) -> t -> unit
  (** [iter f i] applies the function [f] to all bindings in [i]. The
  function [f] receives as first argument the key and as second argument
  the associated value. *)

  (** {3 Scan Functions} *)

  val mem : key -> t -> bool
  (** [mem k i] checks if [i] contains a binding for the key [k]. *)

  (** {3 Search Functions} *)

  val find : key -> t -> element
  (** [find k i] returns the element [e] associated with [k].
  @raise Not_found if there is no binding for [k]. *)

  (** {3 Miscellaneous} *)

  val clear : t -> t
  (** [clear i] removes all bindings from the index [i]. *)
  val copy : t -> t
  (** [copy i] copies the index [i]. *)
  val elements : t -> element list
  (** [elements i] returns the elements of the range of [i]. *)
  val fresh : t -> key * t
  (** [fresh i] returns a fresh key. *)
  val keys : t -> key list
  (** [keys i] returns the keys used by [i]. *)
  val size : t -> int
  (** [size i] returns the number of bindings stored in the index [i]. *)

  (** {3 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt i] prints [i] using the [OCaml] module [Format]. *)
  val to_string : t -> string
  (** [to_string i] transforms [i] into a formatted string. *)
 end

 module type ISOMORPHIC = sig
  (*** TYPES ******************************************************************)
  type key
  type element
  type t
  
  (*** VALUES *****************************************************************)
  (** {3 Constructors} *)

  val add : key -> element -> t -> t
  (** [add k e i] adds a binding from [k] to the element [e] and vice
  versa. If there exists already a binding for [k] or [e], the index
  remains unchanged. Note that as a side effect, the original index is
  changed too. *)
  val empty : int -> t
  (** [empty n] creates an index which will store approximately [n] bindings. *)
  val index : element -> t -> t
  (** [index e i] adds a binding from a fresh key [k] to the element [e] and
  vice versa. If there exists already a binding for [e], the index remains
  unchanged. Note that as a side effect, the original index is changed too. *)
  val of_list : (key * element) list -> t
  (** [of_list xs] creates an index consisting of all bindings in [xs]. If
  [xs] contains bindings with the same key or element, the first is added to
  the index. All others are ignored. *)
  val replace : key -> element -> t -> t
  (** [replace k e i] adds a binding from [k] to the element [e] and vice
  versa. If there exists already a binding for [k] or [e], it is replaced.
  Note that as a side effect, the original index is changed too. *)
  val to_list : t -> (key * element) list
  (** [to_list i] creates a list consisting of all bindings in [i]. *)

  (** {3 Iterators} *)

  val fold : (key -> element -> 'a -> 'a) -> 'a -> t -> 'a
  (** [fold f d i] computes [f k_n e_n (... (f k_1 e_1 d) ...)], where
  [k_1, ..., k_n] are the keys of all bindings in [i], and [e_1, ..., e_n]
  are the associated values. Each binding is presented exactly once to [f].
  The order in which the bindings are passed to [f] is unspecified. *)
  val iter : (key -> element -> unit) -> t -> unit
  (** [iter f i] applies the function [f] to all bindings in [i]. The
  function [f] receives as first argument the key and as second argument
  the associated value. *)

  (** {3 Scan Functions} *)

  val mem_elt : element -> t -> bool
  (** [mem_elt e i] checks if [i] contains a binding for the element [e]. *)
  val mem_key : key -> t -> bool
  (** [mem_key k i] checks if [i] contains a binding for the key [k]. *)

  (** {3 Search Functions} *)

  val find_elt : key -> t -> element
  (** [find_elt k i] returns the element [e] associated with [k].
  @raise Not_found if there is no binding for [k]. *)
  val find_key : element -> t -> key
  (** [find_key e i] returns the index [k] associated with [e].
  @raise Not_found if there is no binding for [e]. *)

  (** {3 Miscellaneous} *)

  val clear : t -> t
  (** [clear i] removes all bindings from the index [i]. Note that as a side
  effect, the original index is changed too. *)
  val copy : t -> t
  (** [copy i] copies the index [i]. *)
  val elements : t -> element list
  (** [elements i] returns the elements of the range of [i]. *)
  val fresh : t -> key * t
  (** [fresh i] returns a fresh key. Note that as a side effect, the original
  index is changed too. *)
  val keys : t -> key list
  (** [keys i] returns the keys used by [i]. *)
  val size : t -> int
  (** [size i] returns the number of bindings stored in the index [i]. *)

  (** {3 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt i] prints [i] using the [OCaml] module [Format]. *)
  val to_string : t -> string
  (** [to_string i] transforms [i] into a formatted string. *)
 end
 
 (*** MODULES *****************************************************************)
 module Default (E : ELEMENT) : SIGNATURE with
  type key = int and type element = E.t
 (** Functor building an index. The keys are of type [int] whereas the
 associated elements are of module type {!modtype: Util.Index.ELEMENT}. *)
 module Isomorphic (K : KEY) (E : ELEMENT) : ISOMORPHIC with
  type key = K.t and type element = E.t
 (** Functor building a bijective index. The keys are of module type
 {!modtype: Util.Index.KEY} whereas the associated elements are of
 module type {!modtype: Util.Index.ELEMENT}. *)
 module Make (K : KEY) (E : ELEMENT) : SIGNATURE with
  type key = K.t and type element = E.t
 (** Functor building an index. The keys are of module type
 {!modtype: Util.Index.KEY} whereas the associated elements
 are of module type {!modtype: Util.Index.ELEMENT}. *)
 module Standard (E : ELEMENT) : ISOMORPHIC with
  type key = int and type element = E.t
 (** Functor building a bijective index. The keys are of type [int] whereas
 the associated elements are of module type {!modtype: Util.Index.ELEMENT}. *)
end

(** {2 Module Int} *)

(** Integers
@author Harald Zankl
@since  Thu Dec  4 16:53:44 CET 2008 *)

(** This module collects some frequently used operations on integers. *)
module Int : sig
 (*** TYPES *******************************************************************)
 type t = int

 (*** VALUES ******************************************************************)
 (** {3 Constructors} *)

 val next : t -> t
 (** [next n] computes the successor of [n]. *)
 val zero : t
 (** [zero] represents [0]. *)
 val one : t
 (** [one] represents [1]. *)

 (** {3 Integer Operations} *)

 val bits : t -> t
 (** [bits n] returns the smallest [b] such that [n] can be represented in
 binary with [b] bits. *)
 val bits_int64 : Int64.t -> t
 (** [bits_int64 n] same as [bits t] but returns [big_int]*)
 val bits_big_int : Big_int.big_int -> t
 (** [bits_big_int n] same as [bits t] but returns [big_int]*)
 val bit_max : t -> t
 (** [bit_max n] returns the largest number representable with [n] bits. *)
 val bit_max_big_int : t -> Big_int.big_int
 (** [bit_max_big_int n] returns the largest number representable with [n] bits. *)
 val bit_max_int64 : t -> Int64.t
 (** [bit_max_int64 n] returns the largest number representable with [n] bits. *)
 val gcd : t -> t -> t
 (** [gcd n m] returns the greatest common divisor of [n] and [m]. Does not
 work for negative values. *)
 val lcm : t -> t -> t
 (** [lcm n m] returns the least common multiple of [n] and [m]. Does not
 work for negative values. *)
 val one_complement : t -> t
 (** [one_complement n] returns the one-complement of [n], i.e. [-n]. *)
 val pow : t -> t -> t
 (** [pow n m] computes [n^m].
 @raise Failure "negative exponent" if [y] is a negative number.
 @raise Failure "overflow" if an overflow occurred during the computation. *)
 val square : t -> t
 (** [square n] returns the square of [n], i.e. [n*n]. *)

 (** {3 Properties} *)

 val even : t -> bool
 (** [even n] checks if [n] is even. *)
 val is_zero : t -> bool
 (** [is_zero n] checks if [n] is [0]. *)
 val odd : t -> bool
 (** [odd n] checks if [n] is odd. *)

 (** {3 Miscellaneous} *)

 val binary : t -> t list
 (** [copy n] returns the binary representation of [n]. *)
 val copy : t -> t
 (** [copy n] returns a copy of [n]. *)
 val hash : t -> int
 (** [hash n] returns a hash value for [n]. *)

 (** {3 Compare Functions} *)

 val compare: t -> t -> int
 (** [compare n m] compares [n] and [m]. If [s] and [t] are equal, [0] is
 returned. If [s] is smaller than [t], [-1], and if [s] is greater than [t],
 [1] is returned. *)
 val equal : t -> t -> bool
 (** [equal n m] checks if [n] and [m] are equal. Note that this function is
 equivalent to [compare n m = 0]. *)
 val eq : t -> t -> bool
 (** [eq] is same as [equal]. *)
 val ge : t -> t -> bool
 (** [ge a b] computes [a >= b]. *)
 val gt : t -> t -> bool
 (** [gt a b] computes [a > b]. *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt n] prints [n] using the [OCaml] module [Format]. *)
 val fprintfx : Format.formatter -> t -> unit
 (** [fprintfx fmt n] prints certified [n] using the [OCaml] module [Format]. *)
 val to_string : t -> string
 (** [fprintf fmt n] transforms [n] into a formatted string. *)
end
 
(** {2 Module Isomorphism} *)

(** Isomorphism
@author Martin Korp, Christian Sternagel
@since  Wed Sep  3 09:11:32 CEST 2008 *)

(** This module provides an implementation of an isomorphism. An isomorphism
interconnects elements via a bijective map. *)
module Isomorphism : sig
 (*** MODULE TYPES ************************************************************)
 module type DOMAIN = sig
  type t
 
  val compare : t -> t -> int
  (** [compare s t] compares [s] and [t]. It must be guaranteed that
  [compare s t = 0] if [s] and [t] are equal, [compare s t < 0] if [s]
  is smaller than [t], and [compare s t > 0] if [s] is greater than [t]. *)
  val copy : t -> t
  (** [copy t] copies the value [t]. *)
  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt t] prints [t] using the [OCaml] module [Format]. *)
  val hash : t -> int
  (** [hash t] returns the hash value of [t]. It must be guaranteed that
  [hash s = hash t] whenever [compare s t = 0]. *)
 end
 
 module type RANGE = DOMAIN

 module type SIGNATURE = sig
  (*** TYPES ******************************************************************)
  type domain
  type range
  type t
  
  (*** VALUES *****************************************************************)
  (** {3 Constructors} *)

  val add : domain -> range -> t -> t
  (** [add d r i] adds a binding from the element [d] to the element [r] and
  vice versa. If there exists already a binding for [d] or [r], the isomorphism
  remains unchanged. Note that as a side effect, the original isomorphism is
  changed too. *)
  val empty : int -> t
  (** [empty n] creates an isomorphism which will store approximately [n]
  bindings. *)
  val replace : domain -> range -> t -> t
  (** [replace d r i] adds a binding from [d] to [r] and vice versa. If there
  exists already a binding for [d] or [r], it is replaced. Note that as a side
  effect, the original isomorphism is changed too. *)

  (** {3 Iterators} *)

  val fold : (domain -> range -> 'a -> 'a) -> 'a -> t -> 'a
  (** [fold f d i] computes [f s_n t_n (... (f s_1 t_1 d) ...)], where
  [s_1, ..., s_n] are elements of the domain and [t_1, ..., t_n] are elements
  of the range. Each binding is presented exactly once to [f]. The order in
  which the bindings are passed to [f] is unspecified. *)
  val iter : (domain -> range -> unit) -> t -> unit
  (** [iter f i] applies the function [f] to all bindings in [i]. The
  function [f] receives as first argument an element of the domain and as
  second argument the associated value of type [range]. *)
  val iteri : (int -> domain -> range -> unit) -> t -> unit
  (** [iteri f i] applies the function [f] to all bindings in [i]. The
  function [f] receives as first argument the index of the binding, as
  second argument an element of the domain, and as third argument the
  associated value of type [range]. *)

  (** {3 Scan Functions} *)

  val mem_dom : domain -> t -> bool
  (** [mem_dom d i] checks if [i] contains a binding for the element [d]. *)
  val mem_ran : range -> t -> bool
  (** [mem_ran r i] checks if [i] contains a binding for the element [r]. *)

  (** {3 Search Functions} *)

  val find_dom : range -> t -> domain
  (** [find_dom r i] returns the element [d] associated with [r].
  @raise Not_found if there is no binding for [r]. *)
  val find_ran : domain -> t -> range
  (** [find_ran d i] returns the element [r] associated with [d].
  @raise Not_found if there is no binding for [d]. *)

  (** {3 Miscellaneous} *)

  val clear : t -> t
  (** [clear i] removes all bindings from the isomorphism [i]. Note that as a
  side effect, the original isomorphism is changed too. *)
  val copy : t -> t
  (** [copy i] copies the isomorphism [i]. *)
  val domain : t -> domain list
  (** [domain i] returns the elements of the domain of [i]. *)
  val range : t -> range list
  (** [range i] returns the elements of the range of [i]. *)
  val size : t -> int
  (** [size i] returns the number of bindings stored in the isomorphism [i]. *)

  (** {3 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt i] prints [i] using the [OCaml] module [Format]. *)
  val to_string : t -> string
  (** [to_string i] transforms [i] into a formatted string. *)
 end
 
 (*** MODULES *****************************************************************)
 module Make (D : DOMAIN) (R : RANGE) : SIGNATURE with
  type domain = D.t and type range = R.t
 (** Functor building an isomorphism. The elements of the domain are of
 module type {!modtype: Util.Isomorphism.DOMAIN} whereas the elements of
 the range are of module type {!modtype: Util.Isomorphism.RANGE}. *)
end

(** {2 Module List} *)

(** Extended List
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Wed Mar  7 13:00:09 CET 2007
*)

(** This is an extension of the standard [OCaml] module [List]. *)
module List : sig
 (*** MODULE TYPES ************************************************************)
 module type LIST = sig
  val append : 'a list -> 'a list -> 'a list
  val assq : 'a -> ('a * 'b) list -> 'b
  val combine : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val exists : ('a -> bool) -> 'a list -> bool
  val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val filter : ('a -> bool) -> 'a list -> 'a list
  val find : ('a -> bool) -> 'a list -> 'a
  val find_all : ('a -> bool) -> 'a list -> 'a list
  val flatten : 'a list list -> 'a list
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
  val for_all : ('a -> bool) -> 'a list -> bool
  val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val hd : 'a list -> 'a
  val iter : ('a -> unit) -> 'a list -> unit
  val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
  val length : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val mem_assq : 'a -> ('a * 'b) list -> bool
  val memq : 'a -> 'a list -> bool
  val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  val nth : 'a list -> int -> 'a
  val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
  val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
  val rev : 'a list -> 'a list
  val rev_append : 'a list -> 'a list -> 'a list
  val rev_map : ('a -> 'b) -> 'a list -> 'b list
  val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val split : ('a * 'b) list -> 'a list * 'b list
  val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val tl : 'a list -> 'a list
 end
 (** This module type contains all standard [OCaml] functions provided by the
 module [List]. *)
 
 (*** INCLUDES ****************************************************************)
 include LIST
 
 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val cons : 'a -> 'a list -> 'a list
 (** [cons x xs] returns [x :: xs]. *)
 val init : 'a list -> 'a list
 (** [init xs] returns the list [xs] without the last element.
 @raise Failure "empty list" if [xs] is the empty list. *)
 val insert : int -> 'a -> 'a list -> 'a list
 (** [insert p e xs] inserts the element [e] at position [p] in list [xs]. If
 [p <= 0], [e] is inserted at the beginning of the list and if [p >= length xs],
 [e] is appended to the end of the list. *)
 val last : 'a list -> 'a
 (** [last xs] returns the last element of the list [xs].
 @raise Failure "empty list" if [xs] is the empty list. *)
 val remove : ?c:('a -> 'a -> int) -> 'a -> 'a list -> 'a list
 (** [remove ~c:c x xs] removes the first occurrence of [x] in [xs] using the
 compare function [c]. Per default, the compare function [compare] is used. *)
 val remove_all : ?c:('a -> 'a -> int) -> 'a -> 'a list -> 'a list
 (** [remove_all ~c:c x xs] removes all occurrences of [x] in [xs] using the
 compare function [c]. Per default, the compare function [compare] is used. *)
 val remove_equal : ?c:('a -> 'a -> int) -> 'a list -> 'a list
  -> 'a list * 'a list
 (** [remove_equal ~c:c xs ys] removes leading identical elements of both [xs]
 and [ys] and returns the remaining lists. Note that equality is tested by
 using the compare function [c]. Per default, the compare function [compare]
 is used. *)
 val replace : ?c:('a -> 'a -> int) -> 'a -> 'a -> 'a list -> 'a list
 (** [replace ~c:c x y xs] replaces all occurrences of [x] in [xs] by
 [y] using the compare function [c]. Per default, the compare function
 [compare] is used. *)
 val rev_concat : 'a list list -> 'a list
 (** [rev_concat xs] is equivalent to [rev (concat xs)] but tail-recursive. *)
 val singleton : 'a -> 'a list
 (** [singleton x] returns the singleton list [x]. *)
 
 (** {3 Iterators} *)

 val flat_map : ('a -> 'b list) -> 'a list -> 'b list
 (** [flat_map f xs] is equivalent to [concat (map f xs)] but tail-recursive
 and more efficient. *)
 val flat_mapi : (int -> 'a -> 'b list) -> 'a list -> 'b list
 (** [flat_mapi f xs] is equivalent to {!val: Util.List.flat_map} except that
 the function [f] receives as first argument the index of the considered
 element. Note that the first element has index 0. *)
 val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
 (** [foldl f d xs] is equivalent to [fold_left]. *)
 val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a
 (** [foldl1 f xs] is equivalent to [foldl f (hd xs) (tl xs)].
 @raise Failure "empty list" if [xs] is the empty list. *)
 val foldl2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
 (** [foldl2 f d xs ys] is equivalent to [fold_left2]. *)
 val foldli : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a
 (** [foldli f d xs] is equivalent to {!val: Util.List.foldl} except that [f]
 takes as first argument the index of the considered element. Note that the
 first element has index [0]. *)
 val foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
 (** [foldr f d xs] is equivalent to [fold_right f xs d]. This function is not
 tail-recursive. *)
 val foldr1 : ('a -> 'a -> 'a) -> 'a list -> 'a
 (** [foldr1 f xs] is equivalent to [foldr f (hd xs) (tl xs)]. This function
 is not tail-recursive.
 @raise Failure "empty list" if [xs] is the empty list. *)
 val foldr2 : ('a -> 'b -> 'c -> 'c) -> 'c -> 'a list -> 'b list -> 'c
 (** [foldr2 f d xs ys] is equivalent to [fold_right2 f xs ys d]. This
 function is not tail-recursive. *)
 val foldri : (int -> 'a -> 'b -> 'b) -> 'b -> 'a list -> 'b
 (** [foldri f d xs] is similar to [fold_right] except that [f] takes as
 first argument the index of the considered element. Note that the first
 element has index [0]. This function is not tail-recursive. *)
 val fold_left1 : ('a -> 'a -> 'a) -> 'a list -> 'a
 (** [fold_left1 f xs] is equivalent to {!val: Util.List.foldl1}.
 @raise Failure "empty list" if [xs] is the empty list. *)
 val fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a
 (** [fold_lefti f d xs] is equivalent to {!val: Util.List.foldli}. *)
 val fold_right1 : ('a -> 'a -> 'a) -> 'a list -> 'a
 (** [fold_right1 f xs] is equivalent to {!val: Util.List.foldr1}. This
 function is not tail-recursive.
 @raise Failure "empty list" if [xs] is the empty list. *)
 val fold_righti : (int -> 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b
 (** [fold_righti f d xs] is equivalent to {!val: Util.List.foldri}. This
 function is not tail-recursive. *)
 val gen : (int -> 'a) -> int -> 'a list
 (** [gen f n] is equivalent to [map_range f 0 n]. *)
 val iteri : (int -> 'a -> unit) -> 'a list -> unit
 (** [iteri f xs] applies the function [f] to all elements of [xs]. Note that
 as first argument the function [f] gets the index of the considered element,
 where the first element has index [0]. *)
 val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
 (** [mapi f xs] applies the function [f] to all elements of [xs] and returns
 the resulting list. Note that as first argument the function [f] gets the
 index of the considered element where the first element has index [0]. Unlike
 [map] this function is tail-recursive. *)
 val map_option : ('a -> 'b option) -> 'a list -> 'b list
 (** [map_option f xs] is equivalent to {!val: Util.List.map_tl} just that
 elements for which [f] returns [None] are omitted. *)
 val map_range : (int -> 'a) -> int -> int -> 'a list
 (** [map_range f n m] is equivalent to [map f (range n m)] but more efficient
 and tail-recursive. *)
 val map_tl : ('a -> 'b) -> 'a list -> 'b list
 (** [map_tl f xs] is equivalent to [map] but tail-recursive. *)
 val rev_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
 (** [rev_mapi f xs] is equivalent to {!val: Util.List.mapi} except that the
 returned list is in reversed order. *)

 (** {3 Scan Functions} *)

 val count : 'a -> 'a list -> int
 (* [count elt xs] counts the occurrences of [elt] in [xs] *) 

 val existsi : (int -> 'a -> bool) -> 'a list -> bool
 (** [existsi f xs] is similar to [exists] except that [f] takes as first
 argument the index of the considered element. Note that the first element
 has index [0]. *)
 val for_alli : (int -> 'a -> bool) -> 'a list -> bool
 (** [for_alli f xs] is similar to [for_all] except that [f] takes as first
 argument the index of the considered element. Note that the first element
 has index [0]. *)
 val mem : ?c:('a -> 'a -> int) -> 'a -> 'a list -> bool
 (** [map ~c:c x xs] checks if [x] is contained in the list [xs] using the
 compare function [c] to check equivalence. Per default, the compare function
 [compare] is used. *)

 (** {3 Search Functions} *)

 val filteri : (int -> 'a -> bool) -> 'a list -> 'a list
 (** [filteri p xs] is equivalent to the function [filter] except that the
 function [f] receives as first argument the index of the considered element.
 Note that the first element has index [0]. *)
 val findi : (int -> 'a -> bool) -> 'a list -> 'a
 (** [findi p xs] behaves similar as the function [find] except that [f] takes
 as first argument the index of the considered element. Note that the first
 element has index [0]. 
 @raise Not_found if no element is found that satisfies [p]. *)
 val position : ('a -> bool) -> 'a list -> int
 (** [position p xs] returns the position of the first element that satisfies
 the predicate [p] (counting starts at [0]).
 @raise Not_found if [xs] does not contain such an element. *)
 
 (** {3 Sublists} *)

 val drop : int -> 'a list -> 'a list
 (** [drop n xs] drops the first [n] elements of [xs]. If [n <= 0], [xs] is
 returned and if [n >= length xs], [\[\]] is returned. *)
 val drop_while : ('a -> bool) -> 'a list -> 'a list
 (** [drop_while p xs] drops the longest prefix of [xs], such that all elements
 satisfy the predicate [p]. *)
 val group : ?c:('a -> 'a -> int) -> 'a list -> 'a list list
 (** [group ~c:c xs] returns a list of lists such that the concatenation of the
 result is equal to the argument. Moreover, each sublist in the result contains
 only equal elements. Equality is checked via the compare function [c]. Per
 default [c = compare]. If [xs] is the empty list, [\[\]] is returned. *)
 val rev_span : ('a -> bool) -> 'a list -> 'a list * 'a list
 (** [rev_span p xs] similar to {!val: Util.List.span} except that the first
 list of the result is in reverse order. *)
 val rev_spani : (int -> 'a -> bool) -> 'a list -> 'a list * 'a list
 (** [rev_spani p xs] similar to {!val: Util.List.spani} except that the first
 list of the result is in reverse order. *)
 val rev_split_at : int -> 'a list -> 'a list * 'a list
 (** [rev_split_at p xs] is equivalent to {!val: Util.List.split_at} except
 that the first list of the result is in reverse order. *)
 val rev_split_last : int -> 'a list -> 'a list * 'a list
 (** [rev_split_last p xs] is equivalent to {!val: Util.List.split_last} except
 that the first list of the result is in reversed order. *)
 val span : ('a -> bool) -> 'a list -> 'a list * 'a list
 (** [span p xs] splits the list [xs] into two sublists. The first sublist
 is the longest prefix of [xs], such that all elements satisfy the predicate
 [p]. *)
 val spani : (int -> 'a -> bool) -> 'a list -> 'a list * 'a list
 (** [spani p xs] similar to {!val: Util.List.span} except that [p] takes as
 first argument the index of the considered element. Note that the first
 element has index [0]. *)
 val split_at : int -> 'a list -> 'a list * 'a list
 (** [split_at p xs] splits the list [xs] at position [p] such that the
 first list contains [p] elements. If [p <= 0], [(\[\],xs)] is returned
 and if [p >= length xs], [(xs,\[\])] is returned. *)
 val split_last : int -> 'a list -> 'a list * 'a list
 (** [split_last p xs] splits the list [xs] into two lists such that the second
 list contains [p] elements. If [p <= 0], [(xs,\[\])] is returned
 and if [p >= length xs], [(\[\],xs)] is returned. *)
 val take : int -> 'a list -> 'a list
 (** [take n xs] returns the first [n] elements of [xs]. If [n <= 0], [\[\]] is
 returned and if [n >= length xs], [xs] is returned.*)
 val take_while : ('a -> bool) -> 'a list -> 'a list
 (** [take_while p xs] returns the longest prefix of [xs], such that all
 elements satisfy the predicate [p]. *)

 (** {3 Zip Functions} *)

 val rev_unzip : ('a * 'b) list -> 'a list * 'b list
 (** [rev_unzip xs] is equivalent to [rev_unzip_with id xs]. *)
 val rev_unzip_with : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list
 (** [rev_unzip_with f xs] is equivalent to {!val: Util.List.unzip_with}
 except that the elements of the returned lists are in reversed order. Note
 that this function is more efficient than {!val: Util.List.unzip_with}. *)
 val rev_zip : 'a list -> 'b list -> ('a * 'b) list
 (** [rev_zip xs ys] is equivalent to [rev_zip_with pair xs ys]. *)
 val rev_zip_with : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
 (** [rev_zip_with f xs ys] is equivalent to {!val: Util.List.zip_with} except
 that the elements of the returned list are in reversed order. Note that this
 function is more efficient than {!val: Util.List.zip_with}. *)
 val unzip : ('a * 'b) list -> 'a list * 'b list
 (** [unzip xs] is equivalent to [unzip_with id xs]. *)
 val unzip_with : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list
 (** [unzip_with f xs] unzips the elements of [xs] using the function [f]. *)
 val zip : 'a list -> 'b list -> ('a * 'b) list
 (** [zip xs ys] is equivalent to [zip_with pair xs ys]. *)
 val zip_with : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
 (** [zip_with f xs ys] zips the lists [xs] and [ys] using the function [f] such
 that [f] is applied to the i-th elements of [xs] and [ys]. If the two lists
 have different length, the abundant elements are ignored. *)

 (** {3 Properties} *)

 val is_empty : 'a list -> bool
 (** [is_empty xs] checks if [xs] is the empty list. *)
 val is_prefix : ?c:('a -> 'a -> int) -> 'a list -> 'a list -> bool
 (** [is_prefix ~c:c xs ys] checks if [xs] is a prefix of [ys]. *)
 val is_proper_prefix : ?c:('a -> 'a -> int) -> 'a list -> 'a list -> bool
 (** [is_proper_prefix ~c:c xs ys] checks if [xs] is a proper prefix of [ys]. *)
 val is_proper_suffix : ?c:('a -> 'a -> int) -> 'a list -> 'a list -> bool
 (** [is_proper_suffix ~c:c xs ys] checks if [xs] is a proper suffix of [ys]. *)
 val is_singleton : 'a list -> bool
 (** [is_singleton xs] checks if [xs] is a singleton list. *)
 val is_suffix : ?c:('a -> 'a -> int) -> 'a list -> 'a list -> bool
 (** [is_suffix ~c:c xs ys] checks if [xs] is a suffix of [ys]. *)

 (** {3 Association Lists} *)

 val assoc : ?c:('a -> 'a -> int) -> 'a -> ('a * 'b) list -> 'b
 (** [assoc ~c:c x xs] returns the first value associated with key [x] in the
 list of pairs [xs]. Equality is tested using the compare function [c]. Per
 default [c = compare].
 @raise Not_found if there is no value associated with [x] in the list [xs]. *)
 val mem_assoc : ?c:('a -> 'a -> int) -> 'a -> ('a * 'b) list -> bool
 (** [mem_assoc ~c:c x xs] checks if [xs] contains a value associated with key
 [x]. Equality is tested using the compare function [c]. Per default [c] is the
 standard compare function [compare]. *)
 val remove_assoc : ?c:('a -> 'a -> int) -> 'a -> ('a * 'b) list
  -> ('a * 'b) list
 (** [remove_assoc ~c:c x xs] returns the list of pairs [xs] without the first
 pair with key [x], if any. Equality is tested using the compare function [c].
 Per default [c = compare]. This function is not tail-recursive. *)

 (** {3 Lists as Sets} *)

 val diff : ?c:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
 (** [diff ~c:c xs ys] removes all occurrences of elements in [ys] from
 [xs] using the compare function [c]. Per default, the compare function
 [compare] is used. *)
 val equal : ?c:('a -> 'a -> int) -> 'a list -> 'a list -> bool
 (** [equal ~c:c xs ys] checks if [xs] is a subset of [ys] and vice versa using
 the compare function [c]. Per default [c = compare]. *)
 val flat_product : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
 (** [flat_product f xs ys] is equivalent to [map (uncurry f) (product xs ys)]
 but tail-recursive and more efficient. *)
 val flat_times : ('b -> 'a -> 'b) -> 'b -> 'a list list -> 'b list
 (** [flat_times f d xs] is equivalent to {!val: Util.List.times} except that
 the resulting lists are combined via the function [f]. As default value [d]
 is used. *)
 val intersect : ?c:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
 (** [intersect ~c:c xs ys] returns a sublist of [xs] containing all elements
 that occur in [ys]. Equality is tested using the compare function [c]. Per
 default [c = compare]. *)
 val is_proper_subset : ?c:('a -> 'a -> int) -> 'a list -> 'a list -> bool
 (** [is_proper_subset ~c:c xs ys] checks if all elements of [xs] occur in [ys]
 but not all elements of [ys] occur in [xs]. To compare elements, the function
 [c] is used. Per default [c = compare]. *)
 val is_subset : ?c:('a -> 'a -> int) -> 'a list -> 'a list -> bool
 (** [is_subset ~c:c xs ys] checks if all elements of [xs] occur in [ys] using
 the compare function [c]. Per default [c = compare]. *)
 val is_supset : ?c:('a -> 'a -> int) -> 'a list -> 'a list -> bool
 (** [is_supset ~c:c xs ys] is equivalent to [is_subset ~c:c ys xs]. Per
 default [c = compare]. *)
 val powerset : 'a list -> 'a list list
 (** [powerset xs] returns the powerset of the elements in the list [xs].
 Note that the order of the elements and lists is preserved. However,
 duplicates in [xs] are not removed. *)
 val product : 'a list -> 'b list -> ('a * 'b) list
 (** [product xs ys] returns the Cartesian product
 [\[(x_1, y_1); (x_1, y_2); ... (x_n, y_m)\]] of the lists
 [xs = \[x_1; ...; x_n\]] and [ys = \[y_1; ...; y_m\]]. Note
 that the order of the elements and lists is preserved. *)
 val rev_times : 'a list list -> 'a list list
 (** [rev_times f xs] is equivalent to {!val: Util.List.times} except that the
 elements of the resulting lists are in reversed order. *)
 val square : 'a list -> ('a * 'a) list
 (** [square xs] is equivalent to [product xs xs]. *)
 val times : 'a list list -> 'a list list
 (** [times xs] calculates the Cartesian product of the list [xs], i.e., if
 [xs = \[\[x_11; ...; x_1i\]; ...; \[x_j1; ...; x_jk\]\]] then the result is
 [\[\[x_11; ...; x_j1\]; \[x_12; ...; x_j1\]; ...; \[x_1i; ..., x_jk\]\]].
 Note that the order of the elements and lists is preserved. *)
 val union : ?c:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
 (** [union ~c:c xs ys] merges [xs] with [ys] and removes duplicates (elements
 of [ys] that occur in [xs]) using the compare function [c]. The order of the
 elements and lists is preserved, i.e., the first occurrence of an element is
 kept. Note that the resulting list does not contain any duplicated elements if
 [xs] and [ys] are unique lists. Furthermore, per default [c = compare]. *)
 val unique : ?c:('a -> 'a -> int) -> 'a list -> 'a list
 (** [unique ~c:c xs] returns the list [xs] without any duplicated elements.
 The function [c] is used to compare elements. Per default the compare function
 [compare] is used. Note that the order of the elements is preserved, i.e., the
 first occurrence of an element is kept. *)
 val unique_hash : 'a list -> 'a list
 (** [unique_hash xs] returns the list [xs] without any duplicated elements.
 As compare function [compare] is used. Note that the order of the elements is
 preserved, i.e., the first occurrence of an element is kept. *)

 (** {3 Miscellaneous} *)

 val combinations : int -> 'a list -> 'a list list
 (** [combinations i xs] returns a list, containing all possible combinations
 of [i] elements of [xs]. *)
 val copy : 'a list -> 'a list
 (** [copy xs] returns a copy of the list [xs]. *)
 val extend : ?c:('a -> 'a) -> int -> 'a list -> 'a list
 (** [extend ~c:c n xs] returns a list of length [n] filled with elements
 of [xs]. If [xs] contains at least [n] elements, the first [n] elements of
 [xs] are returned. Otherwise [xs] is appended to itself until a list of
 length [n] is reached. Note that elements are copied via the function [c].
 Per default [c = id]. *)
 val range : int -> int -> int list
 (** [range n m] returns the list [\[n; n + 1; ...; m - 1\]]. If [n = m]
 then [\[\]] is returned. *)
 val replicate : ?c:('a -> 'a) -> int -> 'a -> 'a list
 (** [replicate ~c:c n x] returns a list containing [n] copies of [x].
 Elements are copied via the function [c]. Per default [c = id]. *)
 val transpose : 'a list list -> 'a list list
 (** [transpose xs] transposes the lists in [xs].
 @raise Failure "different list lengths" if the lists in [xs] do not have
 equal lengths. *)

 (** {3 Lists of Lists} *)

 val collapse : ?n:int -> 'a list list -> 'a list list
 (** [collapse n xs] collapses the list of lists [xs] into at most
 [n] chunks (by concatenating consecutive elements), e.g.,
 [chunks 3 \[\[1;2;3\];\[4\];\[5\];\[6;7\];\[8\]\]]
 results in [\[\[1;2;3;4\];\[5;6;7\];\[8\]\]]. *)

 (** {3 Printers} *)

 val fprintf : (Format.formatter -> 'a -> unit)
  -> (unit,Format.formatter,unit) Pervasives.format -> Format.formatter
  -> 'a list -> unit
 (** [fprintf f d fmt xs] writes pretty printed output of [xs]. *)
 val fprintfi : (int -> Format.formatter -> 'a -> unit)
  -> (unit,Format.formatter,unit) Pervasives.format -> Format.formatter
  -> 'a list -> unit
 (** [fprintfi f d fmt xs] writes pretty printed output of [xs]. Note that [f]
 receives as first argument the index of the considered element, where the
 first element has index [0]. *)
 val join : ('a -> string) -> (unit,Format.formatter,unit) Pervasives.format
  -> 'a list -> string
 (** [join f d xs] transforms elements from [xs] into a formatted string
 separated by [d]. *)
 val joini : (int -> 'a -> string)
  -> (unit,Format.formatter,unit) Pervasives.format -> 'a list -> string
 (** [joini f d xs] transforms elements from [xs] into a formatted string
 separated by [d]. Note that [f] receives as first argument the index of the
 considered element, where the first element has index [0]. *)
 val to_string : ('a -> string)
  -> (unit,Format.formatter,unit) Pervasives.format -> 'a list -> string
 (** [to_string f d xs] is equivalent to {!val: Util.List.join}. *)
 val to_stringi : (int -> 'a -> string)
  -> (unit,Format.formatter,unit) Pervasives.format -> 'a list -> string
 (** [to_stringi f d xs] is equivalent to {!val: Util.List.joini}. *)
end

(** {2 Module LazyList} *)

(** Lazy List
@author Christian Sternagel
@since  Mon May 11 13:38:15 CEST 2009
*)

(** This is an implementation of lazy lists. *)
module LazyList : sig
 (*** TYPES *******************************************************************)
 type 'a t = 'a cell Lazy.t
 and 'a cell = Nil | Cons of ('a * 'a t)

 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val append : 'a t -> 'a t -> 'a t
 (** [append xs ys] concatenates the two lazy lists [xs] and [ys]. *)
 val combinations : int -> 'a t ->'a t t
 (** [combinations n xs] constructs all possible combinations of [n]
 elements from [xs]. *)
 val concat : 'a t t -> 'a t
 (** [concat ls] is equivalent to [!val: Util.LazyList.flatten]. *)
 val empty : 'a t
 (** The empty lazy list. *)
 val flatten : 'a t t -> 'a t
 (** [flatten ls] transforms a lazy list of lazy lists into
 a lazy list by concatenating all elements of [ls]. *)
 val filter : ('a -> bool) -> 'a t -> 'a t
 (** [filter f ls] removes all elements from [ls] that do not
 satisfy the predicate [f]. *)
 val hd : 'a t -> 'a
 (** Extracts the first element of the given lazy list.
 @raise Failure "empty" if the lazy list was empty. *)
 val init : int -> (int -> 'a) -> 'a t
 (** [init i f] Initializes a lazy list with [i] elements generated
 by the function [f]. *)
 val iterate : ('a -> 'a) -> 'a -> 'a t
 (** [iterate f x] returns the infinite list [\[x;f x;f(f x); ...\]]. *)
 val map : ('a -> 'b) -> 'a t -> 'b t
 (** [map f ls] maps the lazy list [\[e1; e2; ...\]] to
 the lazy list [\[f e1; f e2; ...\]]. *)
 val make : 'a -> ('a -> 'a) -> 'a t
 (** [make i n] creates an infinite list with first element
 [i] and all following elements obtained by applying [f] to
 their immediate predecessor i.e. [\[i; f i; f (f i); ...\]]. *)
 val nth : int -> 'a t -> 'a
 (** [nth i xs] extracts the [i]-th element of [xs].
 @raise Failure "out of bounds" if the list does not contain index [i]. *)
 val of_channel : in_channel -> char t
 (** [of_channel c] returns the content of channel [c] as a lazy list
 of characters. *)
 val of_file : string -> char t
 (** [of_file f] returns the content of file [f] as a lazy list
 of characters. *)
 val of_function : (int -> 'a option) -> 'a t
 (** [of_function f] applies the function [f] in turn to the values
 [0], [1], [2], ... As long as [f i] generates [Some x], [x] is
 used as [i]th element of the resulting lazy list. The list ends
 as soon as [None] is returned. *)
 val of_list : 'a list -> 'a t
 (** [of_list xs] transforms the given list [xs] into a lazy list. *)
 val of_string : string -> char t
 (** [of_string s] is a lazy list containing the characters of
 string [s]. *)
 val tl : 'a t -> 'a t
 (** Extracts all but the first element of the given lazy list.
 @raise Failure "empty" if the lazy list was empty. *)
 val to_list : ?n:int -> 'a t -> 'a list
 (** Transforms a lazy list into a list. This function does not
 terminate if the given lazy list was infinite. If [n] is specified,
 at most [n] elements are taken from the lazy list. *)
 val permutations : 'a t -> 'a t t
 (** [permutations ls]. *)
 val take : int -> 'a t -> 'a t
 (** [take n xs] returns the prefix of [xs] with length [n], or
 [xs] itself if it has less elements than [n]. *)
 val zip : 'a t -> 'b t -> ('a * 'b)t
 (** Similar to the list function. *)

 (** {3 Reducing Lists (Folds)} *)

 val foldr : ('a -> 'b Lazy.t -> 'b Lazy.t) -> 'b Lazy.t -> 'a t -> 'b Lazy.t
 (** A lazy version of [fold_right] for lists. *)

 (** {3 Search Functions} *)

 val find : ('a -> bool) -> 'a t -> 'a
 (** [find p xs] returns the first element of [xs] satisfying [p].
 This may lead to nontermination for infinite lists.
 @raise Not_found if the end of the list is reached. *)
 val mem : ?n : int -> 'a -> 'a t -> bool
 (** [mem ?n:i e ls] checks whether [e] is among the first [i]
 elements of [ls].

 [mem e ls] checks whether [e] is an element of [ls]. If [ls]
 is infinite this function may not terminate. *)

 (** {3 Properties} *)

 val null : 'a t -> bool
 (** Test whether a lazy list is empty. *)

 (** {3 Printers} *)
 
 val fprintf : (Format.formatter -> 'a -> unit)
  -> (unit,Format.formatter,unit) Pervasives.format -> Format.formatter
  -> 'a t -> unit
 (** [fprintf f d fmt xs] writes pretty printed output of [xs]. This will
 lead to nontermination for an infinite lazy list. *)
 val fprintfi : (int -> Format.formatter -> 'a -> unit)
  -> (unit,Format.formatter,unit) Pervasives.format -> Format.formatter
  -> 'a t -> unit
 (** [fprintfi f d fmt xs] writes pretty printed output of [xs]. Note that [f]
 receives as first argument the index of the considered element, where the
 first element has index [0]. This will lead to nontermination for an
 infinite lazy list. *)
end

(** {2 Module Stream} *)

(** Stateful Streams
@author Christian Sternagel
@since  Mon May 11 13:38:15 CEST 2009
*)

(** This is an implementation of stateful streams. The behavior is
very similar to lazy lists, just that no memoization is used and that
in addition a state is maintained. *)
module Stream : sig
 (*** MODULE TYPES ************************************************************)
 module type STATE = sig type t end

 module type SIGNATURE = sig
  (*** TYPES ******************************************************************)
  type state
  type 'a t

  (*** VALUES *****************************************************************)
  (** {3 Constructors} *)

  val gen : ('a -> state -> ('a * state) option) -> 'a -> 'a t
  (** [gen f x] uses the function [f] to generate a stream starting
  with the value [x]. As soon as [f] returns [None] (which may not
  happen at all), the end of the stream is reached. *)
  val of_streams : ('a -> 'b t) -> 'a list -> 'b t
  (** [of_streams f xs] takes the list [\[x1; ...; xN\]] and concatenates
  the streams resulting from [f x1], ..., [f xN]. *)
  val of_values : ('a -> state -> ('b * state) option) -> 'a list -> 'b t
  (** [of_values f xs] takes the list [\[x1; ...; xN\]] and returns the
  finite stream consisting of all those [xI] for which [f] did not
  return [None]. *)

  (** {3 Combining Streams} *)

  val append : 'a t -> 'a t -> 'a t
  (** [append s t] concatenates the two streams [s] and [t]. *)
  val concat : 'a t t -> 'a t
  (** [concat ss] takes a stream of streams [ss] and concatenates its
  contents. *)
  val flat_map : ('a -> 'b t) -> 'a t -> 'b t
  (** [flat_map f s] takes the stream [s] and applies on every
  element the function [f] (generating a stream). The result
  streams are concatenated*)
  val merge : 'a t -> 'a t -> 'a t
  (** [merge s t] produces a stream that returns alternating
  elements of the two streams [s] and [t]. *)

  (** {3 Filters} *)

  val filter : ('a -> bool) -> 'a t -> 'a t
  (** [filter p s] removes all elements from [s] that do not
  satisfy the property [p]. *)
  val sieve : ('a -> 'a -> bool) -> 'a t -> 'a t
  (** [sieve p s]. *)

  (** {3 Evaluation} *)

  val eval : 'a t -> state -> ('a * state) LazyList.t
  (** [eval stream s] uses the initial state [s] to compute
  the elements of the stream [stream], thereby transforming it
  into a lazy list of value-state pairs. *)

  (** {3 Miscellaneous} *)

  val iterate : ('a -> state -> 'a * state) -> 'a -> 'a t
  (** [iterate f x] generates the stream  *)
 end

 (*** MODULES *****************************************************************)
 module Make (State : STATE) : SIGNATURE with type state = State.t
 (** Functor building a stateful stream. The uses states are of module type
 {!modtype: Util.Stream.STATE}. *)
end

(** {2 Module Map} *)

(** Extended Map
@author Martin Korp
@since  Sat Nov 29 11:16:35 CEST 2008 *)

(** This is an extension of the standard [OCaml] module [Map]. *)
module Map : sig
 (*** MODULE TYPES ************************************************************)
 module type DOMAIN = sig
  type t
 
  val compare : t -> t -> int
  (** [compare s t] compares [s] and [t]. It must be guaranteed that
  [compare s t = 0] if [s] and [t] are equal, [compare s t < 0] if [s]
  is smaller than [t], and [compare s t > 0] if [s] is greater than [t]. *)
  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt t] prints [t] using the [OCaml] module [Format]. *)
 end
 
 module type RANGE = DOMAIN
 
 module type PARTIAL = sig
   type domain
   type 'a t

   val add: domain -> 'a -> 'a t -> 'a t
   val empty: 'a t
   val remove: domain -> 'a t -> 'a t
   val find: domain -> 'a t -> 'a
   val fold: (domain -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
   val iter: (domain -> 'a -> unit) -> 'a t -> unit
   val map: ('a -> 'b) -> 'a t -> 'b t
   val mapi: (domain -> 'a -> 'b) -> 'a t -> 'b t
   val mem:  domain -> 'a t -> bool
   val is_empty: 'a t -> bool
   val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
   val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
 end
 (** This module is equivalent to the standard [OCaml] module type [Map.S],
 except that type [key] is now called [domain]. *)

 module type SIGNATURE = sig
  (*** TYPES ******************************************************************)
  type domain
  type range
  type t
  
  (*** VALUES *****************************************************************)
  (** {3 Constructors} *)

  val add : domain -> range -> t -> t
  (** [add k e m] adds a new binding of [k] to [e] to [m]. If there exists
  already a binding for [k] in [m], the map remains unchanged. *)
  val copy : t -> t
  (** [copy m] returns a copy of [m]. *)
  val empty : t
  (** [empty] is equivalent to [Map.S.empty]. *)
  val of_list : (domain * range) list -> t
  (** [of_list xs] creates a map consisting of all bindings in [xs]. Note
  that multiples of one and the same binding in [xs] are added only once. *)
  val remove : domain -> t -> t
  (** [remove k m] is equivalent to [Map.S.remove]. *)
  val replace : domain -> range -> t -> t
  (** [replace k e m] is equivalent to [Map.S.add]. *)
  val singleton : domain -> range -> t
  (** [singleton k e] returns a map containing a singleton binding of [k] to
  [e]. *)
  val to_list : t -> (domain * range) list
  (** [to_list m] creates a list consisting of all bindings in [m]. *)

  (** {3 Search Functions} *)

  val find : domain -> t -> range
  (** [find k m] is equivalent to [Map.S.find].
  @raise Not_found if [m] does not contain a binding for [k]. *)
  val search : (domain -> range -> bool) -> t -> domain * range
  (** [search p m] returns the first binding that satisfies the predicate [p].
  @raise Not_found if [m] does not contain a binding that satisfies [p]. *)

  (** {3 Iterators} *)

  val fold : (domain -> range -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f m d] is equivalent to [Map.S.fold]. *)
  val foldi : (int -> domain -> range -> 'a -> 'a) -> t -> 'a -> 'a
  (** [foldi f m d] combines all elements of [m] using the function [f]. Note
  that as first argument the function [f] gets the index of the considered
  element, where the first element has index [0]. *)
  val iter : (domain -> range -> unit) -> t -> unit
  (** [iter f m] is equivalent to [Map.S.iter]. *)
  val iteri : (int -> domain -> range -> unit) -> t -> unit
  (** [iteri f m] applies the function [f] to all elements of [m]. Note that
  as first argument the function [f] gets the index of the considered element,
  where the first element has index [0]. *)
  val map : (range -> range) -> t -> t
  (** [map f m] is equivalent to [Map.S.map]. *)
  val mapi : (domain -> range -> range) -> t -> t
  (** [mapi f m] is equivalent to [Map.S.mapi]. *)

  (** {3 Scan Functions} *)

  val exists : (domain -> range -> bool) -> t -> bool
  (** [exists p m] checks whether the predicate [p] holds for at least one
  binding in [m]. *)
  val for_all : (domain -> range -> bool) -> t -> bool
  (** [for_all p m] checks whether the predicate [p] holds for all bindings in
  [m]. *)
  val mem : domain -> t -> bool
  (** [mem k m] is equivalent to [Map.S.mem]. *)

  (** {3 Properties} *)

  val is_empty : t -> bool
  (** [is_empty m] is equivalent to [Map.S.is_empty]. *)

  (** {3 Miscellaneous} *)

  val domain : t -> domain list
  (** [domain m] returns the domain of [m]. *)
  val range : t -> range list
  (** [range m] returns the range of [m]. *)
  val size : t -> int
  (** [size m] returns the number of bindings in [m]. *)

  (** {3 Compare Functions} *)

  val compare : t -> t -> int
  (** [compare m n] compares [m] and [n]. This function defines a total
  ordering on maps. *)
  val equal : t -> t -> bool
  (** [equal m n] tests whether [m] contains the same bindings as [n] and
  vice versa. This function is equivalent to [compare m n = 0]. *)

  (** {3 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt m] prints the replacement map [m] nicely. *)
  val to_string : t -> string
  (** [to_string m] transforms [m] into a formatted string. *)
 end

 (*** MODULES *****************************************************************)
 module Make (D : DOMAIN) (R : RANGE) : SIGNATURE with
  type domain = D.t and type range = R.t
 (** Functor building a map. The elements of the range are of module type
 {!modtype: Util.Map.RANGE} whereas the elements of the domain are of module
 type {!modtype: Util.Map.DOMAIN}. *)
 module Partial (D : DOMAIN) : PARTIAL with type domain = D.t
 (** Functor building a partial map. The elements of the domain are of module
 type {!modtype: Util.Map.DOMAIN}. The range can be chosen arbitrarily. Note
 that this functor is equivalent to the standard [OCaml] functor [Map.Make]. *)
end

(** {2 Module Monad} *)

(** Monads
@author Martin Korp, Christian Sternagel
@since  Fri Nov 21 19:18:15 CET 2008 *)

(** The following module contains some standard monads. Additionally,
various monad transformers are provided, to allow the combination of
different monads. *)
module Monad : sig
 (*** MODULE TYPES ************************************************************)
 module type ERROR = sig type t end
 module type STATE = sig type t end
 
 module type MINIMAL_MONAD = sig
  (*** TYPES ******************************************************************)
  type 'a t
 
  (*** VALUES *****************************************************************)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** [m >>= f] evaluates [m] and applies [f] to its result. *)
  val return : 'a -> 'a t
  (** [return d] constructs an instance of type [t] with value [d]. *)
 end
 
 module type SIGNATURE = sig
  (*** INCLUDES ***************************************************************)
  include MINIMAL_MONAD
 
  (*** VALUES *****************************************************************)
  (** {3 Miscellaneous} *)

  val (=<<) : ('a -> 'b t) -> 'a t -> 'b t
  (** [f =<< m] is equivalent to [m >>= f]. *)
  val (>>) : 'a t -> 'b t -> 'b t
  (** [m >> n] is equivalent to [m >>= const n]. *)
  val (<<) : 'b t -> 'a t -> 'b t
  (** [m << n] is equivalent to [n >> m]. *)
  val (>=>) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
  (** [(f >=> g) x] is equivalent to [f x >>= g]. *)
  val (<=<) : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
  (** [(f <=< g) x] is equivalent to [g x >=> f]. *)
  val ap : ('a -> 'b) t -> 'a t -> 'b t
  (** [ap m n] applies the argument of [m] to [n]. Note that the
  [ap] function can be used to get a generalized version of the
  {!val: Util.Monad.SIGNATURE.lift} function. I.e.,
  [lift_i f m_1 ... m_i = ap (... (ap (return f) m_1) ...) m_i]
  where [f] is an [i]-ary function. *)
  val join : 'a t t -> 'a t
  (** [join m] removes one level of monadic structure by projecting its
  argument into the outer level. *)
  val lift : ('a -> 'b) -> 'a t -> 'b t
  (** [lift f m] promotes the function [f] to the monad [m]. *)
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [lift2 f m n] promotes the function [f] to the monad [m] and [n]. *)
  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** [lift3 f m n o] promotes the function [f] to the monads [m], [n], and
  [o]. In situations with more than three arguments, this function has to be
  replaced by uses of {!val: Util.Monad.SIGNATURE.ap}, which promotes function
  application (see {!val: Util.Monad.SIGNATURE.ap} for more details). *)

  (** {3 List Functions} *)

  val exists : ('a -> bool t) -> 'a list -> bool t
  (** [exists p xs] is equivalent to {!val: Util.List.LIST.exists}, except
  that its result is encapsulated in a monad. *)
  val filter : ('a -> bool t) -> 'a list -> 'a list t
  (** [filter f xs] is equivalent to {!val: Util.List.LIST.filter}, except
  that its result is encapsulated in a monad. *)
  val flat_map : ('a -> 'b list t) -> 'a list -> 'b list t
  (** [flat_map f xs] is analogous to {!val: Util.List.flat_map}, except
  that its result is encapsulated in a monad. *)
  val flat_mapi : (int -> 'a -> 'b list t) -> 'a list -> 'b list t
  (** [flat_mapi f xs] is analogous to {!val: Util.List.flat_mapi}, except
  that its result is encapsulated in a monad. *)
  val foldl : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
  (** [foldl f x xs] is analogous to {!val: Util.List.foldl}, except that
  its result is encapsulated in a monad. *)
  val foldl1 : ('a -> 'a -> 'a t) -> 'a list -> 'a t
  (** [foldl1 f xs] is equivalent to {!val: Util.List.foldl1}, except that
  its result is encapsulated in a monad.
  @raise Failure "empty list" if [xs] is the empty list. *)
  val foldl2 : ('a -> 'b -> 'c -> 'a t) -> 'a -> 'b list -> 'c list -> 'a t
  (** [foldl2 f d xs ys] is equivalent to {!val: Util.List.foldl2}, except
  that its result is encapsulated in a monad. *)
  val foldli : (int -> 'a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
  (** [foldli f x xs] is analogous to {!val: Util.List.foldli}, except that
  its result is encapsulated in a monad. *)
  val foldr : ('a -> 'b -> 'b t) -> 'b -> 'a list -> 'b t
  (** [foldr f x xs] is analogous to {!val: Util.List.foldr}, except that
  its result is encapsulated in a monad. *)
  val foldr1 : ('a -> 'a -> 'a t) -> 'a list -> 'a t
  (** [foldr1 f xs] is equivalent to {!val: Util.List.foldr1}, except that
  its result is encapsulated in a monad. This function is not tail-recursive.
  @raise Failure "empty list" if [xs] is the empty list. *)
  val foldr2 : ('a -> 'b -> 'c -> 'c t) -> 'c -> 'a list -> 'b list -> 'c t
  (** [foldr2 f d xs ys] is equivalent to {!val: Util.List.foldr2}, except
  that its result is encapsulated in a monad. *)
  val foldri : (int -> 'a -> 'b -> 'b t) -> 'b -> 'a list -> 'b t
  (** [foldri f x xs] is analogous to {!val: Util.List.foldri}, except that
  its result is encapsulated in a monad. *)
  val for_all : ('a -> bool t) -> 'a list -> bool t
  (** [for_all p xs] is equivalent to {!val: Util.List.LIST.for_all}, except
  that its result is encapsulated in a monad. *)
  val iter : ('a ->  unit t) -> 'a list -> unit t
  (** [iter f r] is equivalent to {!val: Util.List.LIST.iter}, except that
  its result is encapsulated in a monad. *)
  val iteri : (int -> 'a -> unit t) -> 'a list -> unit t
  (** [iteri f r] is equivalent to {!val: Util.List.iteri}, except that
  its result is encapsulated in a monad. *)
  val map : ('a -> 'b t) -> 'a list -> 'b list t
  (** [map f xs] is equivalent to {!val: Util.List.LIST.map}, except that
  its result is encapsulated in a monad. *)
  val mapi : (int -> 'a -> 'b t) -> 'a list -> 'b list t
  (** [mapi f xs] is equivalent to {!val: Util.List.mapi}, except that
  its result is encapsulated in a monad. *)
  val replicate : int -> 'a t -> 'a list t
  (** [replicate n m] is analogous to {!val: Util.List.replicate}, except
  that its result is encapsulated in a monad. *)
  val replicatei : int -> (int -> 'a t) -> 'a list t
  (** [replicatei n f] executes [f], [n] times in turn, accumulating the
  results in a monad. At the i-th call, [i] is supplied to [f] (starting
  from 0). *)
  val rev_map : ('a -> 'b t) -> 'a list -> 'b list t
  (** [rev_map f xs] is equivalent to {!val: Util.List.LIST.rev_map}, except
  that its result is encapsulated in a monad. *)
  val rev_mapi : (int -> 'a -> 'b t) -> 'a list -> 'b list t
  (** [rev_mapi f xs] is equivalent to {!val: Util.List.rev_mapi}, except
  that its result is encapsulated in a monad. *)
  val sequence : 'a t list -> 'a list t
  (** [sequence ms] evaluates each action in [ms] from left to right and
  collects the results. *)

  (** {3 Pair Functions} *)

  val apply : ('a -> 'c t) -> ('b -> 'd t) -> 'a * 'b -> ('c * 'd) t
  (** [apply f g (x,y)] is equivalent to {!val: Util.Pair.apply}, except
  that its result is encapsulated in a monad. *)
  val cross : ('a -> 'c t) * ('b -> 'd t) -> 'a * 'b -> ('c * 'd) t
  (** [cross (f,g) (x,y)] is equivalent to {!val: Util.Pair.cross}, except
  that its result is encapsulated in a monad. *)
  val fold : ('a -> 'c -> 'c t) -> ('b -> 'c -> 'c t) -> 'c -> 'a * 'b -> 'c t
  (** [fold f g d (x,y)] is equivalent to {!val: Util.Pair.fold}, except
  that its result is encapsulated in a monad. *)
  val pair : ('a -> 'b t) * ('a -> 'c t) -> 'a -> ('b * 'c) t
  (** [pair (f,g) x] is equivalent to {!val: Util.Pair.pair}, except
  that its result is encapsulated in a monad. *)
  val project : ('a -> 'b t) -> 'a * 'a -> ('b * 'b) t
  (** [project f (x,y)] is equivalent to {!val: Util.Pair.map}, except
  that its result is encapsulated in a monad. *)
  val uncurry : ('a -> 'b -> 'c t) -> 'a * 'b -> 'c t
  (** [uncurry f (x,y)] is equivalent to {!val: Util.Pair.uncurry}, except
  that its result is encapsulated in a monad. *)

  (** {3 Boolean Functions} *)
 
  val ite : bool t -> ('a -> 'b t) -> ('a -> 'b t) -> 'a -> 'b t
  (** [ite m f g x] evaluates [m] and returns [f x] if [m] evaluates to [true]
  and [g x] otherwise. *)

  (** {3 Printers} *)
 
  val fprintf : (Format.formatter -> 'a -> unit t)
   -> (unit,Format.formatter,unit) Pervasives.format -> Format.formatter
   -> 'a list -> unit t
  (** [fprintf f d fmt xs] is equivalent to {!val: Util.List.fprintf}, except
  that its result is encapsulated in a monad. *)
  val fprintfi : (int -> Format.formatter -> 'a -> unit t)
   -> (unit,Format.formatter,unit) Pervasives.format -> Format.formatter
   -> 'a list -> unit t
  (** [fprintfi f d fmt xs] is equivalent to {!val: Util.List.fprintfi}, except
  that its result is encapsulated in a monad. *)
  val to_string : ('a -> string t)
   -> (unit,Format.formatter,unit) Pervasives.format -> 'a list -> string t
  (** [to_string f d xs] is equivalent to {!val: Util.List.to_string}, except
  that its result is encapsulated in a monad. *)
  val to_stringi : (int -> 'a -> string t)
   -> (unit,Format.formatter,unit) Pervasives.format -> 'a list -> string t
  (** [to_stringi f d xs] is equivalent to {!val: Util.List.to_stringi}, except
  that its result is encapsulated in a monad. *)
 end
 
 module type ERROR_MONAD = sig
  (*** TYPES ******************************************************************)
  type error
 
  (*** INCLUDES ***************************************************************)
  include SIGNATURE

  (*** VALUES *****************************************************************)
  (** {3 Access Functions} *)

  val ap_error : ((error,'a) either -> (error,'b) either) t -> 'a t -> 'b t
  (** [ap_error m n] applies the argument of [m] to [n]. *)
  val map_error : ((error,'a) either -> (error,'b) either) -> 'a t -> 'b t
  (** [map_error f m] promotes the function [f] to [m] where [m] has the
  type ['a t]. In order to obtain map functions with a higher arity use
  {!val: Util.Monad.ERROR_MONAD.ap_error}. *)

  (** {3 Error Handling} *)

  val catch :  (error -> 'a t) -> 'a t -> 'a t
  (** [catch h m] is equivalent to
  {!val: Util.Monad.Transformer.ERROR_MONAD.catch}. *)
  val fail : error -> 'a t
  (** [fail e] is equivalent to
  {!val: Util.Monad.Transformer.ERROR_MONAD.fail}. *)
  val failif : bool -> error -> unit t
  (** [failif b e] is equivalent to
  {!val: Util.Monad.Transformer.ERROR_MONAD.failif}. *)
 
  (** {3 Evaluation Functions} *)

  val run : 'a t -> (error,'a) either
  (** [run m] evaluates [m] and returns the calculated result. *)
 end
 
 module type STATE_MONAD = sig
  (*** TYPES ******************************************************************)
  type state
 
  (*** INCLUDES ***************************************************************)
  include SIGNATURE
 
  (*** VALUES *****************************************************************)
  (** {3 Access Functions} *)

  val ap_state : ('a * state -> 'b * state) t -> 'a t -> 'b t
  (** [ap_state m n] applies the argument of [m] to the return value of [n]. *)
  val map_state : ('a * state -> 'b * state) -> 'a t -> 'b t
  (** [map_state f m] promotes the function [f] to the return value of [m]
  where [m] has the type ['a t]. In order to obtain map functions with a
  higher arity use {!val: Util.Monad.STATE_MONAD.ap_state}. *)

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
  (** [set s] is equivalent to
  {!val: Util.Monad.Transformer.STATE_MONAD.set}. *)
  val update : (state -> state) -> unit t
  (** [update s] is equivalent to
  {!val: Util.Monad.Transformer.STATE_MONAD.update}. *)
  val with_state : (state -> state) -> 'a t -> 'a t
  (** [with_state f m] is equivalent to
  {!val: Util.Monad.Transformer.STATE_MONAD.with_state}. *)
 
  (** {3 Evaluation Functions} *)
 
  val eval : state -> 'a t -> 'a * state
  (** [eval s m] evaluates [m] under state [s] and returns the resulting
  value and state. *)
  val execute : state -> 'a t -> state
  (** [execute s m] evaluates [m] under state [s] and returns the resulting
  state. *)
  val run : state -> 'a t -> 'a
  (** [run s m] evaluates [m] under state [s] and returns the resulting
  value. *)
 end
 
 (*** MODULES *****************************************************************)
 module Error : ERROR_MONAD with type error = string
 (** This module defines an error monad with error type [string]. *)
 module LazyList : SIGNATURE with type 'a t = 'a LazyList.t
 (** This module defines the lazy list monad. *)
 module List : SIGNATURE with type 'a t = 'a list
 (** This module defines the list monad. *)
 module Option : SIGNATURE with type 'a t = 'a option
 (** This module defines the option monad. *)
 module Make (M : MINIMAL_MONAD) : SIGNATURE with type 'a t = 'a M.t
 (** Functor building a monad based on a module [M] of module type
 {!modtype: Util.Monad.MINIMAL_MONAD} containing all the basic functions
 needed to be a monad. *)
 module State (S : STATE) : STATE_MONAD with type state = S.t
 (** Functor building a state monad based on a module [S] of module
 type {!modtype: Util.Monad.STATE} implementing a custom state. *)
 
 module Transformer : sig
  (*** MODULE TYPES ***********************************************************)
  module type COMBINED_MONAD = sig
   type 'a m
 
   include SIGNATURE
  
   val liftm : 'a m -> 'a t
   (** [liftm m] lifts monad [m] to an instance of type [t]. *)
  end
  
  module type ERROR_MONAD = sig
   type error
  
   include COMBINED_MONAD
  
   (** {3 Access Functions} *)

   val ap_error : ((error,'a) either m -> (error,'b) either m) t -> 'a t -> 'b t
   (** [ap_error m n] applies the argument of [m] to [n]. *)
   val map_error : ((error,'a) either m -> (error,'b) either m) -> 'a t -> 'b t
   (** [map_error f m] promotes the function [f] to [m] where [m] has the
   type ['a t]. In order to obtain map functions with a higher arity use
   {!val: Util.Monad.Transformer.ERROR_MONAD.ap_error}. *)
 
   (** {3 Error Handling} *)

   val catch : (error -> 'a t) -> 'a t -> 'a t
   (** [catch h m] if [m] is successful its value is promoted, otherwise the
   handler [h] is called. *)
   val fail : error -> 'a t
   (** [fail e] returns an instance of type [t] representing the error [e]. *)
   val failif : bool -> error -> unit t
   (** [failif b e] fails with error [e] if [b] is [true]. *)

   (** {3 Evaluation Functions} *)
 
   val run : 'a t -> (error,'a) either m
   (** [run m] evaluates [m] and returns the resulting value. *)
  end
  
  module type LIST_MONAD = sig
   include COMBINED_MONAD
  
   (** {3 Access Functions} *)

   val ap_list : ('a list m -> 'b list m) t -> 'a t -> 'b t
   (** [ap_list m n] applies the argument of [m] to [n]. *)
   val map_list : ('a list m -> 'b list m) -> 'a t -> 'b t
   (** [map_list f m] promotes the function [f] to [m] where [m] has the
   type ['a t]. In order to obtain map functions with a higher arity use
   {!val: Util.Monad.Transformer.LIST_MONAD.ap_list}. *)
 
   (** {3 Evaluation Functions} *)

   val run : 'a t -> 'a list m
   (** [run m] evaluates [m] and returns the resulting value. *)
  end
  
  module type OPTION_MONAD = sig
   include COMBINED_MONAD
  
   (** {3 Access Functions} *)

   val ap_option : ('a option m -> 'b option m) t -> 'a t -> 'b t
   (** [ap_option m n] applies the argument of [m] to [n]. *)
   val map_option : ('a option m -> 'b option m) -> 'a t -> 'b t
   (** [map_option f m] promotes the function [f] to [m] where [m] has the
   type ['a t]. In order to obtain map functions with a higher arity use
   {!val: Util.Monad.Transformer.OPTION_MONAD.ap_option}. *)
 
   (** {3 Evaluation Functions} *)

   val run : 'a t -> 'a option m
   (** [run m] evaluates [m] and returns the resulting value. *)
  end
 
  module type STATE_MONAD = sig
   type state
  
   include COMBINED_MONAD

   (** {3 Access Functions} *)

   val ap_state : (('a * state) m -> ('b * state) m) t -> 'a t -> 'b t
   (** [ap_state m n] applies the argument of [m] to the return value of
   [n]. *)
   val map_state : (('a * state) m -> ('b * state) m) -> 'a t -> 'b t
   (** [map_state f m] promotes the function [f] to the return value of [m]
   where [m] has the type ['a t]. In order to obtain map functions with a
   higher arity use {!val: Util.Monad.Transformer.STATE_MONAD.ap_state}. *)
 
   (** {3 State Modifications} *)
  
   val adopt : (state -> 'a * state) -> 'a t
   (** [adopt f] applies the function [f] to the current state and returns its
   result. In addition, the current state is replaced by the state returned by
   the function [f]. *)
   val get : state t
   (** [get] returns the current state as value. *) 
   val modify : (state -> state) -> state t
   (** [modify f] modifies the current state via the function [f] and returns
   the new state as result. It is equivalent to [update f >> get]. *)
   val set : state -> unit t
   (** [set s] replaces the current state by [s]. *)
   val update : (state -> state) -> unit t
   (** [update f] updates the current state by applying [f] to it. *)
   val with_state : (state -> state) -> 'a t -> 'a t
   (** [with_state f m] modifies the state of [m] via the function [f]. *)

   (** {3 Evaluation Functions} *)
  
   val eval : state -> 'a t -> ('a * state) m
   (** [eval s m] evaluates [m] under state [s] and returns the resulting
   value and state encapsulated in a monad of type [m]. *)
   val execute : state -> 'a t -> state m
   (** [execute s m] evaluates [m] under [s] and returns the calculated
   state encapsulated in a monad of type [m]. *)
   val run : state -> 'a t -> 'a m
   (** [run s m] evaluates [m] under [s] and returns the calculated
   value encapsulated in a monad of type [m]. *)
  end
 
  (*** MODULES ****************************************************************)
  module Error (E : ERROR) (M : MINIMAL_MONAD) : ERROR_MONAD with
   type error = E.t and type 'a m = 'a M.t and type 'a t = (E.t,'a) either M.t
  (** Functor building an error monad based on a module [E] of module type
  {!modtype: Util.Monad.ERROR} implementing a custom error type and a module
  [M] of module type {!modtype: Util.Monad.MINIMAL_MONAD}. The type of the
  monad is ['a t = (error,'a) either m]. *)
  module List (M : MINIMAL_MONAD) : LIST_MONAD with
   type 'a m = 'a M.t and type 'a t = 'a list M.t
  (** Functor building a list monad based on a module [M] of module type
  {!modtype: Util.Monad.MINIMAL_MONAD}. The type of the returned monad is
  ['a t = 'a list m]. *)
  module Option (M : MINIMAL_MONAD) : OPTION_MONAD with
   type 'a m = 'a M.t and type 'a t = 'a option M.t
  (** Functor building a option monad based on a module [M] of module type
  {!modtype: Util.Monad.MINIMAL_MONAD}. The type of the returned monad is
  ['a t = 'a option m]. *)
  module State (S : STATE) (M : MINIMAL_MONAD) : STATE_MONAD with
   type state = S.t and type 'a m = 'a M.t and type 'a t = S.t -> ('a * S.t) M.t
  (** Functor building a state monad based on a module [S] of module type
  {!modtype: Util.Monad.STATE} implementing a custom state and a module [M]
  of module type {!modtype: Util.Monad.MINIMAL_MONAD}. The type of the monad
  is ['a t = S.t -> ('a * S.t) m]. *)
 end
end

(** {2 Module Option} *)

(** Options
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Tue Oct 28 11:54:35 CET 2008 *)

(** This module provides some useful function for the option type. *)
module Option : sig
 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val some : 'a -> 'a option 
 (** [some x] returns [Some x]. *)
 val the : 'a option -> 'a
 (** [the o] return [x] if [o] is [Some x].
 @raise Failure "no content" if [o] is [None]. *)

 (** {3 Iterators} *)

 val fold : ('a -> 'b) -> 'b -> 'a option -> 'b
 (** [fold f d o] returns [f x] if [o] is [Some x] and [d] otherwise. *)
 val map : ('a -> 'b) -> 'a option -> 'b option
 (** [map f o] returns [Some (f x)] if [o] is [Some x]. Otherwise [None] is
 returned. *)
 val option : ('a -> 'b) -> 'b -> 'a option -> 'b
 (** [option f d o] returns [fold f d o]. *)

 (** {3 Properties} *)

 val is_none : 'a option -> bool
 (** [is_none o] checks if [o = None]. *)
 val is_some : 'a option -> bool
 (** [is_some o] is equivalent to [not (is_none o)]. *)

 (** {3 Compare Functions} *)

 val compare : ('a -> 'a -> int) -> 'a option -> 'a option -> int
 (** [compare f o o'] compares [o] and [o'] using the function [f]. *)
 val equal : ('a -> 'a -> int) -> 'a option -> 'a option -> bool
 (** [equal f o o'] checks if [o] and [o'] are equal using the compare
 function [f]. Note that this function is equivalent to [compare s t = 0]. *)

 (** {3 Printers} *)

 val fprintf : (Format.formatter -> 'a -> unit) -> Format.formatter
  -> 'a option -> unit
 (** [fprintf f fmt o] writes pretty printed output of [o]. *)
 val to_string : ('a -> string) -> 'a option -> string
 (** [to_string f o] transforms [o] into a formatted string. *)
end

(** {2 Module Pair} *)

(** Pairs
@author Christian Sternagel
@since  Wed Dec  3 17:01:06 CET 2008 *)

(** Special functions for pairs. *)
module Pair : sig
 (*** MODULE TYPES ************************************************************)
 module type PAIR = sig
  val fst : 'a * 'b -> 'a
  val snd : 'a * 'b -> 'b
 end
 (** This module type contains all standard [OCaml] functions that can deal
 with pairs. *)
 
 (*** INCLUDES ****************************************************************)
 include PAIR
 
 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val assocr : ('a * 'b) * 'c -> 'a * ('b * 'c)
 (** [assocr ((x,y),z)] returns [(x,(y,z))]. *)
 val assocl : 'a * ('b * 'c) -> ('a * 'b) * 'c
 (** [assocl (x,(y,z))] returns [((x,y),z)]. *)
 val create : 'a -> 'a * 'a
 (** [create x] is equivalent to [make x x]. *)
 val flip : 'a * 'b -> 'b * 'a
 (** [flip (x,y)] returns [(y,x)]. *)
 val make : 'a -> 'b -> 'a * 'b
 (** [make x y] returns [(x,y)]. *)
 val pair : ('a -> 'b) * ('a -> 'c) -> 'a -> 'b * 'c
 (** [pair (f,g) x] returns [(f x,g x)]. *)
 val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
 (** [uncurry f (x,y)] returns [f x y]. *)

 (** {3 Iterators} *)

 val apply : ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd
 (** [apply f g (x,y)] returns the result [(f x,g y)]. *)
 val cross : ('a -> 'c) * ('b -> 'd) -> 'a * 'b -> 'c * 'd
 (** [cross (f,g) (x,y)] returns the result [(f x,g y)]. *)
 val fold : ('a -> 'c -> 'c) -> ('b -> 'c -> 'c) -> 'c -> 'a * 'b -> 'c
 (** [fold f g d (x,y)] computes the result [g y (f x d)]. *)
 val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
 (** [map f (x,y)] returns [(f x,f y)]. *)

 (** {3 Scan Functions} *)

 val for_all : ('a -> bool) -> ('b -> bool) -> 'a * 'b -> bool
 (** [for_all f g (x,y)] checks if [f x] and [g y] are evaluated to [true]. *)
 val exists : ('a -> bool) -> ('b -> bool) -> 'a * 'b -> bool
 (** [exists f g (x,y)] checks if [f x] or [g y] are evaluated to [true]. *)

 (** {3 Printers} *)

 val fprintf : (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit) -> Format.formatter -> 'a * 'b -> unit
 (** [fprintf f g fmt p] writes pretty printed output of [p]. *)
 val to_string : ('a -> string) -> ('b -> string) -> 'a * 'b -> string
 (** [to_string f g p] transforms [p] into a formatted string. *)
end

(** {2 Module Process} *)

(** Process Handling
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Tue Oct 28 11:54:35 CET 2008 *)

(** This module provides functions which allow the usage of timers and parallel
function executions. In order to use this functions without any failures, make
sure that the used return values are transferable using the [OCaml] functions
[Marshal.to_channel] and [Marshal.from_channel]. *)
module Process : sig
 (*** TYPES *******************************************************************)
 type time =
  | Global of float
  (** [Global] is used to define a point since 00:00:00 GMT, January 1, 1970,
  measured in seconds. *)
  | Local of float
  (** [Local] is used to define a certain time period, measured in seconds. *)
 (** This type is used to define time points and time periods. *)

 (*** VALUES ******************************************************************)
 (** {3 Parallel Evaluations} *)

 val run_parallel : ('a -> 'b) list -> 'a -> 'b option list
 (** [run_parallel fs i] runs the functions [fs] in parallel on input [i]. After
 all functions have been terminated the results are returned as an option list.
 The results are ordered in the same way as the functions [fs]. That means, if
 [f] is the i-th function then the result of this function is the i-th value of
 the returned list. Note that if a function [f] raises an exception, [None] is
 returned instead of [Some (f i)]. Not tail-recursive. *)
 val run_parallel_until : ('b -> bool) -> ('a -> 'b) list -> 'a -> 'b option
 (** [run_parallel_until p fs i] runs the functions [fs] in parallel on input
 [i]. The first result returned by some function [f] in [fs] that satisfies the
 predicate [p] is returned. If none of the results satisfies [p], [None] is
 returned. *)
 
 (** {3 Timed Evaluations} *)

 val run_timed : time -> ('a -> 'b) -> 'a -> 'b option * float
 (** [run_timed t f i] runs the function [f] for a certain time. If [t] is
 of type [Global t] then [f] is executed up to time [t]. Otherwise [f] is
 executed for [t] seconds. If [f] terminates within the given time slot then
 the result [f i] together with the remaining time is returned. Otherwise
 [None] and the remaining time is returned. *)
end

(** {2 Module Process2} *)

(** Extended Process Handling
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Mon Jun 14 15:08:52 CEST 2010 *)

(** This module is experimental *)
module Process2 : sig
 (*** TYPES *******************************************************************)
 (*** VALUES ******************************************************************)
 (** {3 Parallel Evaluations} *)

 (**)
 val parallel : float -> int -> (unit -> 'a) list -> ('a option) list
 (** experimental: [parallel t n fs] runs [n] functions from [fs] in
 parallel in total for at most [t] seconds *)
 (**)
end

(** {2 Module Quadruple} *)

(** Quadruples
@author Martin Korp
@since  Wed Dec  3 17:01:06 CET 2008 *)

(** Special functions for quadruples. *)
module Quadruple : sig
 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val create : 'a -> 'a * 'a * 'a * 'a
 (** [create x] is equivalent to [make x x x x]. *)
 val drop_fst : 'a * 'b * 'c * 'd -> 'b * 'c * 'd
 (** [drop_fst (w,x,y,z)] returns [(x,y,z)]. *)
 val drop_fth : 'a * 'b * 'c * 'd -> 'a * 'b * 'c
 (** [drop_fth (w,x,y,z)] returns [(w,x,y)]. *)
 val drop_snd : 'a * 'b * 'c * 'd -> 'a * 'c * 'd
 (** [drop_snd (w,x,y,z)] returns [(w,y,z)]. *)
 val drop_thd : 'a * 'b * 'c * 'd -> 'a * 'b * 'd
 (** [drop_thd (w,x,y,z)] returns [(w,x,z)]. *)
 val flip : 'a * 'b * 'c * 'd -> 'd * 'c * 'b * 'a
 (** [flip (w,x,y,z)] returns [(z,y,x,w)]. *)
 val fst : 'a * 'b * 'c * 'd -> 'a
 (** [fst (w,x,y,z)] returns [w]. *)
 val fth : 'a * 'b * 'c * 'd -> 'd
 (** [fth (w,x,y,z)] returns [z]. *)
 val insert_fst : 'a -> 'b * 'c * 'd -> 'a * 'b * 'c * 'd
 (** [insert_fst w (x,y,z)] returns [(w,x,y,z)]. *)
 val insert_fth : 'd -> 'a * 'b * 'c -> 'a * 'b * 'c * 'd
 (** [insert_fth z (w,x,y)] returns [(w,x,y,z)]. *)
 val insert_snd : 'b -> 'a * 'c * 'd -> 'a * 'b * 'c * 'd
 (** [insert_snd x (w,y,z)] returns [(w,x,y,z)]. *)
 val insert_thd : 'c -> 'a * 'b * 'd -> 'a * 'b * 'c * 'd
 (** [insert_thd y (w,x,z)] returns [(w,x,y,z)]. *)
 val make : 'a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd
 (** [make w x y z] returns [(w,x,y,z)]. *)
 val replace_fst : 'e -> 'a * 'b * 'c * 'd -> 'e * 'b * 'c * 'd
 (** [replace_fst w (_,x,y,z)] returns [(w,x,y,z)]. *)
 val replace_fth : 'e -> 'a * 'b * 'c * 'd -> 'a * 'b * 'c * 'e
 (** [replace_fth z (w,x,y,_)] returns [(w,x,y,z)]. *)
 val replace_snd : 'e -> 'a * 'b * 'c * 'd -> 'a * 'e * 'c * 'd
 (** [replace_snd x (w,_,y,z)] returns [(w,x,y,z)]. *)
 val replace_thd : 'e -> 'a * 'b * 'c * 'd -> 'a * 'b * 'e * 'd
 (** [replace_thd y (w,x,_,z)] returns [(w,x,y,z)]. *)
 val snd : 'a * 'b * 'c * 'd -> 'b
 (** [snd (w,x,y,z)] returns [x]. *)
 val thd : 'a * 'b * 'c * 'd -> 'c
 (** [thd (w,x,y,z)] returns [y]. *)
 val quadruple : ('e -> 'a) * ('e -> 'b) * ('e -> 'c) * ('e -> 'd) -> 'e
  -> 'a * 'b * 'c * 'd
 (** [quadruple (f,g,h,i) x] returns [(f x,g x,h x,i x)]. *)
 val uncurry : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a * 'b * 'c * 'd -> 'e
 (** [uncurry f (w,x,y,z)] returns [f w x y z]. *)

 (** {3 Iterators} *)

 val apply : ('a -> 'e) -> ('b -> 'f) -> ('c -> 'g) -> ('d -> 'h)
  -> 'a * 'b * 'c * 'd -> 'e * 'f * 'g * 'h
 (** [apply f g h i (w,x,y,z)] returns the result [(f w,g x,h y,i z)]. *)
 val cross : ('a -> 'e) * ('b -> 'f) * ('c -> 'g) * ('d -> 'h)
  -> 'a * 'b * 'c * 'd -> 'e * 'f * 'g * 'h
 (** [cross (f,g,h,i) (w,x,y,z)] returns the result [(f w,g x,h y,i z)]. *)
 val fold : ('a -> 'e -> 'e) -> ('b -> 'e -> 'e) -> ('c -> 'e -> 'e)
  -> ('d -> 'e -> 'e) -> 'e -> 'a * 'b * 'c * 'd -> 'e
 (** [fold f g h i d (w,x,y,z)] computes the result
 [i z (h y (g x (f w d)))]. *)
 val map : ('a -> 'b) -> 'a * 'a * 'a * 'a -> 'b * 'b * 'b * 'b
 (** [map f (w,x,y,z)] returns [(f w,f x,f y,f z)]. *)

 (** {3 Scan Functions} *)

 val for_all : ('a -> bool) -> ('b -> bool) -> ('c -> bool) -> ('d -> bool)
  -> 'a * 'b * 'c * 'd -> bool
 (** [for_all f g h i (w,x,y,z)] checks if [f w], [g x], [h y], and [i z] are
 evaluated to [true]. *)
 val exists : ('a -> bool) -> ('b -> bool) -> ('c -> bool) -> ('d -> bool)
  -> 'a * 'b * 'c * 'd -> bool
 (** [exists f g h i (w,x,y,z)] checks if [f w], [g x], [h y], or [i z] are
 evaluated to [true]. *)

 (** {3 Printers} *)

 val fprintf : (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit) -> (Format.formatter -> 'c -> unit)
  -> (Format.formatter -> 'd -> unit) -> Format.formatter -> 'a * 'b * 'c * 'd
  -> unit
 (** [fprintf f g h i fmt p] writes pretty printed output of [p]. *)
 val to_string : ('a -> string) -> ('b -> string) -> ('c -> string)
  -> ('d -> string) -> 'a * 'b * 'c * 'd -> string
 (** [to_string f g h p] transforms [p] into a formatted string. *)
end


(** {2 Module Replacement} *)

(** Replacement Map
@author Martin Korp
@since  Sat Nov 29 11:16:35 CEST 2008 *)

(** This module defines a replacement map based on a given range and domain. *)
module Replacement : sig
 (*** MODULE TYPES ************************************************************)
 module type DOMAIN = sig
  type t
 
  val compare : t -> t -> int
  (** [compare s t] compares [s] and [t]. It must be guaranteed that
  [compare s t = 0] if [s] and [t] are equal, [compare s t < 0] if [s]
  is smaller than [t], and [compare s t > 0] if [s] is greater than [t]. *)
  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt t] prints [t] using the [OCaml] module [Format]. *)
 end
 
 module type RANGE = DOMAIN
 
 module type SIGNATURE = sig
  (*** TYPES ******************************************************************)
  type domain
  type range
  type t
  
  (*** EXCEPTIONS *************************************************************)
  exception Inconsistent

  (*** VALUES *****************************************************************)
  (** {3 Constructors} *)

  val add : domain -> range -> t -> t
  (** [add k e r] returns a replacement map containing the same bindings as [r]
  plus a binding of [k] to [e].
  @raise Inconsistent if [r] contains already a binding of [k] to [d] where
  [d] is different from [e]. *)
  val copy : t -> t
  (** [copy] is equivalent to {!val: Util.Map.SIGNATURE.copy}. *)
  val empty : t
  (** [empty] is equivalent to {!val: Util.Map.SIGNATURE.empty}. *)
  val of_list : (domain * range) list -> t
  (** [of_list xs] generates a replacement map of list [xs].
  @raise Inconsistent if [xs] contains a pair [(k,e)] and a pair [(k,d)]
  where [d] is different from [e]. *)
  val remove : domain -> t -> t
  (** [remove k r] is equivalent to {!val: Util.Map.SIGNATURE.remove}. *)
  val replace : domain -> range -> t -> t
  (** [replace k e r] returns a replacement map containing the same bindings
  as [r] plus a binding of [k] to [e]. If [r] contains already a binding of
  [k] to [d] it is replaced. *)
  val singleton : domain -> range -> t
  (** [singleton k e] is equivalent to {!val: Util.Map.SIGNATURE.singleton}. *)
  val to_list : t -> (domain * range) list
  (** [to_list r] is equivalent to {!val: Util.Map.SIGNATURE.to_list}. *)

  (** {3 Search Functions} *)

  val apply : domain -> range -> t -> range
  (** [apply k d r] returns the element associated with key [k] in [r] if such
  a binding exists and the default value [d] otherwise. *)
  val find : domain -> t -> range
  (** [find k r] is equivalent to {!val: Util.Map.SIGNATURE.find}.
  @raise Not_found if [r] does not contain a binding for [k]. *)
  val search: (domain -> range -> bool) -> t -> domain * range
  (** [search p r] is equivalent to {!val: Util.Map.SIGNATURE.search}. *)

  (** {3 Iterators} *)

  val fold : (domain -> range -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f r d] is equivalent to {!val: Util.Map.SIGNATURE.fold}. *)
  val foldi : (int -> domain -> range -> 'a -> 'a) -> t -> 'a -> 'a
  (** [foldi f r d] is equivalent to {!val: Util.Map.SIGNATURE.foldi}. *)
  val iter : (domain -> range -> unit) -> t -> unit
  (** [iter f r] is equivalent to {!val: Util.Map.SIGNATURE.iter}. *)
  val iteri : (int -> domain -> range -> unit) -> t -> unit
  (** [iteri f r] is equivalent to {!val: Util.Map.SIGNATURE.iteri}. *)
  val map : (range -> range) -> t -> t
  (** [map f r] is equivalent to {!val: Util.Map.SIGNATURE.map}. *)
  val mapi : (domain -> range -> range) -> t -> t
  (** [mapi f r] is equivalent to {!val: Util.Map.SIGNATURE.mapi}. *)

  (** {3 Scan Functions} *)

  val exists : (domain -> range -> bool) -> t -> bool
  (** [exists p r] is equivalent to {!val: Util.Map.SIGNATURE.exists}. *)
  val for_all : (domain -> range -> bool) -> t -> bool
  (** [for_all p r] is equivalent to {!val: Util.Map.SIGNATURE.for_all}. *)
  val mem : domain -> t -> bool
  (** [mem k r] is equivalent to {!val: Util.Map.SIGNATURE.mem}. *)

  (** {3 Properties} *)

  val is_bijective : t -> bool
  (** [is_bijective r] checks whether [r] is bijective. Note that [r] is
  bijective whenever [r] is injective because every [r] is surjective by
  definition. *)
  val is_empty : t -> bool
  (** [is_empty r] is equivalent to {!val: Util.Map.SIGNATURE.is_empty}. *)
  val is_injective : t -> bool
  (** [is_injective r] tests whether [r] is injective. *)

  (** {3 Miscellaneous} *)

  val compose : (t -> range -> range) -> t -> t -> t
  (** [compose f r r'] composes two replacement maps [r] and [r'] using the
  function [f] to apply [r'] to bindings in [r]. *)
  val domain : t -> domain list
  (** [domain r] is equivalent to {!val: Util.Map.SIGNATURE.domain}. *)
  val power : int -> (t -> range -> range) -> t -> t
  (** [power n f r] computes the [n]-th power of the replacement map [r] using
  the function [f] to apply [r] to its bindings. *)
  val range : t -> range list
  (** [range r] is equivalent to {!val: Util.Map.SIGNATURE.range}. *)
  val size : t -> int
  (** [size r] is equivalent to {!val: Util.Map.SIGNATURE.size}. *)
  val union : t -> t -> t
  (** [union r r'] computes the union of [r] and [r'].
  @raise Inconsistent if [r] contains a binding of [k] to [e] and [r'] contains
  a binding of [k] to [d] where [d] is different from [e]. *)

  (** {3 Compare Functions} *)

  val compare : t -> t -> int
  (** [compare r s] is equivalent to {!val: Util.Map.SIGNATURE.compare}. *)
  val equal : t -> t -> bool
  (** [equal r s] is equivalent to {!val: Util.Map.SIGNATURE.equal}. *)

  (** {3 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt r] is equivalent to {!val: Util.Map.SIGNATURE.fprintf}. *)
  val to_string : t -> string
  (** [to_string r] is equivalent to {!val: Util.Map.SIGNATURE.to_string}. *)
 end

 (*** MODULES *****************************************************************)
 module Make (D : DOMAIN) (R : RANGE) : SIGNATURE with
  type domain = D.t and type range = R.t
 (** Functor building a replacement map. The elements of the range are of
 module type {!modtype: Util.Replacement.RANGE} whereas the elements of the
 domain are of module type {!modtype: Util.Replacement.DOMAIN}. *)
end

(** {2 Module String} *)

(** Extended String
@author Martin Korp, Christian Sternagel 
@since  Tue Sep  2 13:52:45 CEST 2008 *)

(** This is an extension of the standard [OCaml] module [String]. *)
module String : sig
 (*** TYPES *******************************************************************)
 type t = string
 
 (*** MODULE TYPES ************************************************************)
 module type STRING = sig
  val blit : t -> int -> t -> int -> int -> unit
  val capitalize : t -> t
  val compare: t -> t -> int
  val concat : t -> t list -> t
  val contains : t -> char -> bool
  val contains_from : t -> int -> char -> bool
  val copy : t -> t
  val create : int -> t
  val escaped : t -> t
  val fill : t -> int -> int -> char -> unit
  val get : t -> int -> char
  val index : t -> char -> int
  val index_from : t -> int -> char -> int
  val iter : (char -> unit) -> t -> unit
  val length : t -> int
  val lowercase : t -> t
  val make : int -> char -> t
  val rcontains_from : t -> int -> char -> bool
  val rindex : t -> char -> int
  val rindex_from : t -> int -> char -> int
  val set : t -> int -> char -> unit
  val sub : t -> int -> int -> t
  val uppercase : t -> t
  val uncapitalize : t -> t
 end
 (** This module type contains all standard [OCaml] functions provided by the
 module [String]. *)

 (*** INCLUDES ****************************************************************)
 include STRING
 
 (*** VALUES ******************************************************************)
 (** {3 Constructors} *)

 val align : ?n:int -> int -> t -> t
 (** [align ~n:n i s] takes the string [s] and joins its words to a new string,
 adding a newline every time the resulting string would be longer than [i].
 In addition, after each newline [n] white spaces are added. Per default
 [n = 0]. *)
 val alignx : ?n:int -> int -> int -> t -> t
 (** [alignx ~n:n i j s] is equivalent to {!val: Util.String.align} except that
 the first line has at most length [i]. Afterwards a newline is added every
 time the resulting string would be longer than [j]. *)
 val chop : t -> t
 (** [chop s] returns a version of string [s] where heading and trailing
 white spaces are removed. *)
 val itemize : ?n:int -> ?s:string -> int -> t list -> t
 (** [itemize ~n:n ~s:s i is] takes a list of items [is] and formats them
 nicely. The argument [s] defines the string that is put in front of each
 item (per default ['-']).  After each item a newline and [n] white spaces
 are added. Per default [n = 0]. Furthermore, if some text has more than [i]
 symbols, it is split using the function {!val: Util.String.align}. *)
 val itemizex : ?n:int -> int -> (t * t) list -> t
 (** [itemize ~n:n i is] behaves similar to {!val: Util.String.itemize} except
 that the string that is put in front of each item is stored in [is]. I.e.,
 [is] consists of pairs of strings where the first string defines the headline
 and the second string the printed text. After each item a newline and [n]
 white spaces are added. Per default [n = 0]. Furthermore, if some text has
 more than [i] symbols, it is split using the function
 {!val: Util.String.align}. *)
 val shift_left : int -> t -> t
 (** [shift_left i s] adds [i] spaces to the end of [s]. *)
 val shift_right : int -> t -> t
 (** [shift_right i s] adds [i] spaces to the beginning of [s]. *)

 (** {3 Properties} *)

 val is_prefix : t -> t -> bool
 (** [is_prefix s t] returns [true] if and only if [s] is a prefix of [t],
 i.e., [t = s^u] for some string [u]. *)

 (** {3 Character Lists} *)

 val of_char_list : char list -> t 
 (** [of_char_list cs] transforms [cs] into a string. *)
 val to_char_list : t -> char list
 (** [to_char_list s] returns the characters of [s] as a list. *)
 val to_rev_char_list : t -> char list
 (** [to_rev_char_list s] returns the characters of [s] in reversed
 order. Note that this function is more efficient than its pendant
 [List.rev (to_char_list s)]. *)

 (** {3 Search Functions} *)

 val char_pos : char -> t -> int list
 (** [char_pos c s] returns all positions of [s] at which the character [c]
 can be found. *)
 val last : t -> char
 (** [last s] returns the last character of string [s].
 @raise Not_found if [s] is empty. *)

 (** {3 Miscellaneous} *)

 val hash : t -> int
 (** [hash s] returns a hash value for [s]. It is guaranteed that if
 [compare s t = 0] then [hash s = hash t]. *)
 val split : ?d:t -> t -> t list
 (** [split ~d:d s] returns the list of words in [s], where words are separated
 by the pattern in string [d]. By default words are delimited by white spaces
 (i.e. [' '], ['\n'], ['\t'], and ['\r']). See the [OCaml] module [Str] for the
 format of the pattern. *)
 val trim : t -> t
 (** [trim s] returns [s] where blanks have been dropped. *)

 (** {3 Compare Functions} *)

 val equal : t -> t -> bool
 (** [equal s t] checks if [s] and [t] are equal. Note that this function is
 equivalent to [compare s t = 0]. *)

 (** {3 Printers} *)

 val fprintf : Format.formatter -> t -> unit
 (** [fprintf fmt s] prints the string [s] using the [OCaml] module [Format]. *)
 val to_string : t -> string
 (** [to_string s] transforms [s] into a formatted string. *)


 (** {3 XML} *)

 val xml_entities_encode : t -> t
 (** [escape_xml s] returns [s] where [&,>,<] are transformed into [&amp;&lt;&gt]*)
 val xml_entities_decode : t -> t
 (** [descape_xml s] returns [s] where are [&amp;&lt;&gt] escaped into [&,>,<] *)
 
 (** {3 URL} *)
 val url_escape : ?ignore:string list -> t -> t
 (** [url_escape s] escapes all special characters for URLs
     [url_escape ignore s] escapes all special characters for URLs except
     the ones in [ignore] *)
end

(** {2 Module Triple} *)

(** Triples
@author Martin Korp
@since  Wed Dec  3 17:01:06 CET 2008 *)

(** Special functions for triples. *)
module Triple : sig
 (*** VALUES ******************************************************************)
 (** {3 Constructors and Destructors} *)

 val create : 'a -> 'a * 'a * 'a
 (** [create x] is equivalent to [make x x x]. *)
 val drop_fst : 'a * 'b * 'c -> 'b * 'c
 (** [drop_fst (x,y,z)] returns [(y,z)]. *)
 val drop_snd : 'a * 'b * 'c -> 'a * 'c
 (** [drop_snd (x,y,z)] returns [(x,z)]. *)
 val drop_thd : 'a * 'b * 'c -> 'a * 'b
 (** [drop_thd (x,y,z)] returns [(x,y)]. *)
 val flip : 'a * 'b * 'c -> 'c * 'b * 'a
 (** [flip (x,y,z)] returns [(z,y,x)]. *)
 val fst : 'a * 'b * 'c -> 'a
 (** [fst (x,y,z)] returns [x]. *)
 val insert_fst : 'a -> 'b * 'c -> 'a * 'b * 'c
 (** [insert_fst x (y,z)] returns [(x,y,z)]. *)
 val insert_snd : 'b -> 'a * 'c -> 'a * 'b * 'c
 (** [insert_snd y (x,z)] returns [(x,y,z)]. *)
 val insert_thd : 'c -> 'a * 'b -> 'a * 'b * 'c
 (** [insert_thd z (x,y)] returns [(x,y,z)]. *)
 val make : 'a -> 'b -> 'c -> 'a * 'b * 'c
 (** [make x y z] returns [(x,y,z)]. *)
 val replace_fst : 'd -> 'a * 'b * 'c -> 'd * 'b * 'c
 (** [replace_fst x (_,y,z)] returns [(x,y,z)]. *)
 val replace_snd : 'd -> 'a * 'b * 'c -> 'a * 'd * 'c
 (** [replace_snd y (x,_,z)] returns [(x,y,z)]. *)
 val replace_thd : 'd -> 'a * 'b * 'c -> 'a * 'b * 'd
 (** [replace_thd z (x,y,_)] returns [(x,y,z)]. *)
 val snd : 'a * 'b * 'c -> 'b
 (** [snd (x,y,z)] returns [y]. *)
 val thd : 'a * 'b * 'c -> 'c
 (** [thd (x,y,z)] returns [z]. *)
 val triple : ('d -> 'a) * ('d -> 'b) * ('d -> 'c) -> 'd -> 'a * 'b * 'c
 (** [triple (f,g,h) x] returns [(f x,g x,h x)]. *)
 val uncurry : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd
 (** [uncurry f (x,y,z)] returns [f x y z]. *)

 (** {3 Iterators} *)

 val apply : ('a -> 'd) -> ('b -> 'e) -> ('c -> 'f) -> 'a * 'b * 'c
  -> 'd * 'e * 'f
 (** [apply f g h (x,y,z)] returns the result [(f x,g y,h z)]. *)
 val cross : ('a -> 'd) * ('b -> 'e) * ('c -> 'f) -> 'a * 'b * 'c
  -> 'd * 'e * 'f
 (** [cross (f,g,h) (x,y,z)] returns the result [(f x,g y,h z)]. *)
 val fold : ('a -> 'd -> 'd) -> ('b -> 'd -> 'd) -> ('c -> 'd -> 'd) -> 'd
  -> 'a * 'b * 'c -> 'd
 (** [fold f g h d (x,y,z)] computes the result [h z (g y (f x d))]. *)
 val map : ('a -> 'b) -> 'a * 'a * 'a -> 'b * 'b * 'b
 (** [map f (x,y,z)] returns [(f x,f y,f z)]. *)

 (** {3 Scan Functions} *)

 val for_all : ('a -> bool) -> ('b -> bool) -> ('c -> bool) -> 'a * 'b * 'c
  -> bool
 (** [for_all f g h (x,y,z)] checks if [f x], [g y], and [h z] are evaluated
 to [true]. *)
 val exists : ('a -> bool) -> ('b -> bool) -> ('c -> bool) -> 'a * 'b * 'c
  -> bool
 (** [exists f g h (x,y,z)] checks if [f x], [g y], or [h z] are evaluated to
 [true]. *)

 (** {3 Printers} *)

 val fprintf : (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit) -> (Format.formatter -> 'c -> unit)
  -> Format.formatter -> 'a * 'b * 'c -> unit
 (** [fprintf f g h fmt p] writes pretty printed output of [p]. *)
 val to_string : ('a -> string) -> ('b -> string) -> ('c -> string)
  -> 'a * 'b * 'c -> string
 (** [to_string f g h p] transforms [p] into a formatted string. *)
end

(** {2 Module Xsltproc} *)

(** Extended Xsltproc
@author Christian Sternagel 
@since  Thu Sep 10 13:43:17 CEST 2009 *)

(** This module provides an OCaml interface to 'xsltproc'. *)
module Xsltproc : sig
  val translate : ?style:string -> string -> string
  (** [translate ~style:style s] transforms the string [s] according to the
  style [style]. *)
end
