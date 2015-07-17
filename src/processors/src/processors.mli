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

(** Processor Library
@author Martin Korp
@since  Fri Mar  6 14:29:43 CET 2009 *)

(** Provides various processors that can be used to prove termination of
TRSs automatically. In order to integrate new processors follow the
instruction in the file [processors.ml]. *)

(*** MODULES ******************************************************************)
(** {2 Module Rewriting} *)

(** Rewriting Functions
@author Martin Korp
@since  Mon Sep  1 12:22:31 CEST 2009 *)

(** Provides some basic functions that deal with the standard notions of
term rewriting. This module is an extension of the rewriting library.
I.e., it includes special labeling functions needed by the implemented
termination techniques. *)
module Rewritingx : sig
 (*** MODULES *****************************************************************)
 module Function : sig include Rewriting.FUNCTION end
 module Variable : Rewriting.VARIABLE

 module Label : sig
  (*** MODULES ****************************************************************)
  module F : Util.Index.ISOMORPHIC with
   type key = Function.t and type element = string
 
  module V : Util.Index.ISOMORPHIC with
   type key = Variable.t and type element = string

  (*** TYPES ******************************************************************)
  type signature = F.t * V.t
  type theory = A | C | AC
  type t
  
  (*** VALUES *****************************************************************)
  (** {3 Constructors and Destructors} *)

  val add_rlab : Function.t -> t -> t
  (** [add_rlab f l] adds [f] to the label [l] if [l] is a root label.
  @raise Failure "unknown label" if [l] is not a root label. *)
  val incr_curry : t -> t
  (** [incr_curry l] increments the value of [l] if [l] is a curry label.
  @raise Failure "unknown label" if [l] is not a curry label. *)
  val make_curry : int -> t
  (** [make_curry n] creates a curry label with value [n]. *)
  val make_dp : t
  (** [make_dp] creates a dp label. *)
  val make_height : int -> t
  (** [make_height n] creates a height label with value [n]. *)
  val make_rlab : Function.t list -> t
  (** [make_rlab fs] creates a root label with value [fs]. *)
  val make_slab : int -> t
  (** [make_slab n] creates a semantic label with value [n]. *)
  val make_theory : theory -> t
  (** [make_theory t] creates a theory label. *)

  (** {3 Search Functions} *)

  val get_curry : t -> int
  (** [get_curry l] returns the value of [l] if [l] is a curry label.
  @raise Failure "unknown label" if [l] is not a curry label. *)
  val get_height : t -> int
  (** [get_height l] returns the value of [l] if [l] is a height label.
  @raise Failure "unknown label" if [l] is not a height label. *)
  val get_rlab : t -> Function.t list
  (** [get_rlab l] returns the value of [l] if [l] is a root label.
  @raise Failure "unknown label" if [l] is not a root label. *)
  val get_slab : t -> int
  (** [get_slab l] returns the value of [l] if [l] is a semantic label.
  @raise Failure "unknown label" if [l] is not a semantic label. *)
  val get_theory : t -> theory
  (** [get_theory l] returns the theory label of [l].
  @raise Failure "unknown label" if [l] is not a theory label. *)

  (** {3 Properties} *)

  val is_curry : t -> bool
  (** [is_curry l] checks if [l] is a curry label. *)
  val is_dp : t -> bool
  (** [is_dp l] checks if [l] is a dp label. *)
  val is_height : t -> bool
  (** [is_height l] checks if [l] is a height label. *)
  val is_rlab : t -> bool
  (** [is_rlab l] checks if [l] is a root label. *)
  val is_slab : t -> bool
  (** [is_slab l] checks if [l] is a semantic label. *)
  val is_theory : theory -> t -> bool
  (** [is_theory l] checks if [l] is a [theory] label. *)
  val is_some_theory : t -> bool
  (** [is_some_theory l] checks if [l] is a theory label. *)

  (** {3 Miscellaneous} *)

  val copy : t -> t
  (** [copy l] returns a copy of [l]. *)
  val hash : t -> int
  (** [hash l] returns the hash value of [l]. *)

  (** {3 Compare Functions} *)

  val compare : t -> t -> int
  (** [compare l l'] compares the labels [l] and [l']. This function defines a
  total ordering on labels. *)
  val equal : t -> t -> bool
  (** [equal l l'] checks if [l] and [l'] are equal. This function is
  equivalent to [compare l l' = 0]. *)

  (** {3 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt l] prints [l] using the OCaml module Format. *)
  val fprintfs : (Format.formatter -> Function.t -> signature -> signature)
   -> (Format.formatter -> Variable.t -> signature -> signature)
   -> Format.formatter -> t -> signature -> signature
  (** [fprintfs f g fmt l s] prints [l] using the [OCaml] module [Format].
  In difference to {!val: Processors.Rewritingx.Label.fprintf}, this function
  uses the information stored in the signature [s] to print function symbols
  and variables. *)
  val fprintfx : (Format.formatter -> Function.t -> signature -> signature)
   -> (Format.formatter -> Variable.t -> signature -> signature)
   -> Format.formatter -> t -> signature -> signature
  (** [fprintfx f g fmt l s] prints [l] in XML using the [OCaml] module
  [Format]. In difference to {!val: Processors.Rewritingx.Label.fprintf},
  this function uses the information stored in the signature [s] to print
  function symbols and variables. *)
  val to_string : t -> string
  (** [to_string l] returns a string that represents [l]. *)
 end

 module Signature : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.STATEX with type label = Label.t

  (*** VALUES *****************************************************************)
  (** {3 Constructors and Destructors} *)
  
  val add_rlab : Function.t -> Function.t -> t -> t
  (** [add_rlab f g s] updates the outermost root label of [g] by adding the
  function [f] to the top of the given list. Note that as a side effect the
  old signature changes all over. 
  @raise Not_found if [g] has no root label. *)
  val drop_curry : Function.t -> t -> Function.t
  (** [drop_curry f s] unlabels [f] if [f] admits a curry label.
  @raise Not_found otherwise. *)
  val drop_dp : Function.t -> t -> Function.t
  (** [drop_dp f s] unlabels [f] if [f] is a DP symbol.
  @raise Not_found otherwise. *)
  val drop_height : Function.t -> t -> Function.t
  (** [drop_height f s] unlabels [f] if [f] admits a height label.
  @raise Not_found otherwise. *)
  val drop_rlab : Function.t -> t -> Function.t
  (** [drop_rlab f s] unlabels [f] if [f] admits a root label.
  @raise Not_found otherwise. *)
  val drop_slab : Function.t -> t -> Function.t
  (** [drop_slab f s] unlabels [f] if [f] admits a semantic label.
  @raise Not_found otherwise. *)
  val drop_theory : Function.t -> t -> Function.t
  (** [drop_theory f s] unlabels [f] if [f] admits a theory label.
  @raise Not_found otherwise. *)
  val incr_curry : Function.t -> t -> t
  (** [incr_curry f s] increments the outermost curry label of [f]. Note that
  as a side effect the old signature changes all over.
  @raise Not_found if [f] has no curry label. *)
  val modify_curry : Function.t -> int -> t -> t
  (** [modify_curry f n s] replaces the outermost curry label by a new curry
  label with value [n]. Note that as a side effect the old signature changes
  all over. 
  @raise Not_found if [f] has no curry label. *)
  val modify_height : Function.t -> int -> t -> t
  (** [modify_height f n s] replaces the outermost height label by a new height
  label with value [n]. Note that as a side effect the old signature changes
  all over. 
  @raise Not_found if [f] has no height label. *)
  val modify_rlab : Function.t -> Function.t list -> t -> t
  (** [modify_rlab f fs s] replaces the outermost root label by a new root
  label with value [fs]. Note that as a side effect the old signature changes
  all over. 
  @raise Not_found if [f] has no root label. *)
  val modify_slab : Function.t -> int -> t -> t
  (** [modify_slab f n s] replaces the outermost semantic label by a new
  semantic label with value [n]. Note that as a side effect the old signature
  changes all over. 
  @raise Not_found if [f] has no semantic label. *)
  val modify_theory : Function.t -> Label.theory -> t -> t
  (** [modify_theory f n s] replaces the outermost theory label by a new
  theory label with value [n]. Note that as a side effect the old signature
  changes all over. 
  @raise Not_found if [f] has no theory label. *)
  val set_curry : ?arity:int -> Function.t -> int -> t -> Function.t * t
  (** [set_curry ~arity:a f n s] labels the function symbol [f] with the curry
  label [n]. Furthermore the arity of the labeled symbol is set to [a]. If
  [a] is not specified, the arity of the [f] is taken. Note that as a side
  effect the old signature changes all over. *)
  val set_dp : Function.t -> t -> Function.t * t
  (** [set_dp f s] labels the function symbol [f] as DP symbol. Note that as
  a side effect the old signature changes all over. *)
  val set_height : Function.t -> int -> t -> Function.t * t
  (** [set_height f n s] labels the function symbol [f] with height [n]. Note
  that as a side effect the old signature changes all over. *)
  val set_rlab : Function.t -> Function.t list -> t -> Function.t * t
  (** [set_rlab f fs s] labels the function symbol [f] with the root label
  [fs]. Note that as a side effect the old signature changes all over. *)
  val set_slab : Function.t -> int -> t -> Function.t * t
  (** [set_slab f n s] labels the function symbol [f] with the semantic label
  [n]. Note that as a side effect the old signature changes all over. *)
  val set_theory : Function.t -> Label.theory -> t -> Function.t * t
  (** [set_theory f t s] labels the function symbol [f] with the theory label
  [t]. Note that as a side effect the old signature changes all over. *) 
 
  (** {3 Search Functions} *)

  val get_curry : Function.t -> t -> int
  (** [get_curry f s] returns the value of the first curry label of [f].
  @raise Not_found if [f] does not admit a curry label. *)
  val get_height : Function.t -> t -> int
  (** [get_height f s] returns the value of the first height label of [f].
  @raise Not_found if [f] does not admit a height label. *)
  val get_rlab : Function.t -> t -> Function.t list
  (** [get_rlab f s] returns the value of the first root label of [f].
  @raise Not_found if [f] does not admit a root label. *)
  val get_slab : Function.t -> t -> int
  (** [get_slab f s] returns the value of the first semantic label of [f].
  @raise Not_found if [f] does not admit a semantic label. *)
  val get_theory : Function.t -> t -> Label.theory
  (** [get_theory f s] returns the value of the first theory label of [f].
  @raise Not_found if [f] does not admit a theory label. *) 
 
  (** {3 Properties} *)

  val is_curry : Function.t -> t -> bool
  (** [is_curry f s] checks if [f] admits a curry label. *)
  val is_dp : Function.t -> t -> bool
  (** [is_dp f s] checks if [f] is a DP symbol. *)
  val is_height : Function.t -> t -> bool
  (** [is_height f s] checks if [f] admits a height label. *)
  val is_rlab : Function.t -> t -> bool
  (** [is_rlab f s] checks if [f] admits a root label. *)
  val is_slab : Function.t -> t -> bool
  (** [is_slab f s] checks if [f] admits a semantic label. *)
  val is_theory : Label.theory -> Function.t -> t -> bool
  (** [is_theory tl f s] checks if [f] adheres to theory [tl]. *)
  val is_some_theory : Function.t -> t -> bool
  (** [is_some theory f s] checks if [f] adheres to some theory. *)

  (*val find_fun : string -> t -> Function.t*)

  (** {3 Printers} *)
 
  val fprintfx_fun : Format.formatter -> Function.t -> t -> t
  (** [fprintfx_fun fmt f s] prints [f] in XML using the [OCaml] module
  [Format]. This function uses the information stored in the signature
  [s] to print function symbols. If the name of a function symbol is not
  defined, a randomly generated name is printed. Note that as a side
  effect copies of [s] are changed too. Not tail-recursive. *)
  val fprintfx_var : Format.formatter -> Variable.t -> t -> t
  (** [fprintfx_var fmt x s] prints [x] in XML using the [OCaml] module
  [Format]. This function uses the information stored in the signature
  [s] to print variables. If the name of a variable is not defined, a
  randomly generated name is printed. Note that as a side effect copies
  of [s] are changed too. *)
  val to_stringx_fun : Function.t -> t -> string * t
  (** [to_stringx_fun f s] returns a formatted string that represents [f]
  in XML. This function uses the information stored in the signature [s] to
  print function symbols. If the name of a function symbol is not defined,
  a randomly generated name is printed. Note that as a side effect copies
  of [s] are changed too. Not tail-recursive. *)
  val to_stringx_var : Variable.t -> t -> string * t
  (** [to_stringx_var x s] returns a formatted string that represents [x]
  in XML. This function uses the information stored in the signature [s]
  to print variables. If the name of a variable is not defined, a randomly
  generated name is printed. Note that as a side effect copies of [s] are
  changed too. *)
 end

 module Monad : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.MONADX with
   type label = Label.t and type state = Signature.t

  (*** VALUES *****************************************************************)
  (** {3 Labeling Constructors and Destructors} *)
  
  (** {4 Generic Labelings } *)
  
  val add_int : ?arity:int -> Function.t -> int -> Function.t t
  (** [add_int ~arity:a i f] adds integer label [i] to [f] and sets
  its arity to [~arity]. Labels can be stacked. *)
  val drop_int : Function.t -> Function.t t
  (** [drop_int f] drops integer label of [f] from the top of the
  stack. Note that the arity of [f] is not changed by unlabeling.
  @raise No_int_label if [f] has no integer label *)
  val get_int : Function.t -> int t
  (** [get_int f] returns integer label of [f] on from the top of
  stack.
  @raise  No_int_label if [f] has no integer label. *)
  val is_int : Function.t -> bool t
  (** [is_int f] checks if [f] has an integer label*)

  (** {4 Specific Labelings } *)

  val add_rlab : Function.t -> Function.t -> unit t
  (** [add_rlab f g] is equivalent to
  {!val: Processors.Rewritingx.Signature.add_rlab} except that the signature is
  encapsulated in a monad. If [g] has no root label then the returned monad
  represents the error "not found". Note that as a side effect the underlying
  signature changes all over. *)
  val drop_curry : Function.t -> Function.t t
  (** [drop_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.drop_curry} except that the result is
  encapsulated in a monad. If [f] is not labeled with a curry label, then the
  returned monad represents the error "not found". *)
  val drop_dp : Function.t -> Function.t t
  (** [drop_dp f] is equivalent to
  {!val: Processors.Rewritingx.Signature.drop_dp} except that the result is
  encapsulated in a monad. If [f] is not a DP symbol, then the returned monad
  represents the error "not found". *)
  val drop_height : Function.t -> Function.t t
  (** [drop_height f] is equivalent to
  {!val: Processors.Rewritingx.Signature.drop_height} except that the result
  is encapsulated in a monad. If [f] is not labeled with a height label, then
  the returned monad represents the error "not found". *)
  val drop_rlab : Function.t -> Function.t t
  (** [drop_rlab f] is equivalent to
  {!val: Processors.Rewritingx.Signature.drop_rlab} except that the result is
  encapsulated in a monad. If [f] is not labeled with a root label, then the
  returned monad represents the error "not found". *)
  val drop_slab : Function.t -> Function.t t
  (** [drop_slab f] is equivalent to
  {!val: Processors.Rewritingx.Signature.drop_slab} except that the result is
  encapsulated in a monad. If [f] is not labeled with a semantic label, then
  the returned monad represents the error "not found". *)
  val drop_theory : Function.t -> Function.t t
  (** [drop_theory f] is equivalent to
  {!val: Processors.Rewritingx.Signature.drop_theory} except that the result
  is encapsulated in a monad. If [f] is not labeled with a theory label, then
  the returned monad represents the error "not found". *)
  val incr_curry : Function.t -> unit t
  (** [incr_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.incr_curry} except that the signature
  is encapsulated in a monad. If [f] has no curry label then the returned monad
  represents the error "not found". Note that as a side effect the underlying
  signature changes all over. *)
  val modify_curry : Function.t -> int -> unit t
  (** [modify_curry f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.modify_curry} except that the signature
  is encapsulated in a monad. If [f] has no curry label then the returned monad
  represents the error "not found". Note that as a side effect the underlying
  signature changes all over. *)
  val modify_height : Function.t -> int -> unit t
  (** [modify_height f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.modify_height} except that the
  signature is encapsulated in a monad. If [f] has no height label then the
  returned monad represents the error "not found". Note that as a side effect
  the underlying signature changes all over. *)
  val modify_rlab : Function.t -> Function.t list -> unit t
  (** [modify_rlab f fs] is equivalent to
  {!val: Processors.Rewritingx.Signature.modify_rlab} except that the signature
  is encapsulated in a monad. If [f] has no root label then the returned monad
  represents the error "not found". Note that as a side effect the underlying
  signature changes all over. *)
  val modify_slab : Function.t -> int -> unit t
  (** [modify_slab f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.modify_slab} except that the
  signature is encapsulated in a monad. If [f] has no semantic label then
  the returned monad represents the error "not found". Note that as a side
  effect the underlying signature changes all over. *)
  val set_curry : ?arity:int -> Function.t -> int -> Function.t t
  (** [set_curry ~arity:a f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.set_curry} except that the result is
  encapsulated in a monad. Note that as a side effect the underlying signature
  changes all over. *)
  val set_dp : Function.t -> Function.t t
  (** [set_dp f] is equivalent to
  {!val: Processors.Rewritingx.Signature.set_dp} except that the result is
  encapsulated in a monad. Note that as a side effect the underlying signature
  changes all over. *)
  val set_height : Function.t -> int -> Function.t t
  (** [set_height f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.set_height} except that the result is
  encapsulated in a monad. Note that as a side effect the underlying signature
  changes all over. *)
  val set_rlab : Function.t -> Function.t list -> Function.t t
  (** [set_rlab f fs] is equivalent to
  {!val: Processors.Rewritingx.Signature.set_rlab} except that the result is
  encapsulated in a monad. Note that as a side effect the underlying signature
  changes all over. *)
  val set_slab : Function.t -> int -> Function.t t
  (** [set_slab f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.set_slab} except that the result is
  encapsulated in a monad. Note that as a side effect the underlying signature
  changes all over. *)
  val set_theory : Function.t -> Label.theory -> Function.t t
  (** [set_theory f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.set_theory} except that the result is
  encapsulated in a monad. Note that as a side effect the underlying signature
  changes all over. *)

  (** {3 Labeling Search Functions} *)

  val get_curry : Function.t -> int t
  (** [get_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.get_curry} except that the result is
  encapsulated in a monad. If [f] is not labeled with a curry label, then the
  returned monad represents the error "not found". *)
  val get_height : Function.t -> int t
  (** [get_height f] is equivalent to
  {!val: Processors.Rewritingx.Signature.get_height} except that the result is
  encapsulated in a monad. If [f] is not labeled with a height label, then the
  returned monad represents the error "not found". *)
  val get_rlab : Function.t -> Function.t list t
  (** [get_rlab f] is equivalent to
  {!val: Processors.Rewritingx.Signature.get_rlab} except that the result is
  encapsulated in a monad. If [f] is not labeled with a root label, then the
  returned monad represents the error "not found". *)
  val get_slab : Function.t -> int t
  (** [get_slab f] is equivalent to
  {!val: Processors.Rewritingx.Signature.get_slab} except that the result is
  encapsulated in a monad. If [f] is not labeled with a semantic label, then
  the returned monad represents the error "not found". *)
  val get_theory : Function.t -> Label.theory t
  (** [get_theory f] is equivalent to
  {!val: Processors.Rewritingx.Signature.get_theory} except that the result is
  encapsulated in a monad. If [f] is not labeled with a theory label, then
  the returned monad represents the error "not found". *)

  (** {3 Labeling Properties} *)

  val is_curry : Function.t -> bool t
  (** [is_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_curry} except that the result is
  encapsulated in a monad. *)
  val is_dp : Function.t -> bool t
  (** [is_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_dp} except that the result is
  encapsulated in a monad. *)
  val is_height : Function.t -> bool t
  (** [is_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_height} except that the result is
  encapsulated in a monad. *)
  val is_rlab : Function.t -> bool t
  (** [is_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_rlab} except that the result is
  encapsulated in a monad. *)
  val is_slab : Function.t -> bool t
  (** [is_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_slab} except that the result is
  encapsulated in a monad. *)
  val is_theory : Label.theory -> Function.t -> bool t
  (** [is_theory f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_theory} except that the result is
  encapsulated in a monad. *)
  val is_some_theory : Function.t -> bool t
  (** [is_some_theory f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_some_theory} except that the 
  result is encapsulated in a monad. *)

  (** {3 Printers} *)

  val fprintfx_fun : Format.formatter -> Function.t -> unit t
  (** [fprintfx_fun fmt f] is equivalent to
  {!val: Processors.Rewritingx.Signature.fprintfx_fun} except that the
  signature is encapsulated in a monad. Note that as a side effect the
  old signature is changed too. Not tail-recursive. *)
  val fprintfx_var : Format.formatter -> Variable.t -> unit t
  (** [fprintfx_var fmt x] is equivalent to
  {!val: Processors.Rewritingx.Signature.fprintfx_var} except that the
  signature is encapsulated in a monad. Note that as a side effect the
  old signature is changed too. *)
  val to_stringx_fun : Function.t -> string t
  (** [to_stringx_fun f] is equivalent to
  {!val: Processors.Rewritingx.Signature.to_stringx_fun} except that the
  signature is encapsulated in a monad. Note that as a side effect the
  old signature is changed too. Not tail-recursive. *)
  val to_stringx_var : Variable.t -> string t
  (** [to_stringx_var x] is equivalent to
  {!val: Processors.Rewritingx.Signature.to_stringx_var} except that the
  signature is encapsulated in a monad. Note that as a side effect the
  old signature is changed too. *)
 end

 module Position : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.POSITION

  (*** VALUES *****************************************************************)
  (** {3 Printers} *)

  val fprintfx : Format.formatter -> t -> unit
  (** [fprintfx fmt p] prints [p] in XML using the [OCaml] module [Format]. *)
  val to_stringx : t -> string
  (** [to_stringx p] returns a formatted string that represents [p] in XML. *)
 end

 module Parser : Rewriting.PARSER
  with type state = Signature.t and type input = Parsec.StringInput.t

 module Term : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.TERM with type 'a m = 'a Monad.t and type 'a p = 'a Parser.t
    and type 'a x = 'a Parser.Xml.t

  (*** VALUES *****************************************************************)
  (** {3 Labeling and Unlabeling Functions} *)

  val label_dp : t -> t m
  (** [label_dp t] labels the root symbol of [t] as dependency pair symbol. *)
  val label_height : int -> t -> t m
  (** [label_height h t] labels all function symbols of [t] with height [h]. *)
  val unlabel_dp : t -> t m
  (** [unlabel_dp t] checks if the root symbol of [t] is a dependency pair
  symbol. If that is the case, the original symbol is restored. Otherwise
  [t] is returned. *)
  val unlabel_height : t -> t m
  (** [unlabel_height t] unlabels all function symbol that have been labeled
  with a height label. Note that if [t] does not contains a function symbol
  with height label then [t] is returned. *)
  val label_ac : t -> t m
  (** [label_ac t] labels the root symbol of [t] as AC symbol. *)


  (** {3 Printers} *)

  val fprintfx : Format.formatter -> t -> unit m
  (** [fprintfx fmt t] prints [t] in XML using the [OCaml] module [Format].
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
  val to_stringx : t -> string m
  (** [to_stringx t] returns a formatted string that represents [t] in XML.
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
 end

 module Context : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.CONTEXT with type term = Term.t

  (*** VALUES *****************************************************************)
  (** {3 Printers} *)

  val fprintfx : Format.formatter -> t -> unit m
  (** [fprintfx fmt c] prints [c] in XML using the [OCaml] module [Format].
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
  val to_stringx : t -> string m
  (** [to_stringx c] returns a formatted string that represents [c] in XML.
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
 end
 
 module Substitution : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.SUBSTITUTION with
   type term = Term.t and type context = Context.t

  (*** VALUES *****************************************************************)
  (** {3 Printers} *)

  val fprintfx : Format.formatter -> t -> unit m
  (** [fprintfx fmt s] prints [s] in XML using the [OCaml] module [Format].
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
  val to_stringx : t -> string m
  (** [to_stringx s] returns a formatted string that represents [s] in XML.
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
 end
 
 module Elogic : Rewriting.ELOGIC with
  type 'a m = 'a Monad.t and type substitution = Substitution.t and
  type term = Term.t
 
 module Rule : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.RULE with
   type 'a m = 'a Monad.t and type 'a p = 'a Parser.t
    and type substitution = Substitution.t and type term = Term.t
    and type 'a x = 'a Parser.Xml.t

  (*** VALUES *****************************************************************)
  (** {3 Printers} *)

  val fprintfx : Format.formatter -> t -> unit m
  (** [fprintfx fmt r] prints [r] in XML using the [OCaml] module [Format].
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
  val to_stringx : t -> string m
  (** [to_stringx r] returns a formatted string that represents [r] in XML.
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)

  val extend : t -> t option m
  (* Compute AC-extended rule if required. *)
 end
 
 module Trs : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.TRS with
   type 'a m = 'a Monad.t and type 'a p = 'a Parser.t
    and type rule = Rule.t and type term = Term.t
    and type 'a x = 'a Parser.Xml.t

  (*** VALUES *****************************************************************)
  (** {3 Miscellaneous} *)
  val ac_rencap : term -> t -> term m
  (** [accap t r] replaces all proper subterms of [t] that are rooted by 
  defined symbols of [r], and linearizes the term. *)
  val etcap : term -> t -> term m
  (** [etcap t r] modifies the term [t] such that it can be checked whether
  an instance of [t] rewrites to some term [u] with respect to the TRS [r],
  by using ground matching. *)
  val icap : term -> t -> term m
  (** [icap t r] modifies the term [t] such that it can be checked whether
  an instance of [t] rewrites to some term [u] with respect to the innermost
  rewrite relation induced by the TRS [r], by using unification. *)
  val is_strongly_nonoverlapping : t -> bool m
  (** [is_strongly_nonoverlapping r], checks if there are no overlaps after
  linearizing [r]. *)
  val linear : t -> t m
  (** [linear r] returns a complete linearization of [r]. I.e., all possible
  linearizations of left-hand sides are taken into consideration. *)
  val linearize : t -> t m
  (** [linearize r] linearizes [r]. In difference to
  {!val: Processors.Rewritingx.Trs.linear} only one linearization is
  computed. *)
  val recursors : t -> rule list
  (** [recursors r] returns all rewrite rules of [r] whose right-hand side
  contains a defined symbol. *)
  val sharp : t -> (Function.t * t) m
  (** [sharp r] transforms [r] into a rewrite system which can be used to
  compute the set of right-hand sides of forward closures via tree automata
  completion. *)
  val tcap : term -> t -> term m
  (** [tcap t r] modifies the term [t] such that it can be checked whether
  an instance of [t] rewrites to some term [u] with respect to the the TRS
  [r], by using unification. *)
  val extend : t -> t m
  (** [extend t] computes all AC extension rules of [r]. *)
  val srules : t -> t m
  (** [srules t] computes all S-rules corresponding to AC symbols occurring
  in the trs [t], required for AC-DPs. *)
  val theory : t -> t m
  (** [theory t] computes all AC-equations corresponding to AC symbols 
  occurring in the trs [t]. The resulting TRS is symmetric, i.e., inversed
  equations are included. *)

  (** {3 Printers} *)

  val fprintfx : Format.formatter -> t -> unit m
  (** [fprintfx fmt r] prints [r] in XML using the [OCaml] module [Format].
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
  val to_stringx : t -> string m
  (** [to_stringx r] returns a formatted string that represents [r] in XML.
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
 end
 
 module Diagram: sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.DIAGRAM with
   type 'a m = 'a Monad.t and type term = Term.t

  (*** VALUES *****************************************************************)
  (** {3 Miscellaneous} *)
  (** {3 Printers} *)
 end

 module Rewrite : Rewriting.REWRITE with
  type term = Term.t and type trs = Trs.t

 module Filtering : sig
  (*** TYPES ******************************************************************)
  type filter = Collapsing of int | List of int list;;
  type t

  (*** VALUES *****************************************************************)
  (** Constructors and Destructors *)

  val add : Function.t -> filter -> t -> t
  (** [add f filter af] adds a binding from [f] to [filter] to the argument
  filtering [af]. If [af] consists already a binding for [f], [af] remains
  unchanged. *)
  val empty : t
  (** [empty] returns and empty argument filtering. *)
  val of_list : (Function.t * filter) list -> t
  (** [of_list xs] computes an argument filtering containing all bindings of
  [xs]. Note that multiples of one and the same binding are added only once. *)
  val to_list : t -> (Function.t * filter) list
  (** [to_list af] returns [af] as a list.  *)

  (** Search Functions *)

  val find : Function.t -> t -> filter
  (** [find f af] returns the arguments to which the function symbol [f] is
  mapped with respect to the argument filtering [af].
  @raise Not_found if [af] does not contain a binding for [f]. *)

  (** Apply Functions *)

  val apply_rule : t -> Rule.t -> Rule.t
  (** [apply_rule af r] applies the argument filtering [af] to the left- and
  right-hand sides of rule [r].
  @raise Failure "incomplete filtering" if [af] does not contain a binding
  for all function symbols occurring in [r]. *)
  val apply_term : t -> Term.t -> Term.t
  (** [apply_term af t] applies the argument filtering [af] to the term [t].
  @raise Failure "incomplete filtering" if [af] does not contain a binding
  for all function symbols occurring in [t]. *)
  val apply_trs : t -> Trs.t -> Trs.t
  (** [apply_trs af r] applies the argument filtering [af] to the TRS [r].
  @raise Failure "incomplete filtering" if [af] does not contain a binding
  for all function symbols occurring in [r]. *)

  (** {3 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt af] prints [af] using the [OCaml] module [Format]. *)
  val fprintfm : Format.formatter -> t -> unit Monad.t
  (** [fprintfm fmt af] prints [af] using the [OCaml] module [Format].
  Function symbols are represented by their names registered in the
  underlying signature. If the name of a function symbol is not defined,
  a randomly generated name is printed. Note that as a side effect copies
  of the underlying signature are changed too. *)
  val fprintfx : Format.formatter -> t -> unit Monad.t
  (** [fprintfx fmt af] prints [af] in XML using the [OCaml] module [Format].
  Function symbols are represented by their names registered in the
  underlying signature. If the name of a function symbol is not defined,
  a randomly generated name is printed. Note that as a side effect copies
  of the underlying signature are changed too. This function is not
  tail-recursive. *)
  val to_string : t -> string
  (** [to_string af] returns a formatted string that represents [af]. *)
  val to_stringm : t -> string Monad.t
  (** [to_stringm af] returns a formatted string that represents [af].
  Function symbols are represented by their names registered in the
  underlying signature. If the name of a function symbol is not defined,
  a randomly generated name is used. Note that as a side effect copies
  of the underlying signature are changed too. *)
  val to_stringx : t -> string Monad.t
  (** [to_stringx af] returns a formatted string that represents [af] in
  XML. Function symbols are represented by their names registered in the
  underlying signature. If the name of a function symbol is not defined,
  a randomly generated name is printed. Note that as a side effect copies
  of the underlying signature are changed too. This function is not
  tail-recursive. *)
 end
 
 module Graph : sig
  (*** INCLUDES ***************************************************************)
  include Util.Graph.SIGNATURE with
   type node = Rule.t and type edge = Rule.t * Rule.t

  (*** VALUES *****************************************************************)
  (** {3 Printers} *)

  val fprintfm : Format.formatter -> t -> unit Monad.t
  (** [fprintfm fmt g] prints [g] using the [OCaml] module [Format].
  Function symbols and variables are represented by their names registered
  in the underlying signature. If the name of a function symbol or variable
  is not defined, a randomly generated name is printed. Note that as a side
  effect copies of the underlying signature are changed too. This function
  is not tail-recursive. *)
  val fprintfx : Format.formatter -> t -> unit Monad.t
  (** [fprintfx fmt g] prints [g] in XML using the [OCaml] module [Format].
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
  val to_stringm : t -> string Monad.t
  (** [to_stringm g] returns a formatted string that represents [g].
  Function symbols and variables are represented by their names registered
  in the underlying signature. If the name of a function symbol or variable
  is not defined, a randomly generated name is used. Note that as a side
  effect copies of the underlying signature are changed too. This function
  is not tail-recursive. *)
  val to_stringx : t -> string Monad.t
  (** [to_stringx g] returns a formatted string that represents [g] in XML.
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
 end

 module Projection : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** Constructors and Destructors *)

  val add : Function.t -> int -> t -> t
  (** [add f ps sp] adds a binding from [f] to [ps] to the simple projection
  [sp]. If [sp] consists already a binding for [f], [sp] remains unchanged. *)
  val empty : t
  (** [empty] returns and empty simple projection. *)
  val of_list : (Function.t * int) list -> t
  (** [of_list xs] computes a simple projection containing all bindings of
  [xs]. Note that multiples of one and the same binding are added only once. *)
  val to_list : t -> (Function.t * int) list
  (** [to_list sp] returns [sp] as a list. *)

  (** Search Functions *)

  val find : Function.t -> t -> int
  (** [find f sp] returns the argument to which the function symbol [f] is
  mapped with respect to the simple projection [sp].
  @raise Not_found if [sp] does not contain a binding for [f]. *)

  (** Apply Functions *)

  val apply_rule : t -> Rule.t -> Rule.t
  (** [apply_rule sp r] applies the simple projection [sp] to the left- and
  right-hand sides of rule [r].
  @raise Failure "incomplete filtering" if [sp] does not contain a binding
  for all function symbols occurring in [r]. *)
  val apply_term : t -> Term.t -> Term.t
  (** [apply_term sp t] applies the simple projection [sp] to the term [t].
  @raise Failure "incomplete filtering" if [sp] does not contain a binding
  for all function symbols occurring in [t]. *)
  val apply_trs : t -> Trs.t -> Trs.t
  (** [apply_trs sp r] applies the simple projection [sp] to the TRS [r].
  @raise Failure "incomplete filtering" if [sp] does not contain a binding
  for all function symbols occurring in [r]. *)

  (** {3 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt sp] prints [sp] using the [OCaml] module [Format]. *)
  val fprintfm : Format.formatter -> t -> unit Monad.t
  (** [fprintfm fmt sp] prints [sp] using the [OCaml] module [Format].
  Function symbols are represented by their names registered in the
  underlying signature. If the name of a function symbol is not defined,
  a randomly generated name is printed. Note that as a side effect copies
  of the underlying signature are changed too. This function is not
  tail-recursive. *)
  val fprintfx : Format.formatter -> t -> unit Monad.t
  (** [fprintfx fmt sp] prints [sp] in XML using the [OCaml] module [Format].
  Function symbols are represented by their names registered in the
  underlying signature. If the name of a function symbol is not defined,
  a randomly generated name is printed. Note that as a side effect copies
  of the underlying signature are changed too. This function is not
  tail-recursive. *)
  val to_string : t -> string
  (** [to_string sp] returns a formatted string that represents [sp]. *)
  val to_stringm : t -> string Monad.t
  (** [to_stringm sp] returns a formatted string that represents [sp].
  Function symbols are represented by their names registered in the
  underlying signature. If the name of a function symbol is not defined,
  a randomly generated name is used. Note that as a side effect copies
  of the underlying signature are changed too. This function is not
  tail-recursive. *)
  val to_stringx : t -> string Monad.t
  (** [to_stringx sp] returns a formatted string that represents [sp] in
  XML. Function symbols are represented by their names registered in the
  underlying signature. If the name of a function symbol is not defined,
  a randomly generated name is printed. Note that as a side effect copies
  of the underlying signature are changed too. This function is not
  tail-recursive. *)
 end

 module Aclogic : sig
  (*** VALUES *****************************************************************)
  (** {3 Miscellaneous} *)
  val are_unifiable : Term.t -> Term.t -> bool Monad.t
  (** [are_unifiable s t] returns [true] if [s] and [t] are unifiable 
  modulo AC, [false] otherwise. Note that at least one term is assumed to 
  be linear. *)
 end
end

(** {2 Module Problem} *)

(** Problem
@author Martin Korp
@since  Mon Sep  1 12:22:31 CEST 2009 *)

(** Defines the different termination problems that are supported by TTT2. *)
module Problem : sig
 (*** TYPES *******************************************************************)
 type graph = Complete | Partial of Rewritingx.Graph.t
 type language = All | Constructor;;
 type strategy = Full | Innermost | Outermost
 type problem
 type t

 (*** VALUES ******************************************************************)
 (** {3 Constructors} *)

 val adapt : problem -> t -> t
 (** [adapt p' p] replaces the internal problem of [p] by [p']. *)
 val create_cp : Rewritingx.Trs.t -> Rewritingx.Trs.t -> problem
 (** [create_cp s w] returns the CP problem consisting of the strict TRS [s]
 and the weak TRS [w]. *)
 val create_dp : Rewritingx.Trs.t -> Rewritingx.Trs.t -> graph -> problem
 (** [create_dp dp trs dg] returns the DP problem consisting of the dependency
 pairs [dp], the TRS [trs], and the dependency graph [dg]. *)
 val create_rp : Rewritingx.Trs.t -> Rewritingx.Trs.t -> problem
 (** [create_rp s w] returns the relative termination problem consisting of
 the strict TRS [s] and the weak TRS [w]. *)
 val create_sp : Rewritingx.Trs.t -> problem
 (** [create_sp trs] returns the standard termination problem consisting of
 the TRS [trs]. *)
 val make_cp : language -> strategy -> Rewritingx.Trs.t -> Rewritingx.Trs.t -> t
 (** [make_cp l strategy s w] returns the [strategy]-CP problem consisting of
 the strict TRS [s] and the weak TRS [w]. The argument [l] specifies the set
 of terms of which termination should be proved. *)
 val make_dp : language -> strategy -> Rewritingx.Trs.t -> Rewritingx.Trs.t
  -> graph -> t
 (** [make_dp l strategy dp trs dg] returns the [strategy]-DP problem
 consisting of the dependency pairs [dp], the TRS [trs], and the dependency
 graph [dg]. The argument [l] specifies the set of terms of which termination
 should be proved. *)
 val make_ep : language -> strategy -> Rewritingx.Trs.t -> Rewritingx.Trs.t -> t
 (** [make_ep l strategy eqs trs] returns the [strategy]-equational termination 
 problem consisting of the theory [eqs] and the TRS [trs]. The TRS [eqs] is
 assumed to be symmetric. The argument [l] specifies the set of terms
 of which termination should be proved. *)
 val make_edp : language -> strategy -> Rewritingx.Trs.t -> Rewritingx.Trs.t -> Rewritingx.Trs.t -> Rewritingx.Trs.t -> Rewritingx.Trs.t -> graph -> t
 (** [make_ep l strategy eqssharp dps eqs trs s] returns the [strategy]-
 equational DP problem consisting of the theory [eqs] and the TRS [trs],
 together with sharped version of the theory [eqssharp] and DPs [dps] and
 S-rules [s]. The theories are assumed to be symmetric. The argument [l] 
 specifies the set of terms of which termination should be proved. *)
 val make_rp : language -> strategy -> Rewritingx.Trs.t -> Rewritingx.Trs.t -> t
 (** [make_rp l strategy s w] returns the [strategy]-relative termination
 problem consisting of the strict TRS [s] and the weak TRS [w]. The argument
 [l] specifies the set of terms of which termination should be proved. *)
 val make_sp : language -> strategy -> Rewritingx.Trs.t -> t
 (** [make_sp l strategy trs] returns the [strategy]-termination problem
 consisting of the TRS [trs]. The argument [l] specifies the set of terms
 of which termination should be proved. *)
 val minimize : t -> t
 (** [minimize p] in case that [p] is a DP problem, the dependency graph of
 [p] is restricted to the current set of dependency pairs. Otherwise the
 problem remains unchanged. *)
 val set_dg : graph -> t -> t
 (** [set_dg dg p] replaces the dependency graph of the DP problem [p] by
 [dg]. Note that [None] represents the fully connected graph which has the
 dependency pairs of [p] as nodes.
 @raise Failure "not a DP problem" if [p] is not a DP problem. *)
 val set_dps : Rewritingx.Trs.t -> t -> t
 (** [set_dps dp p] replaces the dependency pairs of the DP problem [p] by [dp].
 @raise Failure "not a DP problem" if [p] is not a DP problem. *)
 val set_language : language -> t -> t
 (** [set_language l p] replaces the language of the problem [p] by [l]. *)
 val set_strategy : strategy -> t -> t
 (** [set_strategy s p] replaces the strategy of the problem [p] by [s]. *)
 val set_strict : Rewritingx.Trs.t -> t -> t
 (** [set_strict s p] replaces the strict TRS of the relative termination
 or CP problem [p] by [s].
 @raise Failure "not a relative termination or CP problem" if [p] is neither
 a relative termination problem nor a CP problem. *)
 val set_sw : Rewritingx.Trs.t -> Rewritingx.Trs.t -> t -> t
 (** [set_sw s w p] replaces the dependency pairs (TRS, strict TRS) of the
 dependency pair problem (standard termination problem, relative termination
 or CP problem) [p] by [s] and the TRS (weak TRS) of the dependency pair
 problem (relative termination or CP problem) by [w].
 @raise Failure "not a standard problem" if [p] is a standard termination
 problem and [w] is not the empty TRS. *)
 val set_trs : Rewritingx.Trs.t -> t -> t
 (** [set_trs trs p] replaces the TRS of [p] by [s] if [p] is a standard
 termination or DP problem.
 @raise Failure "not a standard termination or DP problem" if [p] is not
 a standard termination or DP problem. *)
 val set_weak : Rewritingx.Trs.t -> t -> t
 (** [set_weak w p] replaces the weak TRS of the relative termination or CP
 problem [p] by [s].
 @raise Failure "not a relative termination or CP problem" if [p] is neither
 a relative termination problem nor a CP problem. *)
val set_srules : Rewritingx.Trs.t -> t -> t

 (** {3 Access Functions} *)

 val get_dg : t -> graph
 (** [get_dg p] returns the dependency graph of the DP problem [p]. Note
 that [None] represents the fully connected graph which has the dependency
 pairs of [p] as nodes.
 @raise Failure "not a DP problem" if [p] is not a DP problem. *)
 val get_cds : t -> Rewritingx.Diagram.t list
 (** [get_cds p] returns the critical diagrams of [p].
 @raise Failure "not a confluence problem" if [p] is not a confluence problem. *)
 val get_dps : t -> Rewritingx.Trs.t
 (** [get_dps p] returns the dependency pairs of the DP problem [p].
 @raise Failure "not a DP problem" if [p] is not a DP problem. *)
 val get_language : t -> language
 (** [get_language p] returns the language of the termination problem [p]. *)
 val get_strategy : t -> strategy
 (** [get_strategy p] returns the strategy of the termination problem [p]. *)
 val get_strict : t -> Rewritingx.Trs.t
 (** [get_strict p] returns the strict TRS of the relative termination or CP
 problem [p].
 @raise Failure "not a relative termination or CP problem" if [p] is neither
 a relative termination problem nor a CP problem. *)
 val get_trs : t -> Rewritingx.Trs.t
 (** [get_trs p] returns the TRS of [p] if [p] is a standard termination or
 DP problem.
 @raise Failure "not a standard termination or DP problem" if [p] is not a
 standard termination problem. *)
 val get_weak : t -> Rewritingx.Trs.t
 (** [get_weak p] returns the weak TRS of the relative termination or CP
 problem [p].
 @raise Failure "not a relative termination or CP problem" if [p] is neither
 a relative termination problem nor a CP problem. *)
 val get_srules : t -> Rewritingx.Trs.t
 val get_sw : t -> Rewritingx.Trs.t * Rewritingx.Trs.t
 (** [get_sw p] returns the dependency pairs (TRS, strict TRS) as well as
 the TRS (weak TRS) of the dependency pair problem (standard termination
 problem, relative termination or CP problem) [p]. *)

 (** {3 Properties} *)

 val exists : (Rewritingx.Trs.t -> bool) -> t -> bool
 (** [exists f p] checks if at least one of the TRSs of [p] satisfy the
 predicate [f]. *)
 val existsm : (Rewritingx.Trs.t -> bool Rewritingx.Monad.t) -> t
  -> bool Rewritingx.Monad.t
 (** [existsm f p] is equivalent to {!val: Processors.Problem.exists}
 except that the result is encapsulated in a monad. *)
 val for_all : (Rewritingx.Trs.t -> bool) -> t -> bool
 (** [for_all f p] checks if the TRSs of [p] satisfy the predicate [f]. *)
 val for_allm : (Rewritingx.Trs.t -> bool Rewritingx.Monad.t) -> t
  -> bool Rewritingx.Monad.t
 (** [for_allm f p] is equivalent to {!val: Processors.Problem.for_all}
 except that the result is encapsulated in a monad. *)
 val is_al : t -> bool
 (** [is_al p] checks if termination of [p] should be proved for all terms. *)
 val is_cl : t -> bool
 (** [is_cl p] checks if termination of [p] should be proved just for
 constructor terms. *)
 val is_cp : t -> bool
 (** [is_cp p] checks if [p] is a CP problem. *)
 val is_dp : t -> bool
 (** [is_dp p] checks if [p] is a DP problem. *)
 val is_ep : t -> bool
 (** [is_ep p] checks if [p] is an equational problem. *)
 val is_edp : t -> bool
 (** [is_edp p] checks if [p] is an equational DP problem. *)
 val is_empty : t -> bool
 (** [is_empty p] checks if the problem [p] is empty, i.e., trivially
 terminating. *)
 val is_ft : t -> bool
 (** [is_ft p] checks if [p] uses the full rewriting strategy. *)
 val is_it : t -> bool
 (** [is_it p] checks if [p] uses the innermost strategy. *)
 val is_ot : t -> bool
 (** [is_ot p] checks if [p] uses the outermost strategy. *)
 val is_rp : t -> bool
 (** [is_rp p] checks if [p] is a relative termination problem. *)
 val is_sp : t -> bool
 (** [is_sp p] checks if [p] is a standard termination problem. *)

 (** {3 Compare Functions} *)

 val equal : t -> t -> bool
 (** [equal p p'] checks if [p] and [p'] are equal. *)

 (** {3 Printers} *)

 val fprintf : ?g:bool -> Format.formatter -> t -> unit
 (** [fprintf ~g:g fmt p] prints [p] using the [OCaml] module [Format]. If
 [g] is set to [true], the graph of a DP problem is printed. Per default
 [g = false]. *)
 val fprintfm : ?g:bool -> Format.formatter -> t -> unit Rewritingx.Monad.t
 (** [fprintfm ~g:g fmt p] prints [p] using the [OCaml] module [Format]. If
 [g] is set to [true], the graph of a DP problem is printed. Per default
 [g = false]. Function symbols and variables are represented by their names
 registered in the underlying signature. If the name of a function symbol or
 variable is not defined, a randomly generated name is printed. Note that as
 a side effect copies of the underlying signature are changed too. This
 function is not tail-recursive. *)
 val fprintfx : ?g:bool -> Format.formatter -> t -> unit Rewritingx.Monad.t
 (** [fprintfx ~g:g fmt p] prints [p] in XML using the [OCaml] module [Format].
 If [g] is set to [true], the graph of a DP problem is printed. Per default
 [g = false]. Function symbols and variables are represented by their names
 registered in the underlying signature. If the name of a function symbol or
 variable is not defined, a randomly generated name is printed. Note that as
 a side effect copies of the underlying signature are changed too. This
 function is not tail-recursive. *)
 val to_string : ?g:bool -> t -> string
 (** [to_string ~g:g p] returns a string that represents [p]. If [g] is set
 to [true], the graph of a DP problem is printed. Per default [g = false]. *)
 val to_stringm : ?g:bool -> t -> string Rewritingx.Monad.t
 (** [to_stringm ~g:g p] returns a formatted string that represents [p].
 If [g] is set to [true], the graph of a DP problem is printed. Per default
 [g = false]. Function symbols and variables are represented by their names
 registered in the underlying signature. If the name of a function symbol or
 variable is not defined, a randomly generated name is used. Note that as a
 side effect copies of the underlying signature are changed too. This
 function is not tail-recursive. *)
 val to_stringx : ?g:bool -> t -> string Rewritingx.Monad.t
 (** [to_stringx ~g:g p] returns a formatted string in XML that represents
 [p]. If [g] is set to [true], the graph of a DP problem is printed. Per
 default [g = false]. Function symbols and variables are represented by their
 names registered in the underlying signature. If the name of a function symbol
 or variable is not defined, a randomly generated name is used. Note that as a
 side effect copies of the underlying signature are changed too. This function
 is not tail-recursive. *)
end

(** {2 Module Modifier} *)

(** Modifiers
@author Martin Korp
@since  Mon Sep  1 12:22:31 CEST 2009 *)

(** This module provides several techniques which can be used to modify
a termination problem. In difference to transformation techniques, these
methods get additional (history) information. *)
module Modifier : sig
 (*** MODULES *****************************************************************)
 (** {3 Module Restore} *)
 
 (** Restore TRS
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a modifier which restores the TRS of a given
 DP problem. *)
 module Restore : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  modifier within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this modifier and its flags. *)

  (** {4 Modifier} *)

  val solve : string list -> Problem.t -> Problem.t -> t option
  (** [solve fs p q] transforms [q] by replacing the underlying TRS with the
  one of [p]. If one of the given problems is not a DP problem, [None] is
  returned.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ips : t -> Problem.t * Problem.t
  (** [get_ips p] returns the two input problems. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the resulting DP problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end
end

(** {2 Module Nontermination} *)

(** Nontermination Techniques
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Mon Sep  1 12:22:31 CEST 2009 *)

(** This module provides several techniques which can be used to prove
that a certain TRS is non-terminating. *)
module Nontermination : sig
 (*** MODULES *****************************************************************)
 (** {3 Module Contained} *)
 
 (** Containment Processor
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements the containment processor which proves
 nontermination of a given TRS by checking whether there exists a rewrite
 rule [l -> r] such that an instance of [l] is a subterm of [r]. *)
 module Contained : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option
  (** [solve fs p] checks if [p] is nonterminating. If [p] is nonterminating
  then [Some q] is returned, where [q] is the corresponding proof. Otherwise,
  [None] is returned.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem of [p]. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module LoopSat} *)
 
 (** Loops for SRSs
 @author Harald Zankl
 @since  Mon May 11 20:03:20 CEST 2009 *)
 
 (** Find Loops (for SRSs) by encoding a looping reduction in SAT *)
 module LoopSat : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] checks if [p] is nonterminating. If [p] is nonterminating
  then [Some q] is returned, where [q] is the corresponding proof. Otherwise,
  [None] is returned.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Unfolding} *)
 
 (** Loops for SRSs and TRSs
 @author Christian Sternagel
 @since Fri May 15 16:38:52 CEST 2009 *)
 
 (**  Find loops using unfoldings. *)
 module Unfolding : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] checks if [p] is nonterminating. If [p] is nonterminating
  then [Some q] is returned, where [q] is the corresponding proof. Otherwise,
  [None] is returned.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Variables} *)
 
 (** Fresh Variable Processor
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements the fresh variable processor which proves
 nontermination of a given TRS by checking whether it fulfills the
 variable condition (i.e., for each rewrite rule [l -> r], [l] is not
 a variable and each variable that occurs in [r] occurs also in [l]). *)
 module Variables : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option
  (** [solve fs p] checks if [p] is nonterminating. If [p] is nonterminating 
  then [Some q] is returned, where [q] is the corresponding proof. Otherwise,
  [None] is returned.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem of [p]. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end
end

(** {2 Module Predicate} *)

(** Predicates
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Mon Sep  1 12:22:31 CEST 2009 *)

(** This module provides several predicates which can be used within
strategies do decide which substrategy should be chosen. *)
module Predicate : sig
 (** {3 Module Applicative} *)
 
 (** Applicative TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of applicative TRSs. *)
 module Applicative : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool Rewritingx.Monad.t
  (** [solve fs p] checks if [p] consists entirely of applicative TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Collapsing} *)
 
 (** Collapsing TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of collapsing TRSs. *)
 module Collapsing : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists entirely of collapsing TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Constructor} *)
 
 (** Constructor TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of constructor TRSs. *)
 module Constructor : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists entirely of constructor TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Dummy} *)
 
 (** Dummy TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of dummy TRSs. *)
 module Dummy : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists entirely of dummy TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Duplicating} *)
 
 (** Duplicating TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of duplicating TRSs. *)
 module Duplicating : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists a duplicating rule.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Erasing } *)
 
 (** Erasing TRSs
 @author Harald Zankl
 @since  Wed Aug 18 16:03:30 CEST 2010 *)
 
 (** This module implements a predicate which checks if the given problem
 is erasing. *)
 module Erasing : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] contains an erasing rule.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Flat} *)
 
 (** Flat TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of flat TRSs. *)
 module Flat : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists entirely of flat TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Full} *)
 
 (** Full Rewriting
 @author Christian Sternagel
 @since  Thu Jan  7 14:45:45 CET 2010 *)
 
 (** This module implements a predicate which checks if the given problem
 uses full termination. *)
 module Full : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] uses full rewriting.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Ground} *)
 
 (** Ground TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of ground TRSs. *)
 module Ground : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists entirely of ground TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Innermost} *)
 
 (** Innermost Rewriting
 @author Christian Sternagel
 @since  Thu Jan  7 14:45:45 CET 2010 *)
 
 (** This module implements a predicate which checks if the given problem
 uses full termination. *)
 module Innermost : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] uses innermost rewriting.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module LeftGround} *)
 
 (** Left-Ground TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of left-ground TRSs. *)
 module LeftGround : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists entirely of left-ground TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module LeftLinear} *)
 
 (** Left-Linear TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of left-linear TRSs. *)
 module LeftLinear : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists entirely of left-linear TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Linear} *)
 
 (** Linear TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of linear TRSs. *)
 module Linear : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists entirely of linear TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Overlapping} *)
 
 (** Overlapping TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of overlapping TRSs. *)
 module Overlapping : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool Rewritingx.Monad.t
  (** [solve fs p] checks if [p] consists entirely of overlapping TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Overlay} *)
 
 (** Overlay TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of overlay TRSs. *)
 module Overlay : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool Rewritingx.Monad.t
  (** [solve fs p] checks if [p] consists entirely of overlay TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Outermost} *)
 
 (** Outermost Rewriting
 @author Christian Sternagel
 @since  Thu Jan  7 14:45:45 CET 2010 *)
 
 (** This module implements a predicate which checks if the given problem
 uses outermost termination. *)
 module Outermost : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] uses outermost rewriting.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Relative} *)
 
 (** Relative Problem
 @author Harald Zankl
 @since  Wed Jun 30 13:47:41 CEST 2010 *)
 
 (** This module implements a predicate which checks if the given problem
 is a relative problem. *)
 module Relative: sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] is a relative problem.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module RightGround} *)
 
 (** Right-Ground TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of right-ground TRSs. *)
 module RightGround : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists entirely of right-ground TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module RightLinear} *)
 
 (** Right-Linear TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of right-linear TRSs. *)
 module RightLinear : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists entirely of right-linear TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Shallow} *)
 
 (** Shallow TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of shallow TRSs. *)
 module Shallow : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists entirely of shallow TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Srs} *)
 
 (** SRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of SRSs. *)
 module Srs : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists entirely of SRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Standard} *)
 
 (** Standard Problem
 @author Harald Zankl
 @since  Wed Jun 30 13:47:41 CEST 2010 *)
 
 (** This module implements a predicate which checks if the given problem
 is a standard problem (in contrast to a relative problem). *)
 module Standard: sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] is a standard problem.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module StronglyNonOverlapping} *)
 
 (** Strongly Non-Overlapping Problem
 @author Christian Sternagel
 @since Thu Aug 26 18:43:57 CEST 2010 *)
 
 (** This module implements a predicate which checks if the given problem
 is strongly non-overlapping. *)
 module StronglyNonOverlapping : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool Rewritingx.Monad.t
  (** [solve fs p] checks if [p] is strongly non-overlapping.
  @raise Failure if [fs] consists of some unknown flag. *)
 end

 (** {3 Module Trs} *)
 
 (** TRSs
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements a predicate which checks if the given problem
 consists of TRSs. *)
 module Trs : sig
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  predicate within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this predicate and its flags. *)

  (** {4 Predicate} *)

  val solve : string list -> Problem.t -> bool
  (** [solve fs p] checks if [p] consists entirely of TRSs.
  @raise Failure if [fs] consists of some unknown flag. *)
 end
end

(** {2 Module Termination} *)

(** Termination Techniques
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Mon Sep  1 12:22:31 CEST 2009 *)

(** This module provides several techniques which can be used to prove
termination of a TRS. *)
module Termination : sig
 (*** MODULES *****************************************************************)
 (** {3 Module Arctic} *)
 
 (** Arctic Matrix Processor
 @author Harald Zankl
 @since  Mon May 11 12:24:21 CEST 2009 *)
 
 (** This module implements the arctic matrix method. *)
 module Arctic: sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] tries to orient rules from [p] by using arctic matrix
  interpretations.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints certified output of [p] *)
 end

 (** {3 Module Bounds} *)

 (** Match-Bounds
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements the match-bound technique for TRSs. *)
 module Bounds : sig
  (*** TYPES ******************************************************************)
  type t                                                                
                                                                        
  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its
  flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] tries to remove rules from [p] by using match-bounds.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Dp} *)
 
 (** DP Processor
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements the dependency pair processor which transforms a
 TRS into the initial DP problem. *)
 module Dp : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] transforms [p] into the corresponding initial DP problem.
  If the given problem is not a standard termination problem, [None] is
  returned.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input TRS. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the DP problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Dg} *)

 (** DG Processor
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements various dependency graph processors. *)
 module Dg : sig
  (*** MODULES ****************************************************************)
  (** {4 Module Adg} *)

  (** Approximated DG Processor
  @author Martin Korp
  @since  Mon May  5 17:10:46 CEST 2010 *)
  
  (** This module implements a dependency graph processors based on tree
  automata techniques. The approximated dependency graph does not consist
  of an arc from a dependency pair [s] to a dependency pair [t] if no ground
  instance of the right-hand side of [s] can be rewritten to some ground
  instance of the left-hand side of [t] and vice versa. To decide both
  conditions the underlying TRS is approximated. *)
  module Adg : sig
   (*** TYPES *****************************************************************)
   type t

   (*** VALUES ****************************************************************)
   (** {5 Globals} *)

   val code : string
   (** [code] defines the shortcut that has to be used to refer to this
   processor within TTT2. *)
   val help : string * string list * (string * string) list
   (** [help] provides a detailed description of this processor and its
   flags. *)

   (** {5 Processor} *)

   val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
   (** [solve fs p] removes all edges from the current dependency graph that
   are not present in the approximated dependency graph. If the processor does
   not make progress, [None] is returned.
   @raise Failure if [fs] consists of some unknown flag. *)

   (** {5 Destructors} *)

   val get_ip : t -> Problem.t
   (** [get_ip p] returns the initial DP problem. *)
   val get_op : t -> Problem.t
   (** [get_op p] returns the resulting DP problem. *)

   (** {5 Complexity Bounds} *)

   val complexity : Util.Complexity.t -> t -> Util.Complexity.t
   (** [complexity c p] returns information regarding the complexity of the
   new termination problem. *)

   (** {5 Compare Functions} *)

   val equal : t -> t -> bool
   (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

   (** {5 Printers} *)
   
   val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
    -> Format.formatter -> t -> unit Rewritingx.Monad.t
   (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
   [OCaml] module [Format]. Function symbols and variables are represented
   by their names registered in the underlying signature. If the name of a
   function symbol or variable is not defined, a randomly generated name is
   printed. Note that as a side effect copies of the underlying signature
   are changed too. This function is not tail-recursive. *)
   val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
    -> Format.formatter -> t -> unit Rewritingx.Monad.t
   (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
   the [OCaml] module [Format]. Function symbols and variables are represented
   by their names registered in the underlying signature. If the name of a
   function symbol or variable is not defined, a randomly generated name is
   printed. Note that as a side effect copies of the underlying signature are
   changed too. This function is not tail-recursive. *)
  end

  (** {4 Module Cdg} *)

  (** Completion DG Processor
  @author Martin Korp
  @since  Mon Sep  1 12:22:31 CEST 2009 *)
  
  (** This module implements a dependency graph processors based on tree
  automata techniques. The c-dependency graph does not consist of an arc
  from a dependency pair [s] to a dependency pair [t] if a compatible tree
  automaton could be constructed which accepts all ground instances of
  [ren(rhs(s))] but not [lhs(t)]. *)
  module Cdg : sig
   (*** TYPES *****************************************************************)
   type t

   (*** VALUES ****************************************************************)
   (** {5 Globals} *)

   val code : string
   (** [code] defines the shortcut that has to be used to refer to this
   processor within TTT2. *)
   val help : string * string list * (string * string) list
   (** [help] provides a detailed description of this processor and its
   flags. *)

   (** {5 Processor} *)

   val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
   (** [solve fs p] removes all edges from the current dependency graph that
   are not present in the c-dependency graph. If the processor does not make
   progress, [None] is returned.
   @raise Failure if [fs] consists of some unknown flag. *)

   (** {5 Destructors} *)

   val get_ip : t -> Problem.t
   (** [get_ip p] returns the initial DP problem. *)
   val get_op : t -> Problem.t
   (** [get_op p] returns the resulting DP problem. *)

   (** {5 Complexity Bounds} *)

   val complexity : Util.Complexity.t -> t -> Util.Complexity.t
   (** [complexity c p] returns information regarding the complexity of the
   new termination problem. *)

   (** {5 Compare Functions} *)

   val equal : t -> t -> bool
   (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

   (** {5 Printers} *)
   
   val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
    -> Format.formatter -> t -> unit Rewritingx.Monad.t
   (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
   [OCaml] module [Format]. Function symbols and variables are represented
   by their names registered in the underlying signature. If the name of a
   function symbol or variable is not defined, a randomly generated name is
   printed. Note that as a side effect copies of the underlying signature
   are changed too. This function is not tail-recursive. *)
   val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
    -> Format.formatter -> t -> unit Rewritingx.Monad.t
   (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
   the [OCaml] module [Format]. Function symbols and variables are represented
   by their names registered in the underlying signature. If the name of a
   function symbol or variable is not defined, a randomly generated name is
   printed. Note that as a side effect copies of the underlying signature are
   changed too. This function is not tail-recursive. *)
  end

  (** {4 Module Edg} *)

  (** Estimated DG Processor
  @author Martin Korp
  @since  Mon Sep  1 12:22:31 CEST 2009 *)
  
  (** This module implements the estimated dependency graph processors. The
  estimated dependency graph consists of an arc from a dependency pair [s]
  to a dependency pair [t] if and only if both [tcap(rhs(s),r)] unifies with
  [lhs(t)] and [tcap(lhs(t),rev(r))] unifies with [rhs(t)]. Here [r] denotes
  the TRS of the given DP problem. *)
  module Edg : sig
   (*** TYPES *****************************************************************)
   type t

   (*** VALUES ****************************************************************)
   (** {5 Globals} *)

   val code : string
   (** [code] defines the shortcut that has to be used to refer to this
   processor within TTT2. *)
   val help : string * string list * (string * string) list
   (** [help] provides a detailed description of this processor and its
   flags. *)

   (** {5 Processor} *)

   val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
   (** [solve fs p] removes all edges from the current dependency graph that
   are not present in the estimated dependency graph. If the processor does
   not make progress, [None] is returned.
   @raise Failure if [fs] consists of some unknown flag. *)

   (** {5 Destructors} *)

   val get_ip : t -> Problem.t
   (** [get_ip p] returns the initial DP problem. *)
   val get_op : t -> Problem.t
   (** [get_op p] returns the resulting DP problem. *)

   (** {5 Complexity Bounds} *)

   val complexity : Util.Complexity.t -> t -> Util.Complexity.t
   (** [complexity c p] returns information regarding the complexity of the
   new termination problem. *)

   (** {5 Compare Functions} *)

   val equal : t -> t -> bool
   (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

   (** {5 Printers} *)
   
   val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
    -> Format.formatter -> t -> unit Rewritingx.Monad.t
   (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
   [OCaml] module [Format]. Function symbols and variables are represented
   by their names registered in the underlying signature. If the name of a
   function symbol or variable is not defined, a randomly generated name is
   printed. Note that as a side effect copies of the underlying signature
   are changed too. This function is not tail-recursive. *)
   val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
    -> Format.formatter -> t -> unit Rewritingx.Monad.t
   (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
   the [OCaml] module [Format]. Function symbols and variables are represented
   by their names registered in the underlying signature. If the name of a
   function symbol or variable is not defined, a randomly generated name is
   printed. Note that as a side effect copies of the underlying signature are
   changed too. This function is not tail-recursive. *)
  end

  (** {4 Module Tdg} *)

  (** Trivial DG Processor
  @author Martin Korp
  @since  Mon Sep  1 12:22:31 CEST 2009 *)
  
  (** This module implements a trivial dependency graph processors. The trivial
  dependency graph consists of an arc from a dependency pair [s] to a dependency
  pair [t] if and only if the root symbol of the right-hand side of [s] is equal
  to the root symbol of the left-hand side of [t]. *)
  module Tdg : sig
   (*** TYPES *****************************************************************)
   type t

   (*** VALUES ****************************************************************)
   (** {5 Globals} *)

   val code : string
   (** [code] defines the shortcut that has to be used to refer to this
   processor within TTT2. *)
   val help : string * string list * (string * string) list
   (** [help] provides a detailed description of this processor and its
   flags. *)

   (** {5 Processor} *)

   val solve : string list -> Problem.t -> t option
   (** [solve fs p] removes all edges from the current dependency graph that
   are not present in the trivial dependency graph. If the processor does not
   make progress, [None] is returned.
   @raise Failure if [fs] consists of some unknown flag. *)

   (** {5 Destructors} *)

   val get_ip : t -> Problem.t
   (** [get_ip p] returns the initial DP problem. *)
   val get_op : t -> Problem.t
   (** [get_op p] returns the resulting DP problem. *)

   (** {5 Complexity Bounds} *)

   val complexity : Util.Complexity.t -> t -> Util.Complexity.t
   (** [complexity c p] returns information regarding the complexity of the
   new termination problem. *)

   (** {5 Compare Functions} *)

   val equal : t -> t -> bool
   (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

   (** {5 Printers} *)
   
   val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
    -> Format.formatter -> t -> unit Rewritingx.Monad.t
   (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
   [OCaml] module [Format]. Function symbols and variables are represented
   by their names registered in the underlying signature. If the name of a
   function symbol or variable is not defined, a randomly generated name is
   printed. Note that as a side effect copies of the underlying signature
   are changed too. This function is not tail-recursive. *)
   val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
    -> Format.formatter -> t -> unit Rewritingx.Monad.t
   (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
   the [OCaml] module [Format]. Function symbols and variables are represented
   by their names registered in the underlying signature. If the name of a
   function symbol or variable is not defined, a randomly generated name is
   printed. Note that as a side effect copies of the underlying signature are
   changed too. This function is not tail-recursive. *)
  end

 (** {4 Module Odg} *)

  (** DG Processor corresponding to old edg
  @author Sarah Winkler
  @since  Mon Mar  21 12:22:31 CEST 2011 *)
  
  (** This module implements a trivial dependency graph processors. The trivial
  dependency graph consists of an arc from a dependency pair [s] to a dependency
  pair [t] if and only if the root symbol of the right-hand side of [s] is equal
  to the root symbol of the left-hand side of [t]. *)
  module Odg : sig
   (*** TYPES *****************************************************************)
   type t

   (*** VALUES ****************************************************************)
   (** {5 Globals} *)

   val code : string
   (** [code] defines the shortcut that has to be used to refer to this
   processor within TTT2. *)
   val help : string * string list * (string * string) list
   (** [help] provides a detailed description of this processor and its
   flags. *)

   (** {5 Processor} *)

   val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
   (** [solve fs p] removes all edges from the current dependency graph where ren(cap(r)) is not unifiable with l'. If the processor does not
   make progress, [None] is returned.
   @raise Failure if [fs] consists of some unknown flag. *)

   (** {5 Destructors} *)

   val get_idp : t -> Problem.t
   (** [get_dp p] returns the initial DP problem. *)
   val get_odp : t -> Problem.t
   (** [get_odp p] returns the resulting DP problem. *)

   (** {5 Complexity Bounds} *)

   val complexity : Util.Complexity.t -> t -> Util.Complexity.t
   (** [complexity c p] returns information regarding the complexity of the
   new termination problem. *)

   (** {5 Compare Functions} *)

   val equal : t -> t -> bool
   (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

   (** {5 Printers} *)
   
   val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
    -> Format.formatter -> t -> unit Rewritingx.Monad.t
   (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
   [OCaml] module [Format]. Function symbols and variables are represented
   by their names registered in the underlying signature. If the name of a
   function symbol or variable is not defined, a randomly generated name is
   printed. Note that as a side effect copies of the underlying signature
   are changed too. This function is not tail-recursive. *)
   val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
    -> Format.formatter -> t -> unit Rewritingx.Monad.t
   (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
   the [OCaml] module [Format]. Function symbols and variables are represented
   by their names registered in the underlying signature. If the name of a
   function symbol or variable is not defined, a randomly generated name is
   printed. Note that as a side effect copies of the underlying signature are
   changed too. This function is not tail-recursive. *)
  end

  (** Estimated DG Processor for AC DP problems
  @author Sarah Winkler
  @since  Mon Sep  1 12:22:31 CEST 2009 *)

  (** This module implements the estimated dependency graph processors. The
  estimated dependency graph consists of an arc from a dependency pair [s]
  to a dependency pair [t] if and only if both [tcap(rhs(s),r)] unifies with
  [lhs(t)] and [tcap(lhs(t),rev(r))] unifies with [rhs(t)]. Here [r] denotes
  the TRS of the given DP problem. *)
  module Acdg : sig
   (*** TYPES *****************************************************************)
   type t

   (*** VALUES ****************************************************************)
   (** {5 Globals} *)

   val code : string
   (** [code] defines the shortcut that has to be used to refer to this
   processor within TTT2. *)
   val help : string * string list * (string * string) list
   (** [help] provides a detailed description of this processor and its
   flags. *)

   (** {5 Processor} *)

   val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
   (** [solve fs p] removes all edges from the current dependency graph that
   are not present in the estimated dependency graph. If the processor does
   not make progress, [None] is returned.
   @raise Failure if [fs] consists of some unknown flag. *)

   (** {5 Destructors} *)

   val get_ip : t -> Problem.t
   (** [get_ip p] returns the initial DP problem. *)
   val get_op : t -> Problem.t
   (** [get_op p] returns the resulting DP problem. *)

   (** {5 Complexity Bounds} *)

   val complexity : Util.Complexity.t -> t -> Util.Complexity.t
   (** [complexity c p] returns information regarding the complexity of the
   new termination problem. *)

   (** {5 Compare Functions} *)

   val equal : t -> t -> bool
   (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

   (** {5 Printers} *)

   val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
    -> Format.formatter -> t -> unit Rewritingx.Monad.t
   (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
   [OCaml] module [Format]. Function symbols and variables are represented
   by their names registered in the underlying signature. If the name of a
   function symbol or variable is not defined, a randomly generated name is
   printed. Note that as a side effect copies of the underlying signature
   are changed too. This function is not tail-recursive. *)
   val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
    -> Format.formatter -> t -> unit Rewritingx.Monad.t
   (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
   the [OCaml] module [Format]. Function symbols and variables are represented
   by their names registered in the underlying signature. If the name of a
   function symbol or variable is not defined, a randomly generated name is
   printed. Note that as a side effect copies of the underlying signature are
   changed too. This function is not tail-recursive. *)
  end

 end

 (** {3 Module KBO} *)
 
 (** KBO Processor
 @author Harald Zankl
 @since  Fri May  1 20:16:08 CEST 2009 *)
 
 (** This module implements the Knuth-Bendix ordering. *)
 module Kbo: sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] tries to orient rules from [p] by using the simplification
  order KBO.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
 end

 (** {3 Module KBO} *)

 (** FBI Processor
 @author Harald Zankl
 @since  Fri May  1 20:16:08 CEST 2013 *)

 (** This module implements Fixed base Elementary Interpretations Processor. *)
 module Fbi: sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] tries to orient rules from [p] by using the simplification
  order KBO.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
 end

(** {3 Module KBO} *)
 
 (** TKBO Processor
 @author Harald Zankl
 @since  Fri May  1 20:16:08 CEST 2009 *)
 
 (** This module implements the Knuth-Bendix ordering. *)
 module Tkbo: sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] tries to orient rules from [p] by using the simplification
  order KBO.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
 end

 (** {3 Module LPO} *)
 
 (** LPO Processor
 @author Harald Zankl
 @since  Fri May  1 20:16:08 CEST 2009 *)
 
 (** This module implements the lexicographic path ordering. *)
 module Lpo: sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] tries to orient rules from [p] by using the simplification
  order LPO.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)

  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
 end

 (** {3 Module ACRPO} *)

 (** AC-RPO Processor
 @author Sarah Winkler
 @since  Fri September 9 20:16:08 CEST 2011 *)

 (** This module implements the AC-recursive path ordering. *)
 module Acrpo: sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] tries to orient rules from [p] by using the simplification
  order AC-RPO.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)

  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
 end

(** {3 Module ACRPO} *)

 (** AC-RPO Processor
 @author Sarah Winkler
 @since  Fri September 9 20:16:08 CEST 2011 *)

 (** This module implements AC-KBO. *)
 module Ackbo: sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] tries to orient rules from [p] by using the simplification
  order AC-KBO.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)

  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
 end

(** {3 Module CSF} *)

 (** CSF Processor
 @author ?, Sarah Winkler
 @since  Fri Mar  2 20:16:08 CEST 2011 *)

 (** This module implements simple function symbol counting. *)
 module Csf: sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] tries to find a function symbol in [p] such that the
  number of occurrences on lhss is smaller than on rhss.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
 end


 (** {3 Module Matrix} *)
 
 (** Matrix Processor
 @author Harald Zankl
 @since  Mon Apr 27 09:40:16 CEST 2009 *)
 
 (** This module implements the matrix method. *)
 module Matrix : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] tries to orient rules from [p] by using matrix
  interpretations.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)

  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
 end

 (** {3 Module Poly} *)
 
 (** Poly Processor
 @author Harald Zankl
 @since  Tue Dec  8 16:01:29 CET 2009 *)
 
 (** This module implements polynomial interpretations. *)
 module Poly: sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] tries to orient rules from [p] by using polynomial
  interpretations.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)

  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
 end

 (** {3 Module Sccs} *)
 
 (** SCC Processor
 @author Martin Korp
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements the strongly connected component processor which
 splits a DP problem into a set of smaller DP problems by using the information
 provided by the dependency graph. *)
 module Sccs : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option
  (** [solve fs p] splits [p] into the corresponding set of DP problems. If
  the processor does not make progress, [None] is returned.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial DP problem of [p]. *)
  val get_ops : t -> Problem.t list
  (** [get_ops p] returns the computed DP problems. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problems. *)

  (** {4 Properties} *)

  val is_empty : t -> bool
  (** [is_empty p] checks if the resulting DP problems are empty, i.e., if
  the do not have any dependency pairs or an empty dependency graph. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : ?s:bool -> (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf ~s:s fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented by
  their names registered in the underlying signature. If the name of a function
  symbol or variable is not defined, a randomly generated name is printed. If
  the flag [s] is set to true, some additional status information are printed.
  Per default [s] is false. Note that as a side effect copies of the underlying
  signature are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module SemanticLabeling} *)
 
 (** Semantic Labeling Processor
 @author Harald Zankl
 @since  Mon Apr 27 09:40:16 CEST 2009 *)
 
 (** This module implements semantic and predictive labeling. *)
 module SemanticLabeling: sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] tries to orient rules from [p] by semantic labeling.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
 end

 (** {3 Module SizeChangeTermintion} *)
 
 (** Size-Change Termination Processor (using subterm relation)
 @author Christian Sternagel
 @since  Mon Sep  1 12:22:31 CEST 2009 *)
 
 (** This module implements the size-change termination processor
 (using the subterm relation) which directly shows finiteness of
 a DP problem. *)
 module SizeChangeTermination : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option
  (** [solve fs p] checks if the size-change principle is applicable to [p].
  If so the set of initial size-change graphs is given as evidence. Otherwise,
  the processor fails.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module SubtermCriterion} *)
 
 (** Subterm Criterion Processor
 @author Harald Zankl
 @since  Mon Apr 27 09:40:16 CEST 2009 *)
 
 (** This module implements the subterm criterion processor. *)
 module SubtermCriterion : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] tries to orient rules from [p] by using simple projection
  with subterm criterion.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Trivial} *)
 
 (** Trivial Processor
 @author Harald Zankl
 @since  Thu Dec 16 16:54:21 CET 2010 *)
 
 (** This module implements the trivial processor. *)
 module Trivial : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option 
  (** [solve fs p] tests if [p] is trivially terminating
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the remaining problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end
end

(** {2 Module Transformation} *)

(** Transformation Techniques
@author Martin Korp, Christian Sternagel, Harald Zankl
@since  Mon Sep  1 12:22:31 CEST 2009 *)

(** This module provides several techniques which can be used to transform
a TRS. In difference to termination and non-termination techniques, these
methods are not able to prove termination or non-termination of a TRS on
their own. *)
module Transformation : sig
 (*** MODULES *****************************************************************)
 (** {3 Module Cp} *)
 
 (** Complexity Transformation
 @author Martin Korp, Harald Zankl
 @since  Mon May 11 20:03:20 CEST 2009 *)
 
 (** This processor transforms a standard termination problem into an initial
 CP problem. *)
 module Cp : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option
  (** [solve f p] transforms [p] into an initial CP problem. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the constructed initial CP problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new CP problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Dpify} *)
 
 (** Dpify Transformation
 @author Christian Sternagel
 @since  Fri Sep 10 15:30:39 CEST 2010 *)
 
 (** This processor marks the root symbols of lhss and rhss as DP symbols.  *)
 module Dpify : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve f p] transforms [p] into a DP problem with marked roots. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the constructed initial CP problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new CP problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Dup} *)
 
 (** Duplication Transformation
 @author Harald Zankl
 @since  Mon Dec 13 13:41:15 CET 2010 *)
 
 (** This processor transforms standard problem into relative problem
 with duplicating rules in strict component.  *)
 module Dup : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve f p] transforms [p] into relative problem with duplicating
  rules in strict component. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the constructed relative problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new CP problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Linear} *)
 
 (** Linearization of TRSs
 @author Martin Korp
 @since  Mon May 11 20:03:20 CEST 2009 *)
 
 (** This processor transforms a given problem by linearizing all left-hand
 sides. *)
 module Linear : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve f p] modifies [p] by linearizing all left-hand sides. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the resulting problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module QuasiRootLabeling} *)
 
 (** Quasi Root-Labeling Transformation
 @author Christian Sternagel
 @since Fri Nov 13 16:51:45 CET 2009 *)
 
 (** This processor applies a special version of semantic labeling.*)
 module QuasiRootLabeling : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve f p] applies root-labeling to [p].
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the resulting problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Reflect} *)
 
 (** Reflection of TRSs
 @author Martin Korp
 @since  Mon May 11 20:03:20 CEST 2009 *)
 
 (** This processor reverses the arguments of the left- and right-hand sides
 of a given problem. *)
 module Reflect : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option
  (** [solve f p] modifies [p] by reflecting all left- and right-hand sides. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the resulting problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Reverse} *)
 
 (** Reversal of SRSs
 @author Martin Korp
 @since  Mon May 11 20:03:20 CEST 2009 *)
 
 (** This processor reverses the the left- and right-hand sides of a given
 problem if each left- and right-hand side is a string. *)
 module Reverse : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option
  (** [solve f p] modifies [p] by reversing all left- and right-hand sides. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the resulting problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module RootLabeling} *)
 
 (** Root-Labeling Transformation
 @author Christian Sternagel
 @since  Fri Jul 31 14:20:06 CEST 2009 *)
 
 (** This processor applies a special version of semantic labeling (where the
 carrier is fixed to the function symbols). *)
 module RootLabeling : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve f p] applies root-labeling to [p].
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the resulting problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Rt} *)
 
 (** Relative Termination Transformation
 @author Martin Korp
 @since  Mon May 11 20:03:20 CEST 2009 *)
 
 (** This processor transforms a given standard termination problem into a
 relative termination problem by duplicating the underlying TRS. *)
 module Rt : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option
  (** [solve f p] transforms [p] into a relative termination problem by
  duplicating the TRSs stored in [p].
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the resulting problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Split} *)
 
 (** Splitting Processor
 @author Martin Korp, Harald Zankl
 @since  Mon May 11 20:03:20 CEST 2009 *)
 
 (** This processor transforms a CP problem into a list of CP problems by
 relating each rewrite rule for which no complexity bound has been proven
 so far with all other rules. *)
 module Split : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val generate : string list -> Problem.t -> Problem.t list -> t option
  (** [generate f p ps] transforms [p] into the list of complexity problems
  [ps] if [ps] represents a valid splitting. *)
  val solve : string list -> Problem.t -> t option
  (** [solve f p] transforms [p] into a list of complexity problems. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial CP problem. *)
  val get_ops : t -> Problem.t list
  (** [get_ops p] returns the resulting CP problems. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new CP problems. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module St} *)
 
 (** Standard Termination Transformation
 @author Martin Korp
 @since  Mon May 11 20:03:20 CEST 2009 *)
 
 (** This processor transforms a given relative termination problem into a
 standard termination problem by taking the union of the the two involved
 TRSs. *)
 module St : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option
  (** [solve f p] transforms [p] into a standard termination problem by taking
  the union of the the two TRSs stored in [p].
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the resulting problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Star} *)
 
 (** Star Transformation
 @author Harald Zankl
 @since  Mon Dec 13 13:41:15 CET 2010 *)
 
 (** This processor transforms standard problem into relative problem
 (depending on context).  *)
 module Star : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t 
  (** [solve f p] transforms [p] into relative problem with duplicating
  rules in strict component. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the constructed relative problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new CP problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

module Udpac : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve fs p] transforms [p] into the corresponding DP problem
  where DPs rooted by AC symbols are unlabelled.
  If the given problem is not a standard termination problem, [None] is
  returned.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the input TRS. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the DP problem of [p]. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)

  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module TypeIntroduction} *)
 
 (** Type Introduction
 @author Christian Sternagel
 @since Thu Nov 26 23:05:41 CET 2009 *)
 
 (** This processors introduces types (also called sorts) for a TRS, if
 possible the type information is exploited to split the TRS into
 smaller ones, otherwise the processor fails. *)
 module TypeIntroduction : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve f p] tries to split the TRS into several smaller TRSs
  by using type information.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial TRS. *)
  val get_ops : t -> Problem.t list
  (** [get_ops p] returns the resulting TRSs. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Uncurry} *)
 
 (** Uncurrying
 @author Harald Zankl
 @since  Mon May 11 20:03:20 CEST 2009 *)
 
 (** This processor uncurries a given TRS if it is applicable. *)
 module Uncurry : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve f p] uncurries [p] if the underlying TRS is applicative. The
  processor returns [None] if [p] is not a standard termination problem or
  the underlying TRS cannot be uncurried.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the resulting problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Uncurryx} *)
 
 (** Generalized Uncurrying
 @author Christian Sternagel
 @since  Thu Dec  9 09:02:31 CET 2010 *)
 
 (** This processor uncurries a given TRS if it is applicable. *)
 module Uncurryx : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve f p] uncurries [p] if the underlying TRS is applicative. The
  processor returns [None] if [p] is not a standard termination problem or
  the underlying TRS cannot be uncurried.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the resulting problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end

 (** {3 Module Ur} *)
 
 (** Usable Rules
 @author Martin Korp, Harald Zankl
 @since  Mon May 11 20:03:20 CEST 2009 *)
 
 (** This processor computes the usable rules of a given DP problem. Note
 that per default the processor is not sound if the DP problem is duplicating.
 To ensure soundness you have to set the corresponding flags. *)
 module Ur : sig
  (*** TYPES ******************************************************************)
  type t

  (*** VALUES *****************************************************************)
  (** {4 Globals} *)

  val code : string
  (** [code] defines the shortcut that has to be used to refer to this
  processor within TTT2. *)
  val help : string * string list * (string * string) list
  (** [help] provides a detailed description of this processor and its flags. *)

  (** {4 Processor} *)

  val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
  (** [solve f p] removes all rules of the given DP problem which are not
  usable. The processor returns [None] if [p] is not a DP problem.
  @raise Failure if [fs] consists of some unknown flag. *)

  (** {4 Destructors} *)

  val get_ip : t -> Problem.t
  (** [get_ip p] returns the initial DP problem. *)
  val get_op : t -> Problem.t
  (** [get_op p] returns the resulting DP problem. *)

  (** {4 Complexity Bounds} *)

  val complexity : Util.Complexity.t -> t -> Util.Complexity.t
  (** [complexity c p] returns information regarding the complexity of the
  new termination problem. *)

  (** {4 Compare Functions} *)

  val equal : t -> t -> bool
  (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

  (** {4 Printers} *)
  
  val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintf fs fmt p] prints [p] an the corresponding proofs using the
  [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature
  are changed too. This function is not tail-recursive. *)
  val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
   -> Format.formatter -> t -> unit Rewritingx.Monad.t
  (** [fprintfx fs fmt p] prints [p] an the corresponding proofs as XML using
  the [OCaml] module [Format]. Function symbols and variables are represented
  by their names registered in the underlying signature. If the name of a
  function symbol or variable is not defined, a randomly generated name is
  printed. Note that as a side effect copies of the underlying signature are
  changed too. This function is not tail-recursive. *)
 end
end

(** {2 Module Confluence} *)

(** Confluence Techniques
@author Martin Korp, Harald Zankl
@since  Sun Dec 26 17:51:21 CET 2010 *)

(** This module provides several techniques which can be used to (dis)prove
confluence of a TRS. *)
module Confluence : sig
 (*** MODULES *****************************************************************)
 (** {3 Module Nonconfluence} *)
 
 (** Nonconfluence Processor
 @author Martin Korp 
 @since  Sun Dec 26 17:52:34 CET 2010 *)
 
(** This module implements a non-confluence check based on tree automata. *)
module Nonconfluence : sig
 (*** TYPES *****************************************************************)
 type t

 (*** VALUES ****************************************************************)
 (** {5 Globals} *)

 val code : string
 (** [code] the name of the processor. *)
 val help : string * string list * (string * string) list
 (** [help] description of the flags to this processor. *)

 (** {5 Processor} *)

 val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
 (** [solve fs p] checks if there is a critical pair which is not joinable.
 If that is the case, the corresponding critical pair is returned. Otherwise
 [None] is reported.
 @raise Failure if [fs] consists of some unknown flag. *)

 (** {5 Destructors} *)

 val get_ip : t -> Problem.t
 (** [get_ip p] returns the initial DP problem. *)
 val get_op : t -> Problem.t
 (** [get_op p] returns the resulting DP problem. *)

 (** {5 Complexity Bounds} *)

 val complexity : Util.Complexity.t -> t -> Util.Complexity.t
 (** [complexity c p] returns information regarding the complexity of the
 new termination problem. *)

 (** {5 Compare Functions} *)

 val equal : t -> t -> bool
 (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

 (** {5 Printers} *)
 
 val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
  -> Format.formatter -> t -> unit Rewritingx.Monad.t
 (** [fprintf fs fmt p] prints proof [p]. All symbols need to be
 registered in the signature. This function is not tail-recursive. *)
 val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
  -> Format.formatter -> t -> unit Rewritingx.Monad.t
 (** [fprintfx fs fmt p] is similar to [fprintf] but prints xml *)
end

 (** {3 Module RuleLabeling} *)
 
 (** Rule Labeling Processor
 @author Harald Zankl
 @since  Sun Dec 26 17:58:39 CET 2010 *)
 
(** This module implements the rule labeling heuristic. *)
module RuleLabeling: sig
 (*** TYPES *****************************************************************)
 type t

 (*** VALUES ****************************************************************)
 (** {5 Globals} *)

 val code : string
 (** [code] the name of the processor. *)
 val help : string * string list * (string * string) list
 (** [help] description of the flags to this processor. *)

 (** {5 Processor} *)

 val solve : string list -> Problem.t -> t option Rewritingx.Monad.t
 (** [solve fs p] returns [Some p] if the (minimal) critical peaks can
 be joined decreasing wrt the rule labeling and [None] otherwise.
 @raise Failure if [fs] consists of some unknown flag. *)

 (** {5 Destructors} *)

 val get_ip : t -> Problem.t
 (** [get_ip p] returns the initial DP problem. *)
 val get_op : t -> Problem.t
 (** [get_op p] returns the resulting DP problem. *)

 (** {5 Complexity Bounds} *)

 val complexity : Util.Complexity.t -> t -> Util.Complexity.t
 (** [complexity c p] returns information regarding the complexity of the
 new termination problem. *)

 (** {5 Compare Functions} *)

 val equal : t -> t -> bool
 (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

 (** {5 Printers} *)
 
 val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
  -> Format.formatter -> t -> unit Rewritingx.Monad.t
 (** [fprintf fs fmt p] prints proof [p]. All symbols need to be
 registered in the signature. This function is not tail-recursive. *)
 val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
  -> Format.formatter -> t -> unit Rewritingx.Monad.t
 (** [fprintfx fs fmt p] is similar to [fprintf] but prints xml *)
end

 (** {3 Module Shift} *)
 
 (** Shift Processor
 @author Harald Zankl
 @since  Sun Dec 26 17:58:39 CET 2010 *)
 
(** This module shifts strict rules to weak rules. *)
module Shift: sig
 (*** TYPES *****************************************************************)
 type t

 (*** VALUES ****************************************************************)
 (** {5 Globals} *)

 val code : string
 (** [code] the name of the processor. *)
 val help : string * string list * (string * string) list
 (** [help] description of the flags to this processor. *)

 (** {5 Processor} *)

 val solve : string list -> Problem.t -> t option
 (** [solve fs p] returns [Some p] where the strict rules are shifted to
 the weak rules if the lhs of the strict rules are unique.
 @raise Failure if [fs] consists of some unknown flag. *)

 (** {5 Destructors} *)

 val get_ip : t -> Problem.t
 (** [get_ip p] returns the initial DP problem. *)
 val get_op : t -> Problem.t
 (** [get_op p] returns the resulting DP problem. *)

 (** {5 Complexity Bounds} *)

 val complexity : Util.Complexity.t -> t -> Util.Complexity.t
 (** [complexity c p] returns information regarding the complexity of the
 new termination problem. *)

 (** {5 Compare Functions} *)

 val equal : t -> t -> bool
 (** [equal p q] checks if the proof [p] is equivalent to [q]. *)

 (** {5 Printers} *)
 
 val fprintf : (Format.formatter -> unit Rewritingx.Monad.t) list
  -> Format.formatter -> t -> unit Rewritingx.Monad.t
 (** [fprintf fs fmt p] prints proof [p]. All symbols need to be
 registered in the signature. This function is not tail-recursive. *)
 val fprintfx : (Format.formatter -> unit Rewritingx.Monad.t) list
  -> Format.formatter -> t -> unit Rewritingx.Monad.t
 (** [fprintfx fs fmt p] is similar to [fprintf] but prints xml *)
end
end
